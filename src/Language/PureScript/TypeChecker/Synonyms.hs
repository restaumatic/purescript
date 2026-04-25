{-# LANGUAGE GADTs #-}

-- |
-- Functions for replacing fully applied type synonyms
--
module Language.PureScript.TypeChecker.Synonyms
  ( SynonymMap
  , KindMap
  , replaceAllTypeSynonyms
  ) where

import Prelude

import Control.Exception (assert)
import Control.Monad.Error.Class (MonadError(..))
import Data.Maybe (fromMaybe)
import Data.Map qualified as M
import Data.Text (Text)
import Language.PureScript.Environment (Environment(..), TypeKind)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), SourceSpan, errorMessage')
import Language.PureScript.Names (ProperName, ProperNameType(..), Qualified)
import Language.PureScript.TypeChecker.Monad (getEnv, TypeCheckM)
import Language.PureScript.Types
  ( SourceType, Type(..), TypeFlags
  , completeBinderList, everythingOnTypes
  , getAnnForType, hasFlag, modifyFlags, overConstraintArgsAll
  , replaceAllTypeVars, setFlag, tfSynonymsFree, typeFlags
  )

-- | Type synonym information (arguments with kinds, aliased type), indexed by name
type SynonymMap = M.Map (Qualified (ProperName 'TypeName)) ([(Text, Maybe SourceType)], SourceType)

type KindMap = M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)

-- | Replace fully applied type synonyms and mark every output node
-- with 'tfSynonymsFree'. Uses a custom traversal that:
--
-- 1. Short-circuits on subtrees already marked synonym-free
-- 2. Only tries synonym expansion on potential application heads
-- 3. Sets 'tfSynonymsFree' on every output node in a single pass
replaceAllTypeSynonyms'
  :: SynonymMap
  -> KindMap
  -> SourceType
  -> Either MultipleErrors SourceType
replaceAllTypeSynonyms' syns kinds
  | M.null syns = Right . markSF
  | otherwise = walk
  where
  sf :: TypeFlags -> TypeFlags
  sf = setFlag tfSynonymsFree

  -- Mark a single node as synonym-free (no recursion). 'modifyFlags' is
  -- INLINEd, so GHC fuses construction (via pattern synonym) + flag mutation
  -- into a single allocation via case-of-known-constructor.
  markSF :: SourceType -> SourceType
  markSF = modifyFlags sf

  -- Main walk: try synonym expansion at potential application sites,
  -- then recurse into children. Sets tfSynonymsFree on all output nodes.
  walk :: SourceType -> Either MultipleErrors SourceType
  walk t | hasFlag tfSynonymsFree (typeFlags t) = Right t
  walk t@(TypeApp _ _ _) = trySyn t >>= walkChildren
  walk t@(KindApp _ _ _) = trySyn t >>= walkChildren
  walk t@(TypeConstructor _ _) = trySyn t >>= \t' -> case t' of
    TypeConstructor _ _ -> Right (markSF t')  -- leaf
    _ -> walkChildren t'  -- synonym expanded to non-leaf
  walk t = walkChildren t

  -- Try to expand a synonym application at the root.
  -- Uses the original 'go' logic to peel TypeApp/KindApp and find the constructor.
  trySyn :: SourceType -> Either MultipleErrors SourceType
  trySyn t = fromMaybe t <$> go (fst $ getAnnForType t) 0 [] [] t

  go :: SourceSpan -> Int -> [SourceType] -> [SourceType] -> SourceType -> Either MultipleErrors (Maybe SourceType)
  go ss c kargs args (TypeConstructor _ ctor)
    | Just (synArgs, body) <- M.lookup ctor syns
    , c == length synArgs
    , kindArgs <- lookupKindArgs ctor
    , length kargs == length kindArgs
    = let repl = replaceAllTypeVars (zip (map fst synArgs) args <> zip kindArgs kargs) body
      in Just <$> trySyn repl
    | Just (synArgs, _) <- M.lookup ctor syns
    , length synArgs > c
    = throwError . errorMessage' ss $ PartiallyAppliedSynonym ctor
  go ss c kargs args (TypeApp _ f arg) = go ss (c + 1) kargs (arg : args) f
  go ss c kargs args (KindApp _ f arg) = go ss c (arg : kargs) args f
  go _ _ _ _ _ = return Nothing

  -- Walk children and reconstruct via pattern synonyms (which compute flags)
  -- + markSF (which sets the synonym-free bit). The pattern synonym builder
  -- and modifyFlags are both INLINE; GHC fuses them via case-of-known-
  -- constructor into a single allocation per node.
  walkChildren :: SourceType -> Either MultipleErrors SourceType
  walkChildren (TypeApp ann t1 t2) = do
    t1' <- walk t1; t2' <- walk t2
    return $! markSF (TypeApp ann t1' t2')
  walkChildren (KindApp ann t1 t2) = do
    t1' <- walk t1; t2' <- walk t2
    return $! markSF (KindApp ann t1' t2')
  walkChildren (ForAll ann vis ident mbK ty sco) = do
    mbK' <- traverse walk mbK; ty' <- walk ty
    return $! markSF (ForAll ann vis ident mbK' ty' sco)
  walkChildren (ConstrainedType ann c ty) = do
    c' <- overConstraintArgsAll (mapM walk) c; ty' <- walk ty
    return $! markSF (ConstrainedType ann c' ty')
  walkChildren (Skolem ann name mbK i sc) = do
    mbK' <- traverse walk mbK
    return $! markSF (Skolem ann name mbK' i sc)
  walkChildren (RCons ann name ty rest) = do
    ty' <- walk ty; rest' <- walk rest
    return $! markSF (RCons ann name ty' rest')
  walkChildren (KindedType ann ty k) = do
    ty' <- walk ty; k' <- walk k
    return $! markSF (KindedType ann ty' k')
  walkChildren (BinaryNoParensType ann t1 t2 t3) = do
    t1' <- walk t1; t2' <- walk t2; t3' <- walk t3
    return $! markSF (BinaryNoParensType ann t1' t2' t3')
  walkChildren (ParensInType ann t) = do
    t' <- walk t
    return $! markSF (ParensInType ann t')
  walkChildren other = return $! markSF other

  lookupKindArgs :: Qualified (ProperName 'TypeName) -> [Text]
  lookupKindArgs ctor = fromMaybe [] $ fmap (fmap (fst . snd) . fst) . completeBinderList . fst =<< M.lookup ctor kinds

-- | Replace fully applied type synonyms.
-- Short-circuits if the type is already marked as synonym-free.
replaceAllTypeSynonyms :: SourceType -> TypeCheckM SourceType
replaceAllTypeSynonyms d
  | hasFlag tfSynonymsFree (typeFlags d) = do
      env <- getEnv
      -- Sanity check in debug builds: the flag says this type is synonym-free,
      -- so scanning should confirm no TypeConstructor in it refers to a synonym.
      -- 'assert' is compiled away with -O, so this is a no-op in production.
      return $! assert (not (containsTypeSynonyms (typeSynonyms env) d)) d
  | otherwise = do
      env <- getEnv
      either throwError return $ replaceAllTypeSynonyms' (typeSynonyms env) (types env) d

-- | Scan a type for TypeConstructors that are type synonyms.
-- Used as a correctness check for the 'tfSynonymsFree' flag.
containsTypeSynonyms :: SynonymMap -> Type a -> Bool
containsTypeSynonyms syns = everythingOnTypes (||) isSyn
  where
    isSyn (TypeConstructor _ ctor) = M.member ctor syns
    isSyn _ = False
