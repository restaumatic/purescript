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
  , combineFlags, completeBinderList, constraintNodeFlags, forAllNodeFlags
  , getAnnForType, hasFlag, overConstraintArgsAll, replaceAllTypeVars
  , setFlag, skolemNodeFlags, tfSynonymsFree, typeFlags
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

  -- Mark a single node as synonym-free (no recursion)
  markSF :: SourceType -> SourceType
  markSF (TUnknown_ f a b) = TUnknown_ (sf f) a b
  markSF (TypeVar_ f a b) = TypeVar_ (sf f) a b
  markSF (TypeLevelString_ f a b) = TypeLevelString_ (sf f) a b
  markSF (TypeLevelInt_ f a b) = TypeLevelInt_ (sf f) a b
  markSF (TypeWildcard_ f a b) = TypeWildcard_ (sf f) a b
  markSF (TypeConstructor_ f a b) = TypeConstructor_ (sf f) a b
  markSF (TypeOp_ f a b) = TypeOp_ (sf f) a b
  markSF (TypeApp_ f a t1 t2) = TypeApp_ (sf f) a t1 t2
  markSF (KindApp_ f a t1 t2) = KindApp_ (sf f) a t1 t2
  markSF (ForAll_ f a v i k t s) = ForAll_ (sf f) a v i k t s
  markSF (ConstrainedType_ f a c t) = ConstrainedType_ (sf f) a c t
  markSF (Skolem_ f a n k i s) = Skolem_ (sf f) a n k i s
  markSF (REmpty_ f a) = REmpty_ (sf f) a
  markSF (RCons_ f a l t r) = RCons_ (sf f) a l t r
  markSF (KindedType_ f a t k) = KindedType_ (sf f) a t k
  markSF (BinaryNoParensType_ f a t1 t2 t3) = BinaryNoParensType_ (sf f) a t1 t2 t3
  markSF (ParensInType_ f a t) = ParensInType_ (sf f) a t

  -- Main walk: try synonym expansion at potential application sites,
  -- then recurse into children. Sets tfSynonymsFree on all output nodes.
  walk :: SourceType -> Either MultipleErrors SourceType
  walk t | hasFlag tfSynonymsFree (typeFlags t) = Right t
  walk t@(TypeApp_ _ _ _ _) = trySyn t >>= walkChildren
  walk t@(KindApp_ _ _ _ _) = trySyn t >>= walkChildren
  walk t@(TypeConstructor_ _ _ _) = trySyn t >>= \t' -> case t' of
    TypeConstructor_ _ _ _ -> Right (markSF t')  -- leaf
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

  -- Walk children and reconstruct with recomputed structural flags + tfSynonymsFree.
  -- Uses raw constructors to set flags in a single allocation.
  walkChildren :: SourceType -> Either MultipleErrors SourceType
  walkChildren (TypeApp_ _ ann t1 t2) = do
    t1' <- walk t1; t2' <- walk t2
    return $! TypeApp_ (sf (typeFlags t1' `combineFlags` typeFlags t2')) ann t1' t2'
  walkChildren (KindApp_ _ ann t1 t2) = do
    t1' <- walk t1; t2' <- walk t2
    return $! KindApp_ (sf (typeFlags t1' `combineFlags` typeFlags t2')) ann t1' t2'
  walkChildren (ForAll_ _ ann vis ident mbK ty sco) = do
    mbK' <- traverse walk mbK; ty' <- walk ty
    return $! ForAll_ (sf (forAllNodeFlags mbK' ty' sco)) ann vis ident mbK' ty' sco
  walkChildren (ConstrainedType_ _ ann c ty) = do
    c' <- overConstraintArgsAll (mapM walk) c; ty' <- walk ty
    return $! ConstrainedType_ (sf (constraintNodeFlags c' ty')) ann c' ty'
  walkChildren (Skolem_ _ ann name mbK i sc) = do
    mbK' <- traverse walk mbK
    return $! Skolem_ (sf (skolemNodeFlags mbK')) ann name mbK' i sc
  walkChildren (RCons_ _ ann name ty rest) = do
    ty' <- walk ty; rest' <- walk rest
    return $! RCons_ (sf (typeFlags ty' `combineFlags` typeFlags rest')) ann name ty' rest'
  walkChildren (KindedType_ _ ann ty k) = do
    ty' <- walk ty; k' <- walk k
    return $! KindedType_ (sf (typeFlags ty' `combineFlags` typeFlags k')) ann ty' k'
  walkChildren (BinaryNoParensType_ _ ann t1 t2 t3) = do
    t1' <- walk t1; t2' <- walk t2; t3' <- walk t3
    return $! BinaryNoParensType_ (sf (typeFlags t1' `combineFlags` typeFlags t2' `combineFlags` typeFlags t3')) ann t1' t2' t3'
  walkChildren (ParensInType_ _ ann t) = do
    t' <- walk t
    return $! ParensInType_ (sf (typeFlags t')) ann t'
  walkChildren other = return $! markSF other

  lookupKindArgs :: Qualified (ProperName 'TypeName) -> [Text]
  lookupKindArgs ctor = fromMaybe [] $ fmap (fmap (fst . snd) . fst) . completeBinderList . fst =<< M.lookup ctor kinds

-- | Replace fully applied type synonyms.
-- Short-circuits if the type is already marked as synonym-free.
replaceAllTypeSynonyms :: SourceType -> TypeCheckM SourceType
replaceAllTypeSynonyms d
  | hasFlag tfSynonymsFree (typeFlags d) = return d
  | otherwise = do
      env <- getEnv
      either throwError return $ replaceAllTypeSynonyms' (typeSynonyms env) (types env) d
