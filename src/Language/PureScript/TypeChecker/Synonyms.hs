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
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Language.PureScript.Environment (Environment(..), TypeKind)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), SourceSpan, errorMessage')
import Language.PureScript.Names (ProperName, ProperNameType(..), Qualified)
import Language.PureScript.TypeChecker.Monad (getEnv, TypeCheckM)
import Language.PureScript.Types (SourceType, Type(..), completeBinderList, everywhereOnTypesTopDownM, getAnnForType, replaceAllTypeVars)

-- | Type synonym information (arguments with kinds, aliased type), indexed by name
type SynonymMap = HM.HashMap (Qualified (ProperName 'TypeName)) ([(Text, Maybe SourceType)], SourceType)

type KindMap = HM.HashMap (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)

replaceAllTypeSynonyms'
  :: SynonymMap
  -> KindMap
  -> SourceType
  -> Either MultipleErrors SourceType
replaceAllTypeSynonyms' syns kinds = everywhereOnTypesTopDownM try
  where
  try :: SourceType -> Either MultipleErrors SourceType
  try t = fromMaybe t <$> go (fst $ getAnnForType t) 0 [] [] t

  go :: SourceSpan -> Int -> [SourceType] -> [SourceType] -> SourceType -> Either MultipleErrors (Maybe SourceType)
  go ss c kargs args (TypeConstructor _ ctor)
    | Just (synArgs, body) <- HM.lookup ctor syns
    , c == length synArgs
    , kindArgs <- lookupKindArgs ctor
    , length kargs == length kindArgs
    = let repl = replaceAllTypeVars (zip (map fst synArgs) args <> zip kindArgs kargs) body
      in Just <$> try repl
    | Just (synArgs, _) <- HM.lookup ctor syns
    , length synArgs > c
    = throwError . errorMessage' ss $ PartiallyAppliedSynonym ctor
  go ss c kargs args (TypeApp _ f arg) = go ss (c + 1) kargs (arg : args) f
  go ss c kargs args (KindApp _ f arg) = go ss c (arg : kargs) args f
  go _ _ _ _ _ = return Nothing

  lookupKindArgs :: Qualified (ProperName 'TypeName) -> [Text]
  lookupKindArgs ctor = fromMaybe [] $ fmap (fmap (fst . snd) . fst) . completeBinderList . fst =<< HM.lookup ctor kinds

-- | Replace fully applied type synonyms
replaceAllTypeSynonyms :: SourceType -> TypeCheckM SourceType
replaceAllTypeSynonyms d = do
  env <- getEnv
  either throwError return $ replaceAllTypeSynonyms' (typeSynonyms env) (types env) d
