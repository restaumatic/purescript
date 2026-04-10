{-# LANGUAGE GADTs #-}

module Language.PureScript.Make.Rules
  ( makeRules
  , MakeError(..)
  , liftMake
  ) where

import Prelude

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Class (tell)
import Control.Monad (foldM)
import Data.IORef (IORef, atomicModifyIORef')
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

import Rock qualified

import Language.PureScript.AST (Module(..), getModuleName, getModuleSourceSpan)
import Language.PureScript.AST.SourcePos (spanName)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Environment (initEnvironment)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Externs (ExternsFile, applyExternsFileToEnvironment)
import Language.PureScript.Make.Monad (Make, runMake)
import Language.PureScript.Make.Query (Query(..))
import Language.PureScript.ModuleDependencies (DependencyDepth(..), moduleSignature, sortModules)
import Language.PureScript.Names (ModuleName, runModuleName)
import Language.PureScript.Options (Options)
import Language.PureScript.Sugar (Env, externsEnv, primEnv)

import Control.Monad.Writer.Strict (runWriterT)

-- | Exception wrapper for compilation errors, used to propagate errors from
-- the 'Make' monad through rock's IO-based 'Task'.
newtype MakeError = MakeError MultipleErrors
  deriving (Show)

instance Exception MakeError

-- | Run a 'Make' action inside rock's 'Task' monad.
-- Errors become IO exceptions; warnings are accumulated in the IORef.
liftMake :: Options -> IORef MultipleErrors -> Make a -> Rock.Task Query a
liftMake opts warningsRef action = liftIO $ do
  (result, warnings) <- runMake opts action
  atomicModifyIORef' warningsRef (\w -> (w <> warnings, ()))
  case result of
    Left errs -> throwIO (MakeError errs)
    Right a   -> pure a

-- | The type of a single-module compilation function.
-- This is passed as a parameter to avoid circular module imports
-- (the implementation lives in Language.PureScript.Make).
type CompileFn = Env -> [ExternsFile] -> Module -> Make ExternsFile

-- | Define the rock rules for the incremental compilation pipeline.
--
-- The rules close over:
-- * @modules@: map from module name to pre-parsed partial result
-- * @opts@: compiler options
-- * @warningsRef@: accumulator for compilation warnings
-- * @compileFn@: the per-module compilation function (desugar + typecheck + codegen)
makeRules
  :: M.Map ModuleName (CST.PartialResult Module)
  -> Options
  -> IORef MultipleErrors
  -> CompileFn
  -> Rock.Rules Query
makeRules modules opts warningsRef compileFn = \case

  InputModule mn ->
    case M.lookup mn modules of
      Just pr -> pure (CST.resPartial pr)
      Nothing -> liftIO . throwIO . MakeError $ internalError
        ("makeRules: InputModule: module not found: " <> show (runModuleName mn))

  SortedModules -> do
    -- Fetch all input modules to establish rock dependency tracking
    let allNames = M.keys modules
    _ <- traverse (\mn -> Rock.fetch (InputModule mn)) allNames
    -- Use existing sortModules (may throw on circular deps)
    liftMake opts warningsRef $ do
      let prs = M.elems modules
      (sorted, _graph) <- sortModules Transitive (moduleSignature . CST.resPartial) prs
      pure $ map (getModuleName . CST.resPartial) sorted

  ModuleGraph -> do
    -- Fetch all input modules to establish rock dependency tracking
    let allNames = M.keys modules
    _ <- traverse (\mn -> Rock.fetch (InputModule mn)) allNames
    -- Use existing sortModules to build the graph
    liftMake opts warningsRef $ do
      let prs = M.elems modules
      (_sorted, graph) <- sortModules Transitive (moduleSignature . CST.resPartial) prs
      pure $ M.fromList graph

  ModuleSugarEnv mn -> do
    graph <- Rock.fetch ModuleGraph
    sorted <- Rock.fetch SortedModules
    let deps = fromMaybe [] $ M.lookup mn graph
    -- Sort deps in topological order (from SortedModules).
    -- This is critical because externsEnv resolves each module's imports
    -- against the accumulated Env, so dependencies must be processed
    -- before their dependents.
    let depsSet = S.fromList deps
        sortedDeps = filter (`S.member` depsSet) sorted
    depExterns <- traverse (\dep -> Rock.fetch (CompileModule dep)) sortedDeps
    -- Build sugar Env from dependency externs
    liftMake opts warningsRef $
      fmap fst . runWriterT $ foldM externsEnv primEnv depExterns

  ModuleTypeEnv mn -> do
    graph <- Rock.fetch ModuleGraph
    let deps = fromMaybe [] $ M.lookup mn graph
    depExterns <- traverse (\dep -> Rock.fetch (CompileModule dep)) deps
    -- Build typechecker Environment (pure computation)
    pure $ foldl' (flip applyExternsFileToEnvironment) initEnvironment depExterns

  CompileModule mn -> do
    -- Establish dependency on the input module (for rock tracking)
    _inputModule <- Rock.fetch (InputModule mn)
    -- Fetch the sugar environment from dependencies
    sugarEnv <- Rock.fetch (ModuleSugarEnv mn)
    -- Fetch dependency externs (in topological order)
    graph <- Rock.fetch ModuleGraph
    sorted <- Rock.fetch SortedModules
    let deps = fromMaybe [] $ M.lookup mn graph
        depsSet = S.fromList deps
        sortedDeps = filter (`S.member` depsSet) sorted
    depExterns <- traverse (\dep -> Rock.fetch (CompileModule dep)) sortedDeps

    -- Get the full parse result from the closed-over map
    let pr = fromMaybe (internalError $ "makeRules: CompileModule: module not found: " <> show (runModuleName mn))
               (M.lookup mn modules)
        fp = spanName . getModuleSourceSpan . CST.resPartial $ pr
        (pwarnings, mres) = CST.resFull pr

    -- Run compilation in the Make monad
    liftMake opts warningsRef $ do
      -- Emit parser warnings
      tell $ CST.toMultipleWarnings fp pwarnings
      -- Unwrap parse result (throws on parse error)
      m <- CST.unwrapParserError fp mres
      -- Run the full compilation pipeline
      compileFn sugarEnv depExterns m
