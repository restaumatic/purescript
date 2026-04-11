{-# LANGUAGE GADTs #-}

module Language.PureScript.Make.Rules
  ( makeRules
  , MakeError(..)
  , liftMake
  , CacheStatus
  ) where

import Prelude

import Control.Exception (Exception, throwIO)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Class (tell)
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Time.Clock (UTCTime)

import Rock qualified

import Language.PureScript.AST (Module(..), getModuleName, getModuleSourceSpan)
import Language.PureScript.AST.SourcePos (spanName)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Environment (initEnvironment)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Externs (ExternsFile, applyExternsFileToEnvironment)
import Language.PureScript.Make.Actions (MakeActions(..), ProgressMessage(..))
import Language.PureScript.Make.ExternsDiff (ExternsDiff, checkDiffs, diffExterns, emptyDiff)
import Language.PureScript.Make.Monad (Make, runMake)
import Language.PureScript.Make.Query (Query(..))
import Language.PureScript.ModuleDependencies (DependencyDepth(..), moduleSignature, sortModules)
import Language.PureScript.Names (ModuleName, runModuleName)
import Language.PureScript.Options (Options)
import Language.PureScript.Sugar (Env, externsEnv)

import Control.Monad.Writer.Strict (runWriterT)

-- | Exception wrapper for compilation errors, used to propagate errors from
-- the 'Make' monad through rock's IO-based 'Task'.
newtype MakeError = MakeError MultipleErrors
  deriving (Show)

instance Exception MakeError

-- | Run a 'Make' action inside rock's 'Task' monad.
liftMake :: Options -> IORef MultipleErrors -> Make a -> Rock.Task Query a
liftMake opts warningsRef action = liftIO $ do
  (result, warnings) <- runMake opts action
  atomicModifyIORef' warningsRef (\w -> (w <> warnings, ()))
  case result of
    Left errs -> throwIO (MakeError errs)
    Right a   -> pure a

-- | The type of a single-module compilation function.
type CompileFn = Env -> [ExternsFile] -> Module -> Make ExternsFile

-- | Pre-computed cache status for a module.
-- Nothing = needs rebuild, Just (externs, timestamp) = source unchanged, cached externs available.
type CacheStatus = M.Map ModuleName (Maybe (ExternsFile, UTCTime))

-- | Define the rock rules for the incremental compilation pipeline.
makeRules
  :: M.Map ModuleName (CST.PartialResult Module)
  -> Options
  -> MakeActions Make
  -> IORef MultipleErrors
  -> CompileFn
  -> CacheStatus
  -> M.Map ModuleName ExternsFile
  -> IORef (M.Map ModuleName ExternsDiff)
  -> IORef Env
     -- ^ Shared cumulative sugar Env (like the old bpEnv MVar).
     -- Built up incrementally as modules compile, avoiding redundant
     -- externsEnv calls.
  -> Rock.Rules Query
makeRules modules opts actions warningsRef compileFn cacheStatus allCachedExterns diffsRef sharedEnvRef = \case

  InputModule mn ->
    case M.lookup mn modules of
      Just pr -> pure (CST.resPartial pr)
      Nothing -> liftIO . throwIO . MakeError $ internalError
        ("makeRules: InputModule: module not found: " <> show (runModuleName mn))

  SortedModules -> do
    let allNames = M.keys modules
    _ <- traverse (\mn -> Rock.fetch (InputModule mn)) allNames
    liftMake opts warningsRef $ do
      let prs = M.elems modules
      (sorted, _graph) <- sortModules Transitive (moduleSignature . CST.resPartial) prs
      pure $ map (getModuleName . CST.resPartial) sorted

  ModuleGraph -> do
    let allNames = M.keys modules
    _ <- traverse (\mn -> Rock.fetch (InputModule mn)) allNames
    liftMake opts warningsRef $ do
      let prs = M.elems modules
      (_sorted, graph) <- sortModules Transitive (moduleSignature . CST.resPartial) prs
      pure $ M.fromList graph

  -- ModuleSugarEnv and ModuleTypeEnv are no longer used directly;
  -- their logic is inlined into CompileModule for performance.
  -- Kept for API compatibility.
  ModuleSugarEnv _mn -> liftIO $ readIORef sharedEnvRef
  ModuleTypeEnv mn -> do
    graph <- Rock.fetch ModuleGraph
    let deps = fromMaybe [] $ M.lookup mn graph
    depExterns <- traverse (\dep -> Rock.fetch (CompileModule dep)) deps
    pure $ foldl' (flip applyExternsFileToEnvironment) initEnvironment depExterns

  CompileModule mn -> do
    _inputModule <- Rock.fetch (InputModule mn)
    graph <- Rock.fetch ModuleGraph
    sorted <- Rock.fetch SortedModules
    let deps = fromMaybe [] $ M.lookup mn graph
        depsSet = S.fromList deps
        sortedDeps = filter (`S.member` depsSet) sorted
    depExterns <- traverse (\dep -> Rock.fetch (CompileModule dep)) sortedDeps

    let cachedInfo = case M.lookup mn cacheStatus of
          Just (Just (exts, ts)) -> Just (exts, ts)
          _                      -> Nothing

    case cachedInfo of
      Just (cached, myTimestamp) -> do
        let depTimestamps = map (\dep -> case M.lookup dep cacheStatus of
              Just (Just (_, ts)) -> Just ts
              _                   -> Nothing) sortedDeps
            depsNewerThanMe = any (\mts -> maybe False (> myTimestamp) mts) depTimestamps

        if depsNewerThanMe then do
          exts <- doCompile mn sortedDeps depExterns
          recordDiff mn exts sortedDeps
          pure exts
        else do
          diffs <- liftIO $ readIORef diffsRef
          let depDiffs = map (\dep -> fromMaybe (emptyDiff dep) (M.lookup dep diffs)) sortedDeps
              pr = fromMaybe (internalError "makeRules: missing module")
                     (M.lookup mn modules)
              fullModule = case snd (CST.resFull pr) of
                Right m  -> m
                Left _   -> CST.resPartial pr
              needsRebuild = checkDiffs fullModule depDiffs

          if needsRebuild then do
            exts <- doCompile mn sortedDeps depExterns
            let diff = diffExterns exts cached depDiffs
            liftIO $ atomicModifyIORef' diffsRef (\d -> (M.insert mn diff d, ()))
            pure exts
          else do
            -- Skip: update shared env with our deps and report
            updateSharedEnv sortedDeps depExterns
            liftMake opts warningsRef $
              progress actions $ SkippingModule mn Nothing
            liftIO $ atomicModifyIORef' diffsRef (\d -> (M.insert mn (emptyDiff mn) d, ()))
            pure cached

      Nothing -> do
        exts <- doCompile mn sortedDeps depExterns
        recordDiff mn exts sortedDeps
        pure exts

  where
    -- | Compile a module. Builds the sugar Env incrementally from the shared
    -- cumulative env (only processing deps not yet in the env), then runs
    -- the full compilation — all in a single liftMake call.
    doCompile :: ModuleName -> [ModuleName] -> [ExternsFile] -> Rock.Task Query ExternsFile
    doCompile mn sortedDeps depExterns = do
      -- Read current shared env snapshot
      currentEnv <- liftIO $ readIORef sharedEnvRef

      let pr = fromMaybe (internalError $ "makeRules: CompileModule: module not found: " <> show (runModuleName mn))
                 (M.lookup mn modules)
          fp = spanName . getModuleSourceSpan . CST.resPartial $ pr
          (pwarnings, mres) = CST.resFull pr
          -- Only process deps not already in the shared env
          missingExterns = [ exts
                           | (dep, exts) <- zip sortedDeps depExterns
                           , not (M.member dep currentEnv)
                           ]

      -- Single liftMake call: extend env + compile
      liftMake opts warningsRef $ do
        -- Extend env with missing deps only
        sugarEnv <- fmap fst . runWriterT $ foldM externsEnv currentEnv missingExterns
        -- Update shared env for subsequent modules
        liftIO $ atomicModifyIORef' sharedEnvRef (\_ -> (sugarEnv, ()))
        -- Emit parser warnings and compile
        tell $ CST.toMultipleWarnings fp pwarnings
        m <- CST.unwrapParserError fp mres
        compileFn sugarEnv depExterns m

    -- | Update the shared env with deps (used when skipping compilation)
    updateSharedEnv :: [ModuleName] -> [ExternsFile] -> Rock.Task Query ()
    updateSharedEnv sortedDeps depExterns = do
      currentEnv <- liftIO $ readIORef sharedEnvRef
      let missingExterns = [ exts
                           | (dep, exts) <- zip sortedDeps depExterns
                           , not (M.member dep currentEnv)
                           ]
      if null missingExterns then pure ()
      else do
        newEnv <- liftMake opts warningsRef $
          fmap fst . runWriterT $ foldM externsEnv currentEnv missingExterns
        liftIO $ atomicModifyIORef' sharedEnvRef (\_ -> (newEnv, ()))

    -- | Record ExternsDiff for a freshly compiled module
    recordDiff :: ModuleName -> ExternsFile -> [ModuleName] -> Rock.Task Query ()
    recordDiff mn exts sortedDeps = do
      diffs <- liftIO $ readIORef diffsRef
      let depDiffs = map (\dep -> fromMaybe (emptyDiff dep) (M.lookup dep diffs)) sortedDeps
          diff = case M.lookup mn allCachedExterns of
            Just old -> diffExterns exts old depDiffs
            Nothing  -> emptyDiff mn
      liftIO $ atomicModifyIORef' diffsRef (\d -> (M.insert mn diff d, ()))
