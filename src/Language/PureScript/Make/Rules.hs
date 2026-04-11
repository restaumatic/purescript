{-# LANGUAGE GADTs #-}

module Language.PureScript.Make.Rules
  ( makeRules
  , MakeError(..)
  , liftMake
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

import Rock qualified

import Language.PureScript.AST (Module(..), getModuleName, getModuleSourceSpan)
import Language.PureScript.AST.SourcePos (spanName)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Environment (initEnvironment)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Externs (ExternsFile, applyExternsFileToEnvironment)
import Language.PureScript.Make.Actions (MakeActions(..), ProgressMessage(..), RebuildPolicy(..))
import Language.PureScript.Make.Cache (CacheDb)
import Language.PureScript.Make.Cache qualified as Cache
import Language.PureScript.Make.ExternsDiff (ExternsDiff, checkDiffs, diffExterns, emptyDiff)
import Language.PureScript.Make.Monad (Make, runMake)
import Language.PureScript.Make.Query (Query(..))
import Language.PureScript.ModuleDependencies (DependencyDepth(..), moduleSignature, sortModules)
import Language.PureScript.Names (ModuleName, runModuleName)
import Language.PureScript.Options (Options)
import Language.PureScript.Make.Traces qualified as Traces
import Language.PureScript.Sugar (Env, externsEnv)

import Control.Monad.Writer.Strict (runWriterT)
import Data.Time.Clock (UTCTime(..))
import System.Directory (getCurrentDirectory)

-- | Exception wrapper for compilation errors.
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

-- | Per-module cache info, computed lazily on demand.
data CacheInfo = CacheInfo
  { ciCacheStatus :: !CacheStatus
  , ciOldExterns  :: !(Maybe ExternsFile)
    -- ^ Old externs for ExternsDiff (loaded even for changed modules)
  }

-- | Whether a module's build artifacts are up to date.
data CacheStatus
  = CacheHit !UTCTime
    -- ^ Source unchanged, output exists at this timestamp
  | CacheMiss
    -- ^ Needs rebuild (source changed or output missing)

-- | Define the rock rules for the incremental compilation pipeline.
makeRules
  :: M.Map ModuleName (CST.PartialResult Module)
  -> Options
  -> MakeActions Make
  -> IORef MultipleErrors
  -> CompileFn
  -> CacheDb
  -> IORef (M.Map ModuleName ExternsDiff)
  -> IORef Env
  -> IORef CacheDb
  -> IORef (M.Map ModuleName UTCTime)
  -> IORef (S.Set ModuleName)
     -- ^ Modules actually compiled (not skipped) in this build
  -> Maybe Traces.CachedGraph
     -- ^ Cached module graph from previous build (if valid)
  -> IORef (Maybe ([ModuleName], [(ModuleName, [ModuleName])]))
     -- ^ Captures computed graph for persistence
  -> Rock.Rules Query
makeRules modules opts actions warningsRef compileFn cacheDb diffsRef sharedEnvRef newCacheDbRef timestampsRef compiledRef cachedGraph graphRef = \case

  InputModule mn ->
    case M.lookup mn modules of
      Just pr -> pure (CST.resPartial pr)
      Nothing -> liftIO . throwIO . MakeError $ internalError
        ("makeRules: InputModule: module not found: " <> show (runModuleName mn))

  SortedModules -> case cachedGraph of
    Just cg | M.keysSet modules == S.fromList (Traces.cgSorted cg) -> pure (Traces.cgSorted cg)
    _ -> do
      let allNames = M.keys modules
      _ <- traverse (\mn -> Rock.fetch (InputModule mn)) allNames
      liftMake opts warningsRef $ do
        let prs = M.elems modules
        -- Use Direct deps for sorting (cheaper than Transitive).
        -- Transitive closure is computed on demand in ModuleGraph.
        (sorted, directGraph) <- sortModules Direct (moduleSignature . CST.resPartial) prs
        let result = map (getModuleName . CST.resPartial) sorted
        -- Capture direct graph for persistence (compact on disk)
        liftIO $ atomicModifyIORef' graphRef (\_ -> (Just (result, directGraph), ()))
        pure result

  ModuleGraph -> case cachedGraph of
    Just cg | M.keysSet modules == S.fromList (Traces.cgSorted cg) ->
      pure $ transitiveClosure (M.fromList (Traces.cgGraph cg))
    _ -> do
      -- Ensure SortedModules has run (which populates graphRef)
      _ <- Rock.fetch SortedModules
      directGraph <- liftIO $ readIORef graphRef
      case directGraph of
        Just (_sorted, graph) -> pure $ transitiveClosure (M.fromList graph)
        Nothing -> liftIO . throwIO . MakeError $ internalError
          "makeRules: ModuleGraph: graphRef not populated"

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

    -- Lazy cache check: only done when this module is actually demanded
    cache <- checkModuleCache mn

    case ciCacheStatus cache of
      CacheHit myTimestamp -> do
        -- Source unchanged. Check if any dep was rebuilt externally (not
        -- in this build) by comparing output timestamps. For deps rebuilt
        -- in THIS build, ExternsDiff tells us if the interface changed.
        diffs <- liftIO $ readIORef diffsRef
        timestamps <- liftIO $ readIORef timestampsRef
        compiled <- liftIO $ readIORef compiledRef
        let depDiffs = map (\dep -> fromMaybe (emptyDiff dep) (M.lookup dep diffs)) sortedDeps
            pr = fromMaybe (internalError "makeRules: missing module")
                   (M.lookup mn modules)
            fullModule = case snd (CST.resFull pr) of
              Right m  -> m
              Left _   -> CST.resPartial pr
            -- A dep rebuilt externally (in a previous build, not this one)
            -- has newer output. We must recompile since ExternsDiff can't
            -- tell us what changed in its externs across builds.
            hasExternallyRebuiltDep = any (\dep ->
              not (S.member dep compiled) && depHasNewerOutput timestamps dep myTimestamp) sortedDeps
            needsRebuild = hasExternallyRebuiltDep || checkDiffs fullModule depDiffs

        if needsRebuild then do
          -- Load cached externs for diff computation
          mbCached <- loadExterns mn
          exts <- doCompile mn sortedDeps depExterns
          let diff = case mbCached of
                Just old -> diffExterns exts old depDiffs
                Nothing  -> emptyDiff mn
          liftIO $ atomicModifyIORef' diffsRef (\d -> (M.insert mn diff d, ()))
          pure exts
        else do
          -- Skip: deps' externs haven't meaningfully changed.
          -- Don't call updateSharedEnv here — doCompile handles
          -- missing env entries if a downstream module needs compilation.
          liftMake opts warningsRef $
            progress actions $ SkippingModule mn Nothing
          liftIO $ atomicModifyIORef' diffsRef (\d -> (M.insert mn (emptyDiff mn) d, ()))
          -- Load externs only now (deferred from cache check)
          mbCached <- loadExterns mn
          case mbCached of
            Just cached -> pure cached
            Nothing -> do
              -- Externs missing on disk even though cache says up to date.
              -- Fall back to recompilation.
              exts <- doCompile mn sortedDeps depExterns
              recordDiff mn exts Nothing sortedDeps
              pure exts

      CacheMiss -> do
        exts <- doCompile mn sortedDeps depExterns
        recordDiff mn exts (ciOldExterns cache) sortedDeps
        pure exts

  where
    -- | Check if a dependency's output is newer than a given timestamp.
    -- Used to detect deps rebuilt in a previous build (not in this one).
    depHasNewerOutput :: M.Map ModuleName UTCTime -> ModuleName -> UTCTime -> Bool
    depHasNewerOutput timestamps dep myTimestamp =
      maybe False (> myTimestamp) (M.lookup dep timestamps)

    -- | Lazily check a single module's cache status.
    -- This is the key difference from the eager approach: only called
    -- when rock actually demands this module.
    checkModuleCache :: ModuleName -> Rock.Task Query CacheInfo
    checkModuleCache mn = liftIO $ do
      -- Run the cache check directly in IO via runMake, avoiding
      -- the overhead of accumulating into warningsRef (cache checks
      -- don't produce warnings).
      -- Note: does NOT read externs here — deferred to loadExterns
      -- to avoid reading .cbor files for modules that don't need them.
      (result, _warnings) <- runMake opts $ do
        inputInfo <- getInputTimestampsAndHashes actions mn
        case inputInfo of
          Left RebuildAlways -> do
            (_, mbOld) <- readExterns actions mn
            pure $ CacheInfo CacheMiss mbOld
          Left RebuildNever -> do
            let epoch = UTCTime (toEnum 0) 0
            pure $ CacheInfo (CacheHit epoch) Nothing
          Right timestamps -> do
            cwd <- liftIO getCurrentDirectory
            (newCacheInfo, upToDate) <- Cache.checkChanged cacheDb mn cwd timestamps
            liftIO $ atomicModifyIORef' newCacheDbRef (\db -> (M.insert mn newCacheInfo db, ()))
            if upToDate then do
              outputTs <- getOutputTimestamp actions mn
              case outputTs of
                Nothing -> pure $ CacheInfo CacheMiss Nothing
                Just ts -> do
                  liftIO $ atomicModifyIORef' timestampsRef (\m -> (M.insert mn ts m, ()))
                  pure $ CacheInfo (CacheHit ts) Nothing
            else do
              (_, mbOld) <- readExterns actions mn
              pure $ CacheInfo CacheMiss mbOld
      case result of
        Left _errs -> pure $ CacheInfo CacheMiss Nothing
        Right info -> pure info

    -- | Load cached externs from disk. Only called when externs are
    -- actually needed (for compilation or to return as a result).
    loadExterns :: ModuleName -> Rock.Task Query (Maybe ExternsFile)
    loadExterns mn = liftIO $ do
      (result, _) <- runMake opts $ do
        (_, mbExterns) <- readExterns actions mn
        pure mbExterns
      case result of
        Right ext -> pure ext
        Left _    -> pure Nothing

    doCompile :: ModuleName -> [ModuleName] -> [ExternsFile] -> Rock.Task Query ExternsFile
    doCompile mn sortedDeps depExterns = do
      liftIO $ atomicModifyIORef' compiledRef (\s -> (S.insert mn s, ()))
      currentEnv <- liftIO $ readIORef sharedEnvRef
      let pr = fromMaybe (internalError $ "makeRules: CompileModule: module not found: " <> show (runModuleName mn))
                 (M.lookup mn modules)
          fp = spanName . getModuleSourceSpan . CST.resPartial $ pr
          (pwarnings, mres) = CST.resFull pr
          missingExterns = [ exts
                           | (dep, exts) <- zip sortedDeps depExterns
                           , not (M.member dep currentEnv)
                           ]
      liftMake opts warningsRef $ do
        sugarEnv <- fmap fst . runWriterT $ foldM externsEnv currentEnv missingExterns
        liftIO $ atomicModifyIORef' sharedEnvRef (\_ -> (sugarEnv, ()))
        tell $ CST.toMultipleWarnings fp pwarnings
        m <- CST.unwrapParserError fp mres
        compileFn sugarEnv depExterns m

    recordDiff :: ModuleName -> ExternsFile -> Maybe ExternsFile -> [ModuleName] -> Rock.Task Query ()
    recordDiff mn exts mbOldExterns sortedDeps = do
      diffs <- liftIO $ readIORef diffsRef
      let depDiffs = map (\dep -> fromMaybe (emptyDiff dep) (M.lookup dep diffs)) sortedDeps
          diff = case mbOldExterns of
            Just old -> diffExterns exts old depDiffs
            Nothing  -> emptyDiff mn
      liftIO $ atomicModifyIORef' diffsRef (\d -> (M.insert mn diff d, ()))

-- | Compute transitive closure of a direct dependency graph.
-- For each module, find all modules reachable via dependencies.
transitiveClosure :: M.Map ModuleName [ModuleName] -> M.Map ModuleName [ModuleName]
transitiveClosure directGraph = M.mapWithKey (\mn _ -> S.toList (go S.empty (directDeps mn))) directGraph
  where
    directDeps :: ModuleName -> [ModuleName]
    directDeps mn = fromMaybe [] (M.lookup mn directGraph)
    go :: S.Set ModuleName -> [ModuleName] -> S.Set ModuleName
    go visited [] = visited
    go visited (dep:deps)
      | S.member dep visited = go visited deps
      | otherwise = go (S.insert dep visited) (directDeps dep ++ deps)
