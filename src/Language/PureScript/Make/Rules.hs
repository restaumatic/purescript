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
-- (Just (externs, outputTimestamp)) = source unchanged, cached externs available
-- Nothing = needs rebuild
data CacheInfo = CacheInfo
  { ciCachedExterns :: !(Maybe (ExternsFile, UTCTime))
  , ciOldExterns    :: !(Maybe ExternsFile)
    -- ^ Old externs for ExternsDiff (loaded even for changed modules)
  }

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
  -> Maybe Traces.CachedGraph
     -- ^ Cached module graph from previous build (if valid)
  -> IORef (Maybe ([ModuleName], [(ModuleName, [ModuleName])]))
     -- ^ Captures computed graph for persistence
  -> Rock.Rules Query
makeRules modules opts actions warningsRef compileFn cacheDb diffsRef sharedEnvRef newCacheDbRef timestampsRef cachedGraph graphRef = \case

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
        (sorted, graph) <- sortModules Transitive (moduleSignature . CST.resPartial) prs
        let result = map (getModuleName . CST.resPartial) sorted
        -- Capture for persistence
        liftIO $ atomicModifyIORef' graphRef (\_ -> (Just (result, graph), ()))
        pure result

  ModuleGraph -> case cachedGraph of
    Just cg | M.keysSet modules == S.fromList (Traces.cgSorted cg) -> pure $ M.fromList (Traces.cgGraph cg)
    _ -> do
      let allNames = M.keys modules
      _ <- traverse (\mn -> Rock.fetch (InputModule mn)) allNames
      liftMake opts warningsRef $ do
        let prs = M.elems modules
        (sorted, graph) <- sortModules Transitive (moduleSignature . CST.resPartial) prs
        let result = map (getModuleName . CST.resPartial) sorted
        -- Capture for persistence (if not already done by SortedModules)
        liftIO $ atomicModifyIORef' graphRef (\prev -> case prev of
          Nothing -> (Just (result, graph), ())
          just    -> (just, ()))
        pure $ M.fromList graph

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

    case ciCachedExterns cache of
      Just (cached, myTimestamp) -> do
        -- Source unchanged. Check if any dep was rebuilt after us.
        timestamps <- liftIO $ readIORef timestampsRef
        let depsNewerThanMe = any (\dep ->
              maybe False (> myTimestamp) (getDepTimestamp timestamps dep)) sortedDeps

        if depsNewerThanMe then do
          exts <- doCompile mn sortedDeps depExterns
          recordDiff mn exts (ciOldExterns cache) sortedDeps
          pure exts
        else do
          -- Check ExternsDiff
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
            updateSharedEnv sortedDeps depExterns
            liftMake opts warningsRef $
              progress actions $ SkippingModule mn Nothing
            liftIO $ atomicModifyIORef' diffsRef (\d -> (M.insert mn (emptyDiff mn) d, ()))
            pure cached

      Nothing -> do
        exts <- doCompile mn sortedDeps depExterns
        recordDiff mn exts (ciOldExterns cache) sortedDeps
        pure exts

  where
    -- | Lazily check a single module's cache status.
    -- This is the key difference from the eager approach: only called
    -- when rock actually demands this module.
    checkModuleCache :: ModuleName -> Rock.Task Query CacheInfo
    checkModuleCache mn = liftIO $ do
      -- Run the cache check directly in IO via runMake, avoiding
      -- the overhead of accumulating into warningsRef (cache checks
      -- don't produce warnings).
      (result, _warnings) <- runMake opts $ do
        inputInfo <- getInputTimestampsAndHashes actions mn
        case inputInfo of
          Left RebuildAlways -> do
            (_, mbOld) <- readExterns actions mn
            pure $ CacheInfo Nothing mbOld
          Left RebuildNever -> do
            (_, mbExterns) <- readExterns actions mn
            let epoch = UTCTime (toEnum 0) 0
            pure $ CacheInfo (fmap (\e -> (e, epoch)) mbExterns) mbExterns
          Right timestamps -> do
            cwd <- liftIO getCurrentDirectory
            (newCacheInfo, upToDate) <- Cache.checkChanged cacheDb mn cwd timestamps
            liftIO $ atomicModifyIORef' newCacheDbRef (\db -> (M.insert mn newCacheInfo db, ()))
            if upToDate then do
              outputTs <- getOutputTimestamp actions mn
              case outputTs of
                Nothing -> pure $ CacheInfo Nothing Nothing
                Just ts -> do
                  liftIO $ atomicModifyIORef' timestampsRef (\m -> (M.insert mn ts m, ()))
                  (_, mbExterns) <- readExterns actions mn
                  pure $ CacheInfo (fmap (\e -> (e, ts)) mbExterns) mbExterns
            else do
              (_, mbOld) <- readExterns actions mn
              pure $ CacheInfo Nothing mbOld
      case result of
        Left _errs -> pure $ CacheInfo Nothing Nothing
        Right info -> pure info

    -- | Get a dep's output timestamp (recorded during cache check).
    getDepTimestamp :: M.Map ModuleName UTCTime -> ModuleName -> Maybe UTCTime
    getDepTimestamp timestamps dep = M.lookup dep timestamps

    doCompile :: ModuleName -> [ModuleName] -> [ExternsFile] -> Rock.Task Query ExternsFile
    doCompile mn sortedDeps depExterns = do
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

    recordDiff :: ModuleName -> ExternsFile -> Maybe ExternsFile -> [ModuleName] -> Rock.Task Query ()
    recordDiff mn exts mbOldExterns sortedDeps = do
      diffs <- liftIO $ readIORef diffsRef
      let depDiffs = map (\dep -> fromMaybe (emptyDiff dep) (M.lookup dep diffs)) sortedDeps
          diff = case mbOldExterns of
            Just old -> diffExterns exts old depDiffs
            Nothing  -> emptyDiff mn
      liftIO $ atomicModifyIORef' diffsRef (\d -> (M.insert mn diff d, ()))
