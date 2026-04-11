module Language.PureScript.Make
  ( make
  , make_
  , rebuildModule
  , rebuildModule'
  , inferForeignModules
  , module Monad
  , module Actions
  ) where

import Prelude

import Control.Concurrent.Async (forConcurrently)
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (foldM, void, when)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ask)
import Control.Monad.Supply (evalSupplyT, runSupply, runSupplyT)
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Writer.Class (MonadWriter(..), censor)
import Control.Monad.Writer.Strict (runWriterT)
import Data.Function (on)
import Data.Foldable (fold, for_)
import Data.IORef (newIORef, readIORef)
import Data.List (foldl', sortOn)
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.PureScript.AST (ErrorMessageHint(..), Module(..), getModuleName, getModuleSourceSpan, importPrim)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Docs.Convert qualified as Docs
import Language.PureScript.Environment (initEnvironment)
import Language.PureScript.Errors (MultipleErrors(..), SimpleErrorMessage(..), addHint, defaultPPEOptions, errorMessage', errorMessage'', errorModule, prettyPrintMultipleErrors)
import Language.PureScript.Externs (ExternsFile, applyExternsFileToEnvironment, moduleToExternsFile)
import Language.PureScript.Linter (Name(..), lint, lintImports)
import Language.PureScript.Names (ModuleName(..), isBuiltinModuleName, runModuleName)
import Language.PureScript.Renamer (renameInModule)
import Language.PureScript.Sugar (Env, collapseBindingGroups, createBindingGroups, desugar, desugarCaseGuards, externsEnv, primEnv)
import Language.PureScript.TypeChecker (CheckState(..), emptyCheckState, typeCheckModule)
import Language.PureScript.Make.Actions as Actions
import Language.PureScript.Make.Cache qualified as Cache
import Language.PureScript.Make.Monad as Monad
    ( Make(..),
      writeTextFile,
      writeJSONFile,
      writeCborFileIO,
      writeCborFile,
      setTimestamp,
      runMake,
      readTextFile,
      readJSONFileIO,
      readJSONFile,
      readExternsFile,
      readCborFileIO,
      readCborFile,
      makeIO,
      hashFile,
      getTimestampMaybe,
      getTimestamp,
      getCurrentTime,
      copyFile )
import Language.PureScript.Make.Query (Query(..))
import Language.PureScript.Make.Rules (makeRules, MakeError(..))
import Language.PureScript.Make.Traces qualified as Traces
import Language.PureScript.CoreFn qualified as CF
import Rock qualified
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension, (</>))
import Language.PureScript.TypeChecker.Monad (liftTypeCheckM)

-- | Rebuild a single module.
--
rebuildModule
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule actions externs m = do
  env <- fmap fst . runWriterT $ foldM externsEnv primEnv externs
  rebuildModule' actions env externs m

rebuildModule'
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule' act env ext mdl = rebuildModuleWithIndex act env ext mdl Nothing

rebuildModuleWithIndex
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> [ExternsFile]
  -> Module
  -> Maybe (Int, Int)
  -> m ExternsFile
rebuildModuleWithIndex MakeActions{..} exEnv externs m@(Module _ _ moduleName _ _) moduleIndex = do
  progress $ CompilingModule moduleName moduleIndex
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      withPrim = importPrim m
  lint withPrim

  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    (desugared, (exEnv', usedImports)) <- runStateT (desugar externs withPrim) (exEnv, mempty)
    let modulesExports = (\(_, _, exports) -> exports) <$> exEnv'
    (checked, CheckState{..}) <- runStateT (liftTypeCheckM $ typeCheckModule modulesExports desugared) $ emptyCheckState env
    let usedImports' = foldl' (flip $ \(fromModuleName, newtypeCtorName) ->
          M.alter (Just . (fmap DctorName newtypeCtorName :) . fold) fromModuleName) usedImports checkConstructorImportsForCoercible
    -- Imports cannot be linted before type checking because we need to
    -- known which newtype constructors are used to solve Coercible
    -- constraints in order to not report them as unused.
    censor (addHint (ErrorInModule moduleName)) $ lintImports checked exEnv' usedImports'
    return (checked, checkEnv)

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated

  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      (optimized, nextVar'') = runSupply nextVar' $ CF.optimizeCoreFn corefn
      (renamedIdents, renamed) = renameInModule optimized
      exts = moduleToExternsFile mod' env' renamedIdents
  ffiCodegen renamed

  -- It may seem more obvious to write `docs <- Docs.convertModule m env' here,
  -- but I have not done so for two reasons:
  -- 1. This should never fail; any genuine errors in the code should have been
  -- caught earlier in this function. Therefore if we do fail here it indicates
  -- a bug in the compiler, which should be reported as such.
  -- 2. We do not want to perform any extra work generating docs unless the
  -- user has asked for docs to be generated.
  let docs = case Docs.convertModule externs exEnv env' withPrim of
               Left errs -> internalError $
                 "Failed to produce docs for " ++ T.unpack (runModuleName moduleName)
                 ++ "; details:\n" ++ prettyPrintMultipleErrors defaultPPEOptions errs
               Right d -> d

  evalSupplyT nextVar'' $ codegen renamed docs exts
  return exts

-- | Compiles in "make" mode using rock for demand-driven incremental compilation.
-- Each module is compiled separately to a @.js@ file and an @externs.cbor@ file.
-- Rock automatically memoizes query results within a build to avoid redundant work.
--
-- It collects and returns externs for all modules passed, in topological order.
make :: MakeActions Make
     -> [CST.PartialResult Module]
     -> Make [ExternsFile]
make ma ms = makeIncremental ma ms

-- | Like 'make' but discards the result.
make_ :: MakeActions Make
      -> [CST.PartialResult Module]
      -> Make ()
make_ ma ms = void $ makeIncremental ma ms

-- | Rock-based incremental compilation.
-- Defines queries for each compilation phase and lets rock handle
-- memoization and dependency tracking.
makeIncremental
  :: MakeActions Make
  -> [CST.PartialResult Module]
  -> Make [ExternsFile]
makeIncremental ma@MakeActions{..} ms = do
  -- Validate module names (no Prim redefinitions, no duplicates)
  checkModuleNames

  -- Get compiler options from the Make monad's Reader environment
  opts <- ask

  -- Build the module map for the rules to close over
  let moduleMap = M.fromList
        [ (getModuleName (CST.resPartial pr), pr) | pr <- ms ]

  -- Read cache database for incremental build support
  cacheDb <- readCacheDb

  -- Try to load cached module graph from previous build.
  -- If valid (all input hashes match), we skip the expensive sortModules call.
  let graphFile = getOutputDir </> "module-graph.json"
  -- Load cached module graph if available and valid.
  -- Disabled for now: needs further debugging for test compatibility.
  -- cachedGraph <- liftIO $ Traces.readCachedGraph graphFile cacheDb (S.fromList $ M.keys moduleMap)
  let cachedGraph = (Nothing :: Maybe Traces.CachedGraph)

  -- IORefs for state accumulated during the rock build
  warningsRef <- liftIO $ newIORef mempty
  memoVar <- liftIO $ newIORef mempty
  diffsRef <- liftIO $ newIORef M.empty
  sharedEnvRef <- liftIO $ newIORef primEnv
  newCacheDbRef <- liftIO $ newIORef cacheDb
  timestampsRef <- liftIO $ newIORef M.empty
  -- Captured graph data for persistence
  graphRef <- liftIO $ newIORef (Nothing :: Maybe ([ModuleName], [(ModuleName, [ModuleName])]))

  let compileFn = rebuildModule' ma

  let rules :: Rock.Rules Query
      rules = Rock.memoise memoVar
            $ makeRules moduleMap opts ma warningsRef compileFn cacheDb diffsRef sharedEnvRef newCacheDbRef timestampsRef cachedGraph graphRef

  -- Run the rock task: sort modules, then compile all in parallel.
  -- Rock's memoise handles synchronization: if module B depends on A,
  -- B's thread blocks on A's MVar until A completes. This gives us
  -- natural parallelism bounded by the dependency graph.
  let rockTask = Rock.runTask rules $ do
        sorted <- Rock.fetch SortedModules
        -- Fork all module compilations concurrently within the same Task.
        -- Each fork shares the same Fetch function (and thus memoization).
        liftIO $ forConcurrently sorted $ \mn ->
          Rock.runTask rules $ Rock.fetch (CompileModule mn)
  result <- liftIO (try rockTask) :: Make (Either SomeException [ExternsFile])

  -- Collect warnings accumulated during rock execution and emit them
  extraWarnings <- liftIO $ readIORef warningsRef
  tell extraWarnings

  case result of
    Left exc
      | Just (MakeError errs) <- fromException exc -> do
          -- On failure, remove ONLY the failed modules from CacheDb.
          newCacheDb <- liftIO $ readIORef newCacheDbRef
          let failedModules = S.fromList $ mapMaybe errorModule (runMultipleErrors errs)
          writeCacheDb $ Cache.removeModules failedModules newCacheDb
          throwError errs
      | otherwise -> liftIO $ throwIO exc
    Right externs -> do
      -- Write updated cache database
      newCacheDb <- liftIO $ readIORef newCacheDbRef
      writeCacheDb newCacheDb
      -- Save module graph for next build
      mbGraph <- liftIO $ readIORef graphRef
      case mbGraph of
        Just (sorted, graph) ->
          liftIO $ Traces.writeCachedGraph graphFile sorted graph newCacheDb
        Nothing -> pure ()
      writePackageJson
      outputPrimDocs
      pure externs

  where
  checkModuleNames :: Make ()
  checkModuleNames = checkNoPrim *> checkModuleNamesAreUnique

  checkNoPrim :: Make ()
  checkNoPrim =
    for_ ms $ \m ->
      let mn = getModuleName $ CST.resPartial m
      in when (isBuiltinModuleName mn) $
           throwError
             . errorMessage' (getModuleSourceSpan $ CST.resPartial m)
             $ CannotDefinePrimModules mn

  checkModuleNamesAreUnique :: Make ()
  checkModuleNamesAreUnique =
    for_ (findDuplicates (getModuleName . CST.resPartial) ms) $ \mss ->
      throwError . flip foldMap mss $ \ms' ->
        let mn = getModuleName . CST.resPartial . NEL.head $ ms'
        in errorMessage'' (fmap (getModuleSourceSpan . CST.resPartial) ms') $ DuplicateModule mn

  -- Find all groups of duplicate values in a list based on a projection.
  findDuplicates :: Ord b => (a -> b) -> [a] -> Maybe [NEL.NonEmpty a]
  findDuplicates f xs =
    case filter ((> 1) . length) . NEL.groupBy ((==) `on` f) . sortOn f $ xs of
      [] -> Nothing
      xss -> Just xss

-- | Infer the module name for a module by looking for the same filename with
-- a .js extension.
inferForeignModules
  :: forall m
   . MonadIO m
  => M.Map ModuleName (Either RebuildPolicy FilePath)
  -> m (M.Map ModuleName FilePath)
inferForeignModules =
    fmap (M.mapMaybe id) . traverse inferForeignModule
  where
    inferForeignModule :: Either RebuildPolicy FilePath -> m (Maybe FilePath)
    inferForeignModule (Left _) = return Nothing
    inferForeignModule (Right path) = do
      let jsFile = replaceExtension path "js"
      exists <- liftIO $ doesFileExist jsFile
      if exists
        then return (Just jsFile)
        else return Nothing
