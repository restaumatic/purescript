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
import Language.PureScript.CoreFn qualified as CF
import Rock qualified
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (replaceExtension)
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
  let docs = case Docs.convertModule externs exEnv env' m of
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

  -- Compute cache status for each module: check which modules have
  -- unchanged source files and valid cached externs on disk.
  -- Also load all previously cached externs (for ExternsDiff computation).
  (cacheStatus, allCachedExterns) <- computeCacheStatus ma cacheDb (M.keys moduleMap)
  -- Compute new CacheDb entries while checking
  newCacheDb <- computeNewCacheDb ma cacheDb (M.keys moduleMap)

  -- IORef to accumulate warnings from rock Task executions
  warningsRef <- liftIO $ newIORef mempty
  -- IORef for rock's within-build memoization cache
  memoVar <- liftIO $ newIORef mempty
  -- IORef for tracking ExternsDiff of recompiled modules
  diffsRef <- liftIO $ newIORef M.empty

  -- The per-module compilation function, partially applied with MakeActions
  let compileFn = rebuildModule' ma

  -- Construct memoized rock rules
  let rules :: Rock.Rules Query
      rules = Rock.memoise memoVar
            $ makeRules moduleMap opts ma warningsRef compileFn cacheStatus allCachedExterns diffsRef

  -- Run the rock task: sort modules and compile each one.
  let rockTask = Rock.runTask rules $ do
        sorted <- Rock.fetch SortedModules
        traverse (\mn -> Rock.fetch (CompileModule mn)) sorted
  result <- liftIO (try rockTask) :: Make (Either SomeException [ExternsFile])

  -- Collect warnings accumulated during rock execution and emit them
  extraWarnings <- liftIO $ readIORef warningsRef
  tell extraWarnings

  case result of
    Left exc
      | Just (MakeError errs) <- fromException exc -> do
          -- On failure, remove ONLY the failed modules from CacheDb.
          -- This ensures the failed modules are rebuilt next time, while
          -- preserving cache entries for modules that didn't fail.
          let failedModules = S.fromList $ mapMaybe errorModule (runMultipleErrors errs)
          writeCacheDb $ Cache.removeModules failedModules newCacheDb
          throwError errs
      | otherwise -> liftIO $ throwIO exc
    Right externs -> do
      -- Write updated cache database
      writeCacheDb newCacheDb
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

-- | Compute cache status for each module: determine which modules have
-- unchanged source files and valid cached externs on disk.
-- Returns:
-- 1. CacheStatus: map from module name to Maybe ExternsFile (Just = can reuse, Nothing = needs rebuild)
-- 2. AllCachedExterns: map of ALL previously cached externs (for ExternsDiff computation)
computeCacheStatus
  :: MakeActions Make
  -> Cache.CacheDb
  -> [ModuleName]
  -> Make (M.Map ModuleName (Maybe ExternsFile), M.Map ModuleName ExternsFile)
computeCacheStatus MakeActions{..} cacheDb moduleNames = do
  results <- traverse checkModule moduleNames
  let cacheStatusMap = M.fromList [(mn, status) | (mn, status, _) <- results]
      allCached = M.fromList [(mn, exts) | (mn, _, Just exts) <- results]
  pure (cacheStatusMap, allCached)
  where
    checkModule mn = do
      -- Always try to load cached externs (needed for diff computation)
      (_, mbExterns) <- readExterns mn
      inputInfo <- getInputTimestampsAndHashes mn
      case inputInfo of
        Left RebuildAlways -> pure (mn, Nothing, mbExterns)
        Left RebuildNever -> pure (mn, mbExterns, mbExterns)
        Right timestamps -> do
          cwd <- liftIO getCurrentDirectory
          (_newCacheInfo, upToDate) <- Cache.checkChanged cacheDb mn cwd timestamps
          if upToDate then do
            outputTs <- getOutputTimestamp mn
            case outputTs of
              Nothing -> pure (mn, Nothing, mbExterns)
              Just _  -> pure (mn, mbExterns, mbExterns)
          else
            pure (mn, Nothing, mbExterns)

-- | Compute the updated CacheDb entries for all modules.
computeNewCacheDb
  :: MakeActions Make
  -> Cache.CacheDb
  -> [ModuleName]
  -> Make Cache.CacheDb
computeNewCacheDb MakeActions{..} cacheDb moduleNames = do
  foldM updateModule cacheDb moduleNames
  where
    updateModule db mn = do
      inputInfo <- getInputTimestampsAndHashes mn
      case inputInfo of
        Left _ -> pure db  -- RebuildPolicy modules don't update cache
        Right timestamps -> do
          cwd <- liftIO getCurrentDirectory
          (newCacheInfo, _) <- Cache.checkChanged db mn cwd timestamps
          pure $ M.insert mn newCacheInfo db

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
