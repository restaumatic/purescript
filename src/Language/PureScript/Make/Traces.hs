-- | Cached module graph for cross-build incrementality.
-- Persisted to disk alongside cache-db.json. Invalidated when
-- any input module's content hash changes.
module Language.PureScript.Make.Traces
  ( CachedGraph(..)
  , readCachedGraph
  , writeCachedGraph
  ) where

import Prelude

import Data.Aeson qualified as Aeson
import Data.Aeson ((.=), (.:))
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as S
import Data.Version (showVersion)
import Language.PureScript.Make.Cache (CacheDb)
import Language.PureScript.Names (ModuleName)
import Paths_purescript qualified as Paths
import System.Directory (doesFileExist)
import System.IO.Error (tryIOError)

-- | Cached module graph: sorted module list and dependency graph.
-- Only valid when all input module hashes match the CacheDb.
data CachedGraph = CachedGraph
  { cgVersion :: String
  , cgSorted :: [ModuleName]
  , cgGraph :: [(ModuleName, [ModuleName])]
  , cgInputHashes :: CacheDb
    -- ^ Snapshot of input hashes when graph was computed.
    -- If current CacheDb matches, graph is still valid.
  } deriving (Show)

instance Aeson.ToJSON CachedGraph where
  toJSON CachedGraph{..} = Aeson.object
    [ "version" .= cgVersion
    , "sorted" .= cgSorted
    , "graph" .= cgGraph
    , "hashes" .= cgInputHashes
    ]

instance Aeson.FromJSON CachedGraph where
  parseJSON = Aeson.withObject "CachedGraph" $ \v ->
    CachedGraph
      <$> v .: "version"
      <*> v .: "sorted"
      <*> v .: "graph"
      <*> v .: "hashes"

-- | Try to read a cached graph. Returns Nothing if:
-- - File doesn't exist
-- - File can't be parsed
-- - Compiler version differs
-- - The set of module names differs from the current compilation
-- - Any input hash differs from the current CacheDb
readCachedGraph :: FilePath -> CacheDb -> S.Set ModuleName -> IO (Maybe CachedGraph)
readCachedGraph path currentCacheDb currentModules = do
  exists <- doesFileExist path
  if not exists then pure Nothing
  else do
    result <- tryIOError $ LBS.readFile path
    case result of
      Left _ -> pure Nothing
      Right bs -> case Aeson.decode bs of
        Nothing -> pure Nothing
        Just cg
          | cgVersion cg /= showVersion Paths.version -> pure Nothing
          | S.fromList (cgSorted cg) /= currentModules -> pure Nothing
          | cgInputHashes cg /= currentCacheDb -> pure Nothing
          | otherwise -> pure (Just cg)

-- | Write cached graph to disk.
writeCachedGraph :: FilePath -> [ModuleName] -> [(ModuleName, [ModuleName])] -> CacheDb -> IO ()
writeCachedGraph path sorted graph cacheDb = do
  let cg = CachedGraph
        { cgVersion = showVersion Paths.version
        , cgSorted = sorted
        , cgGraph = graph
        , cgInputHashes = cacheDb
        }
  _ <- tryIOError $ LBS.writeFile path (Aeson.encode cg)
  pure ()
