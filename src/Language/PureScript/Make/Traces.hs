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
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Version (showVersion)
import Language.PureScript.Make.Cache (CacheDb, ContentHash, hash)
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
  , cgCacheDbHash :: ContentHash
    -- ^ Hash of the serialized CacheDb when graph was computed.
    -- If current CacheDb hashes the same, graph is still valid.
  } deriving (Show)

instance Aeson.ToJSON CachedGraph where
  toJSON CachedGraph{..} = Aeson.object
    [ "version" .= cgVersion
    , "sorted" .= cgSorted
    , "graph" .= cgGraph
    , "cacheDbHash" .= cgCacheDbHash
    ]

instance Aeson.FromJSON CachedGraph where
  parseJSON = Aeson.withObject "CachedGraph" $ \v ->
    CachedGraph
      <$> v .: "version"
      <*> v .: "sorted"
      <*> v .: "graph"
      <*> v .: "cacheDbHash"

-- | Compute a hash of the CacheDb for comparison purposes.
hashCacheDb :: CacheDb -> ContentHash
hashCacheDb = hash . LBS.toStrict . Aeson.encode

-- | Try to read a cached graph. Returns Nothing if:
-- - File doesn't exist
-- - File can't be parsed
-- - Compiler version differs
-- - The set of module names differs from the current compilation
-- - The CacheDb hash differs (some input changed)
-- - The CacheDb lacks entries for some current modules (can't verify content)
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
          -- Require that the CacheDb has entries for all current modules.
          -- Without content hashes for every module, we can't verify the
          -- dependency graph is still valid (e.g. when modules use
          -- RebuildAlways/RebuildNever, their hashes are not tracked).
          | not (currentModules `S.isSubsetOf` M.keysSet currentCacheDb) -> pure Nothing
          | cgCacheDbHash cg /= hashCacheDb currentCacheDb -> pure Nothing
          | otherwise -> pure (Just cg)

-- | Write cached graph to disk.
writeCachedGraph :: FilePath -> [ModuleName] -> [(ModuleName, [ModuleName])] -> CacheDb -> IO ()
writeCachedGraph path sorted graph cacheDb = do
  let cg = CachedGraph
        { cgVersion = showVersion Paths.version
        , cgSorted = sorted
        , cgGraph = graph
        , cgCacheDbHash = hashCacheDb cacheDb
        }
  _ <- tryIOError $ LBS.writeFile path (Aeson.encode cg)
  pure ()
