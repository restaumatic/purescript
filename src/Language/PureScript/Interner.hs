module Language.PureScript.Interner
  ( Interner
  , Interned
  , intern
  , unintern
  , internText
  , uninternText
  , psStringInterner
  , textInterner
  , internPSString
  , uninternPSString
  , getInternedHash
  ) where

import Prelude
import Control.Concurrent.MVar
import Data.Map.Strict qualified as M
import Data.IntMap.Strict qualified as IM
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)
import Data.Word (Word16)
import Control.DeepSeq (NFData, deepseq)
import Data.String (IsString(..))
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Vector.Unboxed qualified as VU
import System.Random (randomIO)

-- | The opaque interned identifier
newtype Interned = Interned Int
  deriving (Eq, NFData)

instance Hashable Interned where
  hashWithSalt salt (Interned i) = hashWithSalt salt i

getInternedHash :: Interned -> Int
getInternedHash (Interned i) = i

instance IsString Interned where
  fromString s = internText (fromString s)

instance Show Interned where
  show (Interned i) = "<interned:" ++ show i ++ ">"


-- | A reusable interner structure
-- param 'k' is the key (e.g., Text, Vector Word16)
data Interner k = Interner
  { internerMap  :: !(M.Map k Interned)
  , reverseMap   :: !(IM.IntMap k)
  , internerId :: Int
  }
  deriving (Eq, Show)

type InternerVar k = MVar (Interner k)

-- | Intern a key and get its Interned ID
intern :: (Hashable k, Ord k, NFData k) => InternerVar k -> k -> Interned
intern var k = unsafePerformIO $ do
  k `deepseq` modifyMVar var $ \st -> do
      -- Check if the key is already interned
    case M.lookup k (internerMap st) of
      Just i -> pure (st, i)
      Nothing ->
        let h = hash k
            i = Interned h
            m' = M.insert k i (internerMap st)
            im' = IM.insert h k (reverseMap st)
         in pure (st { internerMap = m', reverseMap = im' }, i)

-- | Reverse an Interned ID back to the original key
unintern :: InternerVar k -> Interned -> k
unintern var (Interned i) = unsafePerformIO $ do
  Interner { reverseMap, internerId } <- readMVar var
  case IM.lookup i reverseMap of
    Just v -> pure v
    Nothing -> error $ "Unknown interned ID: " ++ show i <> " interner: " <> show internerId

{-# NOINLINE textInterner #-}
textInterner :: InternerVar Text
textInterner = unsafePerformIO $ randomIO >>= \r -> newMVar $ Interner M.empty IM.empty r


internText :: Text -> Interned
internText !t = intern textInterner t

uninternText :: Interned -> Text
uninternText !i = unintern textInterner i


{-# NOINLINE psStringInterner #-}
psStringInterner :: InternerVar [Word16]
psStringInterner = unsafePerformIO $ randomIO >>= \r -> newMVar $ Interner M.empty IM.empty r

newtype Word16Vec = Word16Vec { unVector :: VU.Vector Word16 }
  deriving (Eq, Ord, NFData, Show)

instance Hashable Word16Vec where
    hashWithSalt salt (Word16Vec vec) = hashWithSalt salt (VU.toList vec)

internPSString :: [Word16] -> Interned
internPSString !wa = intern psStringInterner wa

uninternPSString :: Interned -> [Word16]
uninternPSString !i = unintern psStringInterner i
