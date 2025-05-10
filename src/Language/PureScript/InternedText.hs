module Language.PureScript.InternedText
  ( InternedName
  , intern
  , unintern
  , IsString (..)
  , internText
  , uninternText
  ) where

import Prelude
import Control.Concurrent.MVar
import Data.Map.Strict qualified as M
import Data.IntMap.Strict qualified as IM
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)
import Data.String (IsString(..))
import Control.DeepSeq (NFData)
import GHC.Stack (HasCallStack)

newtype InternedName = InternedName Int
  deriving (Show)
  deriving newtype (NFData, Eq, Ord)

instance IsString InternedName where
  fromString s = intern (fromString s :: Text)

-- Global state
{-# NOINLINE interner #-}
interner :: MVar (M.Map a InternedName, IM.IntMap a, InternedName)
interner = unsafePerformIO $ newMVar (M.empty, IM.empty, InternedName 0)

internText :: Text -> InternedName
internText = intern

uninternText :: InternedName -> Text
uninternText = unintern

intern :: Ord a => a -> InternedName
intern s = s `seq` unsafePerformIO $ do
  modifyMVar interner $ \(m, im, next) ->
    case M.lookup s m of
      Just i -> pure ((m, im, next), i)
      Nothing ->
        let i@(InternedName ii) = next
            next' = InternedName (ii + 1)
         in pure ((M.insert s i m, IM.insert ii s im, next'), i)


unintern :: HasCallStack => InternedName -> a
unintern (InternedName i) = unsafePerformIO $ do
  (_, im, _) <- readMVar interner
  case IM.lookup i im of
    Just s -> pure s
    Nothing -> error $ "Unknown interned name: " ++ show i


