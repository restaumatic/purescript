{-# LANGUAGE NumDecimals #-}

module Language.PureScript.Timing where

import Prelude

import System.IO.Unsafe
import System.Clock
import Text.Printf
import Control.Exception
import Control.Monad.IO.Class

timedIO :: MonadIO m => String -> m a -> m a
timedIO str action = do
  start <- liftIO $ getTime Monotonic
  result <- action >>= (liftIO . evaluate)
  end <- liftIO $ getTime Monotonic
  let diff = (fromIntegral $ toNanoSecs (end - start)) / (10e8)
  liftIO $ printf "%30s: %0.3f sec\n" str (diff :: Double)
  pure result
