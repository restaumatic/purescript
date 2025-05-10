module Language.PureScript.Timing
  ( Metric
  , newMetric
  , start
  , report
  , timedIO
  , timed
  , suspendIO
  , printResults
  ) where

import Protolude

import System.Metrics.Counter qualified as Counter
import System.Clock
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef', writeIORef)
import Text.Printf (printf)
import Data.Text qualified as Text

allMetrics :: IORef [Metric]
allMetrics = unsafePerformIO $ newIORef []
{-# NOINLINE allMetrics #-}

startTime :: IORef TimeSpec
startTime = unsafePerformIO $ newIORef (TimeSpec 0 0)
{-# NOINLINE startTime #-}

start :: IO ()
start = do
  t <- getTime Monotonic
  writeIORef startTime t

data Metric = Metric
  { name :: Text
  , totalTime :: Counter.Counter
  }

newMetric :: Text -> IO Metric
newMetric name = do
  m <- Metric name <$> Counter.new
  atomicModifyIORef' allMetrics (\ms -> (m:ms, ()))
  pure m

-- | Report an occurence of the event with given duration.
report :: Metric -> Int64 -> IO ()
report Metric{totalTime} time = Counter.add totalTime time

timedIO :: MonadIO m => Metric -> m a -> m a
timedIO metric action = do
  (result, time) <- measureTime action
  liftIO $ report metric time
  pure result
{-# INLINE timedIO #-}

-- | When inside a timedIO block, "suspend" measuring a metric for the duration of a sublock.
-- Implemented like timedIO but subtracts the value
suspendIO :: MonadIO m => Metric -> m a -> m a
suspendIO metric action = do
  (result, time) <- measureTime action
  liftIO $ report metric (-time)
  pure result
{-# INLINE suspendIO #-}

-- | Measure time it takes to evaluate the given value and report to the given Metric.
timed :: Metric -> a -> a
timed metric x = unsafePerformIO $ timedIO metric (evaluate x)
{-# INLINE timed #-}

-- | Measure time of an IO action, return result in nanoseconds
measureTime :: MonadIO m => m a -> m (a, Int64)
measureTime action = do
  start' <- liftIO $ getTime Monotonic
  result <- action
  end <- liftIO $ getTime Monotonic
  pure (result, diffTimeSpecNanos end start')
{-# INLINE measureTime #-}

diffTimeSpecNanos :: TimeSpec -> TimeSpec -> Int64
diffTimeSpecNanos (TimeSpec s1 ns1) (TimeSpec s2 ns2) =
  (s1 - s2) * 1000000000 + (ns1 - ns2)

printResults :: IO ()
printResults = do
  startTime' <- readIORef startTime
  endTime <- getTime Monotonic
  let total = diffTimeSpecNanos endTime startTime'
  putStrLn $ ("TOTAL " :: Text) <> show total

  metrics <- readIORef allMetrics
  for_ metrics $ \Metric{name,totalTime} -> do
    metricTotal <- Counter.read totalTime
    let percentage = fromIntegral metricTotal / fromIntegral total * 100 :: Double
    putStrLn $ "METRIC " <> name <> " " <> show metricTotal <> " " <> Text.pack (printf "%.2f" percentage) <> "%"
