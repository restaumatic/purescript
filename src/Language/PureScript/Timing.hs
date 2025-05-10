module Language.PureScript.Timing
  ( Metric
  , newMetric
  , report
  , timedIO
  , timed
  , printResult
  ) where

import Protolude

import System.Metrics.Counter qualified as Counter
import System.Clock
import System.IO.Unsafe (unsafePerformIO)

data Metric = Metric
  { name :: Text
  , totalTime :: Counter.Counter
  }

newMetric :: Text -> IO Metric
newMetric name = Metric name <$> Counter.new

-- | Report an occurence of the event with given duration.
report :: Metric -> Int64 -> IO ()
report Metric{totalTime} time = Counter.add totalTime time

timedIO :: MonadIO m => Metric -> m a -> m a
timedIO metric action = do
  (result, time) <- measureTime action
  liftIO $ report metric time
  pure result
{-# INLINE timedIO #-}

-- | Measure time it takes to evaluate the given value and report to the given Metric.
timed :: Metric -> a -> a
timed metric x = unsafePerformIO $ timedIO metric (evaluate x)
{-# INLINE timed #-}

-- | Measure time of an IO action, return result in nanoseconds
measureTime :: MonadIO m => m a -> m (a, Int64)
measureTime action = do
  start <- liftIO $ getTime Monotonic
  result <- action
  end <- liftIO $ getTime Monotonic
  pure (result, diffTimeSpecNanos end start)
{-# INLINE measureTime #-}

diffTimeSpecNanos :: TimeSpec -> TimeSpec -> Int64
diffTimeSpecNanos (TimeSpec s1 ns1) (TimeSpec s2 ns2) =
  (s1 - s2) * 1000000000 + (ns1 - ns2)

printResult :: Metric -> IO ()
printResult Metric{name, totalTime} = do
  total <- Counter.read totalTime
  putStrLn $ "METRIC " <> name <> " " <> show total
