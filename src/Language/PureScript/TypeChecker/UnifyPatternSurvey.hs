{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Pattern survey for unification cache hits/misses. Off by default;
-- gated on the @PURS_UNIFY_SURVEY=1@ environment variable. When
-- enabled, every pair fed into the unification cache check is
-- classified into a structural bucket and the (hit, miss) counts
-- per bucket are dumped on shutdown.
--
-- This module is deliberately separate from
-- 'Language.PureScript.TypeChecker.Unify' to avoid module-level
-- inlining contamination — see @experiments/LESSONS.md@.
module Language.PureScript.TypeChecker.UnifyPatternSurvey
  ( recordPair
  , dumpSurvey
  , surveyEnabled
  ) where

import Prelude

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (hPrintf)

import Language.PureScript.Types
  ( Type, pattern TUnknown
  , typeFlags
  , tfHasWildcards, tfSynonymsFree, hasFlag
  )
import Language.PureScript.Types qualified as T

-- Each bucket has two counters: hit and miss.
numBuckets :: Int
numBuckets = 6

bucketNames :: [String]
bucketNames =
  [ "hash_eq"
  , "root_tunknown_both"
  , "root_tunknown_one"
  , "has_wildcard"
  , "has_unsynonymed"
  , "concrete_synonym_free"
  ]

-- Twelve top-level IORefs feels worse than one IORef holding a
-- 12-element list of counts — atomic increments are simpler too.
counters :: IORef [Int]
counters = unsafePerformIO (newIORef (replicate (numBuckets * 2) 0))
{-# NOINLINE counters #-}

surveyEnabled :: Bool
surveyEnabled = unsafePerformIO $ do
  v <- lookupEnv "PURS_UNIFY_SURVEY"
  pure (v == Just "1")
{-# NOINLINE surveyEnabled #-}

-- Mutually-exclusive bucket id, first match wins. Uses only flag
-- reads + root pattern match; no traversal.
bucketOf :: Type a -> Type a -> Int
bucketOf t1 t2
  | T.typeHash t1 == T.typeHash t2     = 0
  | u1 && u2                           = 1
  | u1 || u2                           = 2
  | hasFlag tfHasWildcards f1 || hasFlag tfHasWildcards f2 = 3
  | not (hasFlag tfSynonymsFree f1 && hasFlag tfSynonymsFree f2) = 4
  | otherwise                          = 5
  where
    f1 = typeFlags t1
    f2 = typeFlags t2
    u1 = case t1 of TUnknown _ _ -> True; _ -> False
    u2 = case t2 of TUnknown _ _ -> True; _ -> False

bumpAt :: Int -> IO ()
bumpAt i = atomicModifyIORef' counters $ \xs ->
  let (h, t) = splitAt i xs
  in case t of
       []     -> (xs, ())
       (n:rs) -> (h ++ (n + 1) : rs, ())

recordPair :: Type a -> Type a -> Bool -> IO ()
recordPair t1 t2 hit
  | not surveyEnabled = pure ()
  | otherwise = do
      let b = bucketOf t1 t2
          i = b * 2 + (if hit then 0 else 1)
      bumpAt i

dumpSurvey :: IO ()
dumpSurvey
  | not surveyEnabled = pure ()
  | otherwise = do
      xs <- readIORef counters
      let pairs = chunk2 xs
          totalH = sum (map fst pairs)
          totalM = sum (map snd pairs)
          total  = totalH + totalM
      hPutStrLn stderr ""
      hPutStrLn stderr "=== unify-pattern-survey ==="
      _ <- hPrintf stderr "total lookups:    %12d\n" total
      _ <- hPrintf stderr "hits (in cache):  %12d  (%5.1f%%)\n"
             totalH (pct totalH total)
      _ <- hPrintf stderr "misses (added):   %12d  (%5.1f%%)\n"
             totalM (pct totalM total)
      hPutStrLn stderr ""
      hPutStrLn stderr "by bucket  (hits / misses / total / hit% / share-of-all-hits):"
      mapM_ (line totalH) (zip bucketNames pairs)
  where
    chunk2 (a:b:rest) = (a,b) : chunk2 rest
    chunk2 _          = []
    pct :: Int -> Int -> Double
    pct x t = if t == 0 then 0 else fromIntegral x * 100 / fromIntegral t
    line totalH (name, (h, m)) =
      let bt = h + m
          hp = pct h bt
          sh = pct h totalH
      in hPrintf stderr
           "  %-22s %12d / %12d / %12d   %5.1f%%   %5.1f%%\n"
           name h m bt hp sh
