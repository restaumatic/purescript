{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Interner where

import Prelude

import Control.Exception
import Control.Monad (when)
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Base (compareInt#, Int#, IO (..), anyToAddr#, addr2Int#)
import GHC.Exts (Any, Addr#, unsafeCoerce#)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Text.ParserCombinators.ReadPrec (step)
import Text.Read (Read(..), lexP, parens, prec)
import Text.Read.Lex (Lexeme (Ident))
import Codec.Serialise (Serialise)
import Codec.Serialise.Class (Serialise(..))
import Control.DeepSeq (NFData (..))
import Data.String (IsString (..))



-- | 'HashCons' with a precomputed hash and an 'IORef' to the value.
--
-- WARNING: Do not use this type to wrap types whose Eq or Ord instances
-- allow distinguishable values to compare as equal; this will result in
-- nondeterminism or even visible mutation of semantically-immutable
-- values at runtime.
data HashCons a = HashConsC
  { _hashCons_hash :: {-# UNPACK #-} !Int       -- ^ Precomputed hash
  , _hashCons_ref  :: {-# UNPACK #-} !(IORef a) -- ^ Reference to the value
  }

instance (Hashable a, Serialise a) => Serialise (HashCons a) where
  encode hc = encode (unHashCons hc)
  decode = do
    (h :: a) <- decode
    pure $ hashCons h

instance NFData a => NFData (HashCons a) where
  rnf hc = rnf (unHashCons hc)

instance (Hashable a, IsString a) => IsString (HashCons a) where
  fromString s = hashCons (fromString s)

pattern HashCons :: Hashable a => () => a -> HashCons a
pattern HashCons x <- (unHashCons -> x) where
  HashCons x = hashCons x

-- | Create a new 'HashCons'.
hashCons :: Hashable a => a -> HashCons a
hashCons a = HashConsC (hash a) $ unsafeDupablePerformIO $ newIORef a
{-# INLINE hashCons #-}

-- | Extract the value from a 'HashCons'.
unHashCons :: HashCons a -> a
unHashCons (HashConsC _ ref) = unsafeDupablePerformIO $ readIORef ref
{-# INLINE unHashCons #-}

-- | Show instance that displays 'HashCons' in the format "hashCons <x>"
instance Show a => Show (HashCons a) where
    showsPrec d hc = showParen (d > appPrec) $
        showString "hashCons " . showsPrec (appPrec + 1) (unHashCons hc)
      where
        appPrec = 10

-- | Read instance that parses 'HashCons' from the format "hashCons <x>"
instance (Read a, Hashable a) => Read (HashCons a) where
  readPrec = parens $ prec 10 $ do
    Ident "hashCons" <- lexP
    a <- step readPrec
    pure $ hashCons a

instance Eq a => Eq (HashCons a) where
  HashConsC h1 ref1 == HashConsC h2 ref2
    | ref1 == ref2 = True
    | h1 /= h2 = False
    | otherwise = compareAndSubstitute ((==) :: a -> a -> Bool) True ref1 ref2
  {- INLINE (==) #-}

-- | NOTE: This instance orders by hash first, and only secondarily by
-- the 'Ord' instance of 'a', to improve performance.
instance Ord a => Ord (HashCons a) where
  compare (HashConsC h1 ref1) (HashConsC h2 ref2) = case compare h1 h2 of
    EQ -> if ref1 == ref2
      then EQ
      else compareAndSubstitute compare EQ ref1 ref2
    result -> result
  {-# INLINE compare #-}

instance Eq a => Hashable (HashCons a) where
  hashWithSalt salt (HashConsC h _) = hashWithSalt salt h
  {-# INLINE hashWithSalt #-}

-- Compare the values in the IORefs with the given comparator, and if the result
-- indicates that they are equal, replace one with the other, preferring the one
-- whose pointer is lower.  This is not expected to be totally stable, but it
-- should be *somewhat* stable, and should push us in direction of coalescing
-- more values.  Without this, if you have a, b, and c, all with equal but
-- distinct values, and compare b == a and b == c repeatedly, but never compare
-- a == c, you could end up with the value of b flapping between that of a and
-- c, costing the worst-case equality check time repeatedly, and never settling
-- on a particular representation of the value.  With this, you should settle on
-- a single value unless you get extremely unlucky with the way that addresses
-- move around.
compareAndSubstitute
  :: Eq r
  => (a -> a -> r)
  -> r
  -> IORef a
  -> IORef a
  -> r
compareAndSubstitute cmp eq ref1 ref2  = unsafeDupablePerformIO $ do
  a1 <- readIORef ref1
  a2 <- readIORef ref2
  let result = a1 `cmp` a2
  when (result == eq) $ do
    -- NOTE: These should already be forced by (==), but in the unlikely event
    -- that they are not (i.e. because (==) on their type unconditionally
    -- returns True), we need to ensure they are not thunks, according to the
    -- documentation of anyToAddr#
    evaluate a1
    evaluate a2
    -- NOTE: There is a race condition here: the addresses could change in
    -- between when they are read.  However, since either (or neither) swap is
    -- fine, we are OK with this only working "most" of the time (which we
    -- expect to be a very high fraction).
    addrCmpResult <- IO $ \s ->
      case anyToAddr# (unsafeCoerce# a1 :: Any) s of
        (# s', addr1 #) -> case anyToAddr# (unsafeCoerce# a2 :: Any) s' of
          (# s'', addr2 #) -> (# s'', addr2Int# addr1 `compareInt#` addr2Int# addr2 #)
    case addrCmpResult of
      LT -> writeIORef ref2 a1
      GT -> writeIORef ref1 a2
      EQ -> pure ()
  pure result
{-# INLINE compareAndSubstitute #-}
