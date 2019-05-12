module StaticPtr
  ( StaticPtr
  , static
  , deref
  , unsafeMakeStaticPtr

  , Key
  , key

  , StaticPtrTable
  , staticPtrTable
  ) where

import Data.Function.Uncurried (Fn2, runFn2)

type Key = String

newtype StaticPtr a = StaticPtr Key

static :: forall a. a -> StaticPtr a
static = let f x = f x in f 1

key :: forall a. StaticPtr a -> Key
key (StaticPtr k) = k

unsafeMakeStaticPtr :: forall a. Key -> StaticPtr a
unsafeMakeStaticPtr = StaticPtr

deref :: forall a. StaticPtr a -> a
deref ptr = runFn2 _deref staticPtrTable ptr

foreign import data StaticPtrTable :: Type

foreign import staticPtrTable :: StaticPtrTable

foreign import _deref :: forall a. Fn2 StaticPtrTable (StaticPtr a) a
