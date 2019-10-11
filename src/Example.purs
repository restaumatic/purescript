module Example where

import Prelude hiding (pure)
import Effect
import Data.Function.Uncurried
import Effect.Console
import Data.Monoid
import Data.Either
import Data.Maybe
import OtherModule
import Unsafe.Coerce

undefined :: forall a. a
undefined = unsafeCoerce 1

foreign import data D :: Type -> Type

pure :: forall f a. a -> Wrap f a
pure _ = unsafeCoerce 1

intD :: Wrap D Int
intD = 1

add1 :: Int -> Int
add1 x = unsafeCoerce 1

example_lift = add1 intD

consume :: forall a. Wrap D a -> Unit
consume _ = undefined

example_pure = consume (pure 1)
example_pure_auto = consume 1
