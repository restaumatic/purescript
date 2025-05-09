module Main where

import Prim.Row
import Effect
import Effect.Console
import Type.Proxy (Proxy(..))
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Monoid ((<>))
import Prelude (discard)

class KeysRec (xs :: RowList Type) where
  keysImpl :: Proxy xs -> String

instance KeysRec Nil where
  keysImpl _ = ""

instance (IsSymbol label, KeysRec tail) => KeysRec (Cons label v tail) where
  keysImpl _ = reflectSymbol (Proxy :: Proxy label) <> ";" <> keysImpl (Proxy :: Proxy tail)


class Keys (x :: Type) where
  keys :: x -> String

instance (RowToList xs xsl, KeysRec xsl) => Keys (Record xs) where
  keys r = keysImpl (Proxy :: Proxy xsl)

newtype Wrapper a = Wrapper a

instance (Keys (Record a)) => Keys (Wrapper (Record a)) where
  keys (Wrapper r) = keys r

main = do
  log (keys { foo: 1, bar: 2, baz: 3 })
  log (keys (Wrapper { foo: 1, baz: 3, bar: 2 }))
  log "Done"
