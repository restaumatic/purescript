module Main where

import Prelude

import StaticPtr
import Effect
import Effect.Console as Console
import Unsafe.Coerce

main :: Effect Unit
main = do
  Console.log "Welcome"
  let ptr = unsafeCoerce "Mod._static_0" :: StaticPtr (Boolean -> String)
  Console.log (deref ptr true)
  Console.log (deref ptr false)
