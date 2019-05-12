module Mod where

import StaticPtr

f :: String
f = "I am a thing"

g :: StaticPtr (Boolean -> String)
g = static \b -> if b then "Foo" else f
