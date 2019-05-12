module Server where

import Prelude

import Effect
import Effect.Console as Console
import App (app)
import Widget as Widget

main :: Effect Unit
main = do
  Console.log ""
  html <- Widget.runServer app
  Console.log html
