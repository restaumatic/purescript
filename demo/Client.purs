module Client where

import Prelude
import StaticPtr
import StaticPtr as StaticPtr
import Effect
import Effect.Console as Console
import Widget
import Widget as Widget

foreign import findComments :: Effect (Array { key :: StaticPtr.Key, node :: Node })
foreign import clientImpl :: Node -> Effect Widget.Impl

main :: Effect Unit
main = do
  comments <- findComments
  foreachE comments \{key, node} -> do
    Widget.Impl impl <- clientImpl node
    let impl' = impl { client = \ptr -> run (Widget.Impl impl') ptr }
    run (Widget.Impl impl') (unsafeMakeStaticPtr key)

  where
  run :: Widget.Impl -> StaticPtr (Widget Unit) -> Effect Unit
  run impl ptr = do
    Console.log $ "Running: " <> key ptr
    let Widget widget = deref ptr
    widget impl
