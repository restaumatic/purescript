module Widget where

import Prelude
import Effect
import Effect.Uncurried
import StaticPtr
import StaticPtr as StaticPtr
import Effect.Ref as Ref
import Data.List as List

foreign import data Node :: Type

type TagName = String

newtype Impl = Impl
  { pushElement :: TagName -> Effect Unit
  , popElement :: Effect Unit
  , appendTextNode :: String -> Effect Unit
  , client :: StaticPtr (Widget Unit) -> Effect Unit
  }

newtype Widget a = Widget (Impl -> Effect a)

derive instance functorWidget :: Functor Widget
instance applyWidget :: Apply Widget where
  apply (Widget f) (Widget x) = Widget \impl -> f impl <*> x impl

instance applicativeWidget :: Applicative Widget where
  pure x = Widget \_ -> pure x

instance bindWidget :: Bind Widget where
  bind (Widget m) k = Widget \impl -> do
    x <- m impl
    case k x of
      Widget k' ->
        k' impl

el :: forall a. TagName -> Widget a -> Widget a
el tag (Widget body) = Widget \(Impl impl) -> do
  impl.pushElement tag
  result <- body (Impl impl)
  impl.popElement
  pure result

text :: String -> Widget Unit
text str = Widget \(Impl impl) -> impl.appendTextNode str

client :: StaticPtr (Widget Unit) -> Widget Unit
client code = Widget \(Impl impl) -> impl.client code

runServer :: Widget Unit -> Effect String
runServer (Widget widget) = do
  output <- Ref.new ""
  tagStack <- Ref.new List.Nil
  let append str = Ref.modify_ (_ <> str) output
  let impl = Impl
        { pushElement: \tag -> do
            Ref.modify_ (List.Cons tag) tagStack
            append ("<" <> tag <> ">")
        , popElement: do
           tags <- Ref.read tagStack
           case tags of
             List.Nil -> pure unit
             List.Cons tag rest -> do
               append ("</" <> tag <> ">")
               Ref.write rest tagStack

        , appendTextNode: append

        , client: \ptr -> append $ "<!-- client:" <> StaticPtr.key ptr <> " -->"
        }
  widget impl
  Ref.read output
