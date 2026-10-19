module Z.XDom.Preact
  ( DomEvent
  , PropWF(..)
  , ReactEl
  , js_effComponent
  , js_propsFromPropWs
  , js_renderEl
  , js_renderFragment
  , js_renderToString
  , js_renderToStringAsync
  , js_textEl
  , js_throwBoundedError
  , js_withBoundedError
  , js_withKey
  , js_withState
  , propWFKey
  , propWFVal
  ) where

import Z.Prelude

foreign import data ReactEl :: Type

foreign import js_textEl :: String -> ReactEl
foreign import js_renderFragment :: Array ReactEl -> ReactEl
foreign import js_renderEl :: String -> Json -> Array ReactEl -> ReactEl
foreign import js_propsFromPropWs
  :: (PropWF -> String) -> (PropWF -> JsAny) -> Array PropWF -> Json

foreign import js_withState
  :: forall s. (s -> (s -> Effect Unit) -> Array ReactEl) -> s -> ReactEl

foreign import js_withKey :: String -> ReactEl -> ReactEl
foreign import js_effComponent
  :: forall a
   . (a -> a -> Boolean)
  -> a
  -> (Effect (Effect Unit))
  -> ReactEl

foreign import js_withBoundedError
  :: forall e. String -> (e -> Effect ReactEl) -> (Effect ReactEl) -> ReactEl

foreign import js_throwBoundedError :: forall e a. String -> e -> a

foreign import js_renderToString :: ReactEl -> String
foreign import js_renderToStringAsync :: ReactEl -> Effect (Promise String)

foreign import data DomEvent :: Type

data PropWF
  = Href String
  | ClassName String
  | OnClick (DomEvent -> Unit)
  | Id String
  | PKey String
  | Role String
  | Target String

propWFKey :: PropWF -> String
propWFKey (ClassName _) = "className"
propWFKey (Href _) = "href"
propWFKey (OnClick _) = "onClick"
propWFKey (Id _) = "id"
propWFKey (PKey _) = "key"
propWFKey (Role _) = "role"
propWFKey (Target _) = "target"

propWFVal :: PropWF -> JsAny
propWFVal (ClassName s) = jsAny s
propWFVal (Href s) = jsAny s
propWFVal (OnClick s) = jsAny s
propWFVal (Id s) = jsAny s
propWFVal (PKey s) = jsAny s
propWFVal (Role s) = jsAny s
propWFVal (Target s) = jsAny s
