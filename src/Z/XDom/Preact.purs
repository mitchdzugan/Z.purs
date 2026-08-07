module Z.XDom.Preact
  ( PropWF(..)
  , ReactEl
  , js_effComponent
  , js_propsFromPropWs
  , js_renderEl
  , js_renderFragment
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
  :: forall s
   . (Unit -> Run () Unit)
  -> (s -> (s -> Run () Unit) -> Array ReactEl)
  -> s
  -> ReactEl

foreign import js_withKey :: String -> ReactEl -> ReactEl
foreign import js_effComponent
  :: forall a
   . (a -> a -> Boolean)
  -> a
  -> (Unit -> (Unit -> Unit))
  -> ((Unit -> Unit) -> Unit)
  -> ReactEl

foreign import js_withBoundedError
  :: forall e. (e -> ReactEl) -> (Unit -> ReactEl) -> ReactEl

foreign import js_throwBoundedError :: forall e a. e -> a

data PropWF
  = Href String
  | ClassName String
  | OnClick (Int -> Unit)
  | PKey String

propWFKey :: PropWF -> String
propWFKey (ClassName _) = "className"
propWFKey (Href _) = "href"
propWFKey (OnClick _) = "onClick"
propWFKey (PKey _) = "key"

propWFVal :: PropWF -> JsAny
propWFVal (ClassName s) = jsAny s
propWFVal (Href s) = jsAny s
propWFVal (OnClick s) = jsAny s
propWFVal (PKey s) = jsAny s