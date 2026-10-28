module Z.XDom.Preact
  ( DomEl
  , DomEvent
  , ElStyles
  , ReactEl
  , js_effComponent
  , js_renderArray
  , js_renderEl
  , js_renderFragment
  , js_renderToString
  , js_renderToStringAsync
  , js_textEl
  , js_throwBoundedError
  , js_withBoundedError
  , js_withKey
  , js_withRef
  , js_withState
  ) where

import Z.Prelude

foreign import data ReactEl :: Type

foreign import js_textEl :: String -> ReactEl
foreign import js_renderFragment :: Array ReactEl -> ReactEl
foreign import js_renderArray :: Array ReactEl -> ReactEl
foreign import js_renderEl :: String -> Json -> Array ReactEl -> ReactEl

foreign import js_withRef
  :: forall s. (Effect s -> (s -> Effect Unit) -> Array ReactEl) -> s -> ReactEl

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

foreign import data DomEl :: Type

type ElStyles = Object String

