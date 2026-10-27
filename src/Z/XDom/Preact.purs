module Z.XDom.Preact
  ( DomEl
  , DomEvent
  , ElStyles
  , PropWF(..)
  , ReactEl
  , js_effComponent
  , js_propsFromPropWs
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
  , propWFKey
  , propWFVal
  ) where

import Z.Prelude

import Web.Event.Internal.Types as WET

foreign import data ReactEl :: Type

foreign import js_textEl :: String -> ReactEl
foreign import js_renderFragment :: Array ReactEl -> ReactEl
foreign import js_renderArray :: Array ReactEl -> ReactEl
foreign import js_renderEl :: String -> Json -> Array ReactEl -> ReactEl
foreign import js_propsFromPropWs
  :: (PropWF -> String) -> (PropWF -> JsAny) -> Array PropWF -> Json

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

data PropWF
  = Href String
  | Ref (DomEl -> Unit)
  | OnClick (DomEvent -> Unit)
  | OnError (DomEvent -> Unit)
  | OnAnimationEnd (DomEvent -> Unit)
  | Id String
  | PKey String
  | Role String
  | Target String
  | TabIndex Int
  | Src String
  | XmlNS String
  | ViewBox String
  | Fill String
  | D String
  | Data String String
  | Type String
  | Checked Boolean
  | Loading String
  | Alt String

propWFKey :: PropWF -> String
propWFKey (Href _) = "href"
propWFKey (Ref _) = "ref"
propWFKey (OnClick _) = "onClick"
propWFKey (OnError _) = "onError"
propWFKey (OnAnimationEnd _) = "onAnimationEnd"
propWFKey (Id _) = "id"
propWFKey (PKey _) = "key"
propWFKey (Role _) = "role"
propWFKey (Target _) = "target"
propWFKey (TabIndex _) = "tabIndex"
propWFKey (Src _) = "src"
propWFKey (XmlNS _) = "xmlns"
propWFKey (ViewBox _) = "viewBox"
propWFKey (Fill _) = "fill"
propWFKey (D _) = "d"
propWFKey (Data k _) = "data-" <> k
propWFKey (Type _) = "type"
propWFKey (Checked _) = "checked"
propWFKey (Alt _) = "alt"
propWFKey (Loading _) = "loading"

propWFVal :: PropWF -> JsAny
propWFVal (Href s) = jsAny s
propWFVal (Ref f) = jsAny f
propWFVal (OnClick f) = jsAny f
propWFVal (OnError f) = jsAny f
propWFVal (OnAnimationEnd f) = jsAny f
propWFVal (Id s) = jsAny s
propWFVal (PKey s) = jsAny s
propWFVal (Role s) = jsAny s
propWFVal (Target s) = jsAny s
propWFVal (TabIndex i) = jsAny i
propWFVal (Src s) = jsAny s
propWFVal (XmlNS s) = jsAny s
propWFVal (ViewBox s) = jsAny s
propWFVal (Fill s) = jsAny s
propWFVal (D s) = jsAny s
propWFVal (Data _ v) = jsAny v
propWFVal (Type s) = jsAny s
propWFVal (Checked b) = jsAny b
propWFVal (Alt s) = jsAny s
propWFVal (Loading s) = jsAny s
