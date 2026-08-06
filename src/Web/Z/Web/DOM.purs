module Web.Z.Web.DOM
  ( XWEB
  , XWeb
  , XWebF
  , class IsEventTarget
  , evTarget
  , eventType
  , runXAThenExit
  , runXWeb
  , toEventTarget
  , xAddEventListener
  , xClosest
  , xDocument
  , xGetAttribute
  , xGetElementById
  , xLocationUrl
  , xPreventDefault
  , xPushState
  , xSetDocumentTitle
  , xStopPropagation
  , xWindow
  ) where

import Z.Prelude

import Debug (traceM)
import Effect.Unsafe as Unsafe
import Web.DOM.Element as Element
import Web.DOM.Internal.Types as T
import Web.DOM.NonElementParentNode as NEPN
import Web.DOM.ParentNode as PN
import Web.Event.Event as WebEvent
import Web.Event.EventTarget as WebEventT
import Web.Event.Internal.Types as WET
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.History as History
import Web.HTML.Location as Loc
import Web.HTML.Window as Window

evTarget :: WET.Event -> Maybe WET.EventTarget
evTarget = WebEvent.target

class IsEventTarget a where
  toEventTarget :: a -> WebEventT.EventTarget

instance htmlDocIsEventTarget :: IsEventTarget HTMLDoc.HTMLDocument where
  toEventTarget = HTMLDoc.toEventTarget

instance windowIsEventTarget :: IsEventTarget HTML.Window where
  toEventTarget = Window.toEventTarget

getElementById :: String -> HTMLDoc.HTMLDocument -> Effect (Maybe T.Element)
getElementById s = NEPN.getElementById s <<< HTMLDoc.toNonElementParentNode

xWindow :: forall x. XWeb x Window.Window
xWindow = lift _xWeb (WindowCmd id)

xLocationUrl :: forall x. XWeb x URL
xLocationUrl = lift _xWeb (LocationUrlCmd id)

xDocument :: forall x. XWeb x HTMLDoc.HTMLDocument
xDocument = lift _xWeb (DocumentCmd id)

xGetElementById :: forall x. String -> XWeb x (Maybe T.Element)
xGetElementById s = lift _xWeb (GetElementByIdCmd s id)

xClosest :: forall x. WET.EventTarget -> String -> XWeb x (Maybe T.Element)
xClosest et qs = lift _xWeb (ClosestCmd qs et id)

xGetAttribute :: forall x. T.Element -> String -> XWeb x (Maybe String)
xGetAttribute el attr = lift _xWeb (GetAttributeCmd attr el id)

eventType
  :: { click :: WebEvent.EventType
     , pushState :: WebEvent.EventType
     , popState :: WebEvent.EventType
     }
eventType =
  { click: WebEvent.EventType "click"
  , pushState: WebEvent.EventType "pushstate"
  , popState: WebEvent.EventType "popstate"
  }

type EventListenerOpts =
  { capture :: Boolean, once :: Boolean, passive :: Boolean }

defaultEventListenerOpts :: EventListenerOpts
defaultEventListenerOpts =
  { capture: false, once: false, passive: false }

xAddEventListener
  :: forall x t
   . IsEventTarget t
  => WebEvent.EventType
  -> t
  -> Edit EventListenerOpts
  -> (WebEvent.Event -> XWeb () Unit)
  -> XWeb x (XWeb x Unit)
xAddEventListener eType target opts onE = do
  let o = edit defaultEventListenerOpts opts
  let tgt = toEventTarget target
  el <- lift _xWeb $ AddEventListenerCmd (evalX <<< runXWeb) eType tgt o onE id
  pure $ lift _xWeb $ RmEventListenerCmd eType tgt o.capture el id

xPushState
  :: forall x
   . String
  -> Maybe String
  -> XWeb x Unit
xPushState url titleOr_ = do
  let title = jOr' titleOr_
  let hasTitle = isJust titleOr_
  let opts = if hasTitle then (encodeForeign { title }) else (encodeForeign {})
  lift _xWeb $ PushStateCmd opts title url unit

xSetDocumentTitle
  :: forall x
   . String
  -> XWeb x Unit
xSetDocumentTitle title = do
  lift _xWeb $ SetDocumentTitle title unit

xPreventDefault :: forall x. WET.Event -> XWeb x Unit
xPreventDefault e = lift _xWeb $ PreventDefaultCmd e unit

xStopPropagation :: forall x. WET.Event -> XWeb x Unit
xStopPropagation e = lift _xWeb $ StopPropagationCmd e unit

type XWeb x a = X (xWeb :: XWebF | x) a

type XWebRunner = forall a. XWeb () a -> a

data XWebF a
  = WindowCmd (Window.Window -> a)
  | DocumentCmd (HTMLDoc.HTMLDocument -> a)
  | LocationUrlCmd (URL -> a)
  | GetElementByIdCmd String (Maybe T.Element -> a)
  | ClosestCmd String WET.EventTarget (Maybe T.Element -> a)
  | GetAttributeCmd String T.Element (Maybe String -> a)
  | PreventDefaultCmd WET.Event a
  | StopPropagationCmd WET.Event a
  | AddEventListenerCmd
      XWebRunner
      WebEvent.EventType
      WebEventT.EventTarget
      EventListenerOpts
      (WebEvent.Event -> XWeb () Unit)
      (WebEventT.EventListener -> a)
  | RmEventListenerCmd
      WebEvent.EventType
      WebEventT.EventTarget
      Boolean
      WebEventT.EventListener
      (Unit -> a)
  | PushStateCmd Foreign String String a
  | SetDocumentTitle String a

handleXWeb :: forall r. XWebF ~> Run r
handleXWeb = case _ of
  WindowCmd f -> pure $ f (Unsafe.unsafePerformEffect HTML.window)
  DocumentCmd f -> pure $ f
    (Unsafe.unsafePerformEffect $ HTML.window >>= Window.document)
  LocationUrlCmd f -> pure $ f $ Unsafe.unsafePerformEffect do
    win <- HTML.window
    loc <- Window.location win
    hashs <- Loc.hash loc
    pathname <- Loc.pathname loc
    host <- Loc.hostname loc
    protocol <- Loc.protocol loc
    ports <- Loc.port loc
    let port = tryParseInt ports
    pure $ urlFromParts
      { hash: if (eq hashs "") then Nothing else Just hashs
      , port
      , username: Nothing
      , password: Nothing
      , protocol
      , host
      , query: mapEmpty
      , path: urlPathFromString pathname
      }
  GetElementByIdCmd id f -> pure $ f
    ( Unsafe.unsafePerformEffect $ HTML.window >>= Window.document >>=
        getElementById id
    )
  ClosestCmd qs et f -> pure $ f $ Unsafe.unsafePerformEffect do
    let orEl = Element.fromEventTarget et
    whenJust orEl $ Element.closest (PN.QuerySelector qs)
  GetAttributeCmd attr el f -> pure $ f $ Unsafe.unsafePerformEffect do
    Element.getAttribute attr el
  AddEventListenerCmd wr et t o h f -> pure $ f $ Unsafe.unsafePerformEffect do
    el <- WebEventT.eventListener \e -> pure $ wr $ h e
    WebEventT.addEventListenerWithOptions et el o t
    pure el
  RmEventListenerCmd et t c l f -> pure $ f $ Unsafe.unsafePerformEffect do
    traceM "removing event..."
    traceM { et, l, c, t }
    WebEventT.removeEventListener et l c t
  PushStateCmd f t u r -> pure $ Unsafe.unsafePerformEffect do
    w <- HTML.window
    h <- Window.history w
    History.pushState f (History.DocumentTitle t) (History.URL u) h
    pure r
  SetDocumentTitle t r -> pure $ Unsafe.unsafePerformEffect do
    w <- HTML.window
    d <- Window.document w
    HTMLDoc.setTitle t d
    pure r
  PreventDefaultCmd e r -> pure $ Unsafe.unsafePerformEffect do
    WebEvent.preventDefault e
    pure r
  StopPropagationCmd e r -> pure $ Unsafe.unsafePerformEffect do
    WebEvent.stopPropagation e
    pure r

derive instance functorXBaseF :: Functor XWebF

type XWEB x = (xWeb :: XWebF | x)

_xWeb = Proxy :: Proxy "xWeb"

runXWeb :: forall r. Run (XWEB + r) ~> Run r
runXWeb = run (on _xWeb handleXWeb send)

foreign import js_errorLog :: forall a. a -> Effect Unit

effAffThenExit :: forall e a. RtError e => Aff (Either e a) -> Effect Unit
effAffThenExit a = runAff_ onDone a
  where
  onDone (Left e) = do
    js_errorLog "process failed with UNHANDLED UNKNOWN error ⌄"
    js_errorLog e
  onDone (Right (Left e)) = do
    js_errorLog
      $ "process failed with known error [| "
      <> rtErrName e
      <> " |] ⌄"
    js_errorLog $ rtErrMessage e
  onDone _ = pure unit

type XWebEA e x = EA e (XWEB x)

runXAThenExit
  :: forall @w @e a. RtError e => XWa w (XWebEA e) a -> Effect Unit
runXAThenExit m = effAffThenExit $ runXA $ do
  w /\ res <- x' @"runW" $ expand $ runXWeb m
  when (arrSize w > 0) do
    xLogWarning "collected warnings ⌄"
    xLogWarning w
  pure res
