module Web.Z.Web.DOM
  ( XWeb
  , XWebF
  , class IsEventTarget
  , eventType
  , runXWeb
  , toEventTarget
  , xAddEventListener
  , xDocument
  , runXThenExit
  , xGetElementById
  , xPushState
  , xSetDocumentTitle
  , xWindow
  ) where

import Z.Prelude

import Web.DOM.Internal.Types as T
import Web.DOM.NonElementParentNode as NEPN
import Web.Event.Event as WebEvent
import Web.Event.EventTarget as WebEventT
import Web.HTML.History as History
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window as Window
import Effect.Unsafe as Unsafe

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

xDocument :: forall x. XWeb x HTMLDoc.HTMLDocument
xDocument = lift _xWeb (DocumentCmd id)

xGetElementById :: forall x. String -> XWeb x (Maybe T.Element)
xGetElementById s = lift _xWeb (GetElementByIdCmd s id)

eventType :: { click :: WebEvent.EventType }
eventType = { click: WebEvent.EventType "click" }

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
  -> XWeb x (XWeb () Unit)
xAddEventListener eType target opts onE = do
  let o = edit defaultEventListenerOpts opts
  let tgt = toEventTarget target
  el <- lift _xWeb $ AddEventListenerCmd (evalX <<< runXWeb) eType tgt o onE id
  pure $ lift _xWeb $ RmEventListenerCmd eType tgt o.capture el unit

xPushState
  :: forall x
   . String
  -> Maybe String
  -> XWeb x Unit
xPushState url mt = do
  let title = jOr' mt
  let f = if isJust mt then (encodeForeign { title }) else (encodeForeign {})
  lift _xWeb $ PushStateCmd f title url unit

xSetDocumentTitle
  :: forall x
   . String
  -> XWeb x Unit
xSetDocumentTitle title = do
  lift _xWeb $ SetDocumentTitle title unit

type XWeb x a = X (xWeb :: XWebF | x) a

type XWebRunner = forall a. XWeb () a -> a

data XWebF a
  = WindowCmd (Window.Window -> a)
  | DocumentCmd (HTMLDoc.HTMLDocument -> a)
  | GetElementByIdCmd String (Maybe T.Element -> a)
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
      a
  | PushStateCmd Foreign String String a
  | SetDocumentTitle String a

handleXWeb :: forall r. XWebF ~> Run r
handleXWeb = case _ of
  WindowCmd f -> pure $ f (Unsafe.unsafePerformEffect HTML.window)
  DocumentCmd f -> pure $ f
    (Unsafe.unsafePerformEffect $ HTML.window >>= Window.document)
  GetElementByIdCmd id f -> pure $ f
    ( Unsafe.unsafePerformEffect $ HTML.window >>= Window.document >>=
        getElementById id
    )
  AddEventListenerCmd wr et t o h f -> pure $ f $ Unsafe.unsafePerformEffect do
    el <- WebEventT.eventListener \e -> pure $ wr $ h e
    WebEventT.addEventListenerWithOptions et el o t
    pure el
  RmEventListenerCmd et t c l r -> pure $ Unsafe.unsafePerformEffect do
    WebEventT.removeEventListener et l c t
    pure r
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

derive instance functorXBaseF :: Functor XWebF

type XWEB x = (xWeb :: XWebF | x)

_xWeb = Proxy :: Proxy "xWeb"

runXWeb :: forall r. Run (XWEB + r) ~> Run r
runXWeb = run (on _xWeb handleXWeb send)

foreign import js_errorLog :: forall a. a -> Effect Unit

execAndExit :: forall e a. RtError e => Aff (Either e a) -> Effect Unit
execAndExit a = runAff_ onDone a
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

runXThenExit
  :: forall @w @e a. RtError e => XWa w (XWebEA e) a -> Effect Unit
runXThenExit m = execAndExit $ runXA $ do
  w /\ res <- x RunW $ expand $ runXWeb m
  when (arrSize w > 0) do
    xLogWarning "collected warnings ⌄"
    xLogWarning w
  pure res