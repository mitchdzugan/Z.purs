module Web.Z.Web.DOM
  ( EventListenerOpts
  , T'_'x'web
  , X'Web
  , XWebR(..)
  , class IsEventTarget
  , evTarget
  , eventType
  , runXAThenExit
  , runXWeb
  , toEventTarget
  , xAddEventListener
  , xBody
  , xClosest
  , xDocument
  , xGetAttribute
  , xGetElementById
  , xHead
  , xLocationUrl
  , xPreventDefault
  , xPushState
  , xSetDocumentTitle
  , xStopPropagation
  , xWebR
  , xWindow
  ) where

import Z.Prelude

import Web.DOM.Element as Element
import Web.DOM.Internal.Types as T
import Web.DOM.NonElementParentNode as NEPN
import Web.DOM.ParentNode as PN
import Web.Event.Event as WebEvent
import Web.Event.EventTarget as WebEventT
import Web.Event.Internal.Types as WET
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.HTMLElement as HTMLEl
import Web.HTML.History as History
import Web.HTML.Location as Loc
import Web.HTML.Window as Window
import Z.Z.Future (future'run)

type T'_'x'web = "_'x'web"

type X'Web x = (_'x'web :: X'R XWebR | x)
newtype XWebR = XWebR
  { window :: Effect Window.Window
  , document :: Effect HTMLDoc.HTMLDocument
  , body :: Effect (Maybe T.Element)
  , head :: Effect (Maybe T.Element)
  , locationUrl :: Effect URL
  , getElementById :: String -> Effect (Maybe T.Element)
  , closest :: String /\ WET.EventTarget -> Effect (Maybe T.Element)
  , preventDefault :: WET.Event -> Effect Unit
  , stopPropagation :: WET.Event -> Effect Unit
  , pushState :: String /\ Maybe String -> Effect Unit
  , setDocumentTitle :: String -> Effect Unit
  , addEventListener ::
      WebEvent.EventType
        /\ WebEventT.EventTarget
        /\ EventListenerOpts
        /\ (WebEvent.Event -> Effect Unit)
      -> Effect WebEventT.EventListener
  , rmEventListener ::
      WebEvent.EventType
        /\ WebEventT.EventTarget
        /\ Boolean
        /\ WebEventT.EventListener
      -> Effect Unit
  , getAttribute :: String /\ T.Element -> Effect (Maybe String)
  , subToEvent ::
      WebEvent.EventType
        /\ WebEventT.EventTarget
        /\ Edit EventListenerOpts
        /\ (WebEvent.Event -> Effect Unit)
      -> Effect (Effect Unit)
  }

derive instance Newtype XWebR _

evTarget :: WET.Event -> Maybe WET.EventTarget
evTarget = WebEvent.target

class IsEventTarget a where
  toEventTarget :: a -> WebEventT.EventTarget

instance IsEventTarget HTMLDoc.HTMLDocument where
  toEventTarget = HTMLDoc.toEventTarget

instance IsEventTarget HTML.Window where
  toEventTarget = Window.toEventTarget

getElementById :: String -> HTMLDoc.HTMLDocument -> Effect (Maybe T.Element)
getElementById s = NEPN.getElementById s <<< HTMLDoc.toNonElementParentNode

getDocumentBody :: HTMLDoc.HTMLDocument -> Effect (Maybe T.Element)
getDocumentBody doc = HTMLDoc.body doc <#> map HTMLEl.toElement

getDocumentHead :: HTMLDoc.HTMLDocument -> Effect (Maybe T.Element)
getDocumentHead doc = HTMLDoc.head doc <#> map HTMLEl.toElement

xWindow :: forall x. Run (X'Web x) Window.Window
xWindow = x'effGet @T'_'x'web @"window"

xLocationUrl :: forall x. Run (X'Web x) URL
xLocationUrl = x'effGet @T'_'x'web @"locationUrl"

xDocument :: forall x. Run (X'Web x) HTMLDoc.HTMLDocument
xDocument = x'effGet @T'_'x'web @"document"

xBody :: forall x. Run (X'Web x) (Maybe T.Element)
xBody = x'effGet @T'_'x'web @"body"

xHead :: forall x. Run (X'Web x) (Maybe T.Element)
xHead = x'effGet @T'_'x'web @"head"

xGetElementById :: forall x. String -> Run (X'Web x) (Maybe T.Element)
xGetElementById s = x'effCall @T'_'x'web @"getElementById" s

xClosest
  :: forall x. WET.EventTarget -> String -> Run (X'Web x) (Maybe T.Element)
xClosest et qs = x'effCall @T'_'x'web @"closest" (qs /\ et)

xGetAttribute :: forall x. T.Element -> String -> Run (X'Web x) (Maybe String)
xGetAttribute el attr = x'effCall @T'_'x'web @"getAttribute" (attr /\ el)

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
  -> (WebEvent.Event -> Effect Unit)
  -> Run (X'Web x) (Run (X'Web x) Unit)
xAddEventListener eType target opts onE = do
  let o = edit defaultEventListenerOpts opts
  let tgt = toEventTarget target
  el <- x'effCall @T'_'x'web @"addEventListener" (eType /\ tgt /\ o /\ onE)
  pure $
    x'effCall @T'_'x'web @"rmEventListener" (eType /\ tgt /\ o.capture /\ el)

xPushState
  :: forall x
   . String
  -> Maybe String
  -> Run (X'Web x) Unit
xPushState url titleOr_ = do
  {-
    TODO
    TODO  use pushState with options supplied
    TODO
  let title = jOr' titleOr_
  let hasTitle = isJust titleOr_
  let opts = if hasTitle then (encodeForeign { title }) else (encodeForeign {})
  -}
  x'effCall @T'_'x'web @"pushState" (url /\ titleOr_)

xSetDocumentTitle
  :: forall x
   . String
  -> Run (X'Web x) Unit
xSetDocumentTitle title = do
  x'effCall @T'_'x'web @"setDocumentTitle" title

xPreventDefault :: forall x. WET.Event -> Run (X'Web x) Unit
xPreventDefault e = x'effCall @T'_'x'web @"preventDefault" e

xStopPropagation :: forall x. WET.Event -> Run (X'Web x) Unit
xStopPropagation e = x'effCall @T'_'x'web @"stopPropagation" e

raw_addEventListener
  :: WebEvent.EventType
  -> WebEventT.EventTarget
  -> EventListenerOpts
  -> (WebEvent.Event -> Effect Unit)
  -> Effect WebEventT.EventListener
raw_addEventListener et t o h = do
  el <- WebEventT.eventListener h
  WebEventT.addEventListenerWithOptions et el o t
  pure el

raw_rmEventListener
  :: WebEvent.EventType
  -> WebEventT.EventTarget
  -> Boolean
  -> WebEventT.EventListener
  -> Effect Unit
raw_rmEventListener et t c l = WebEventT.removeEventListener et l c t

raw_subToEvent
  :: WebEvent.EventType
  -> WebEventT.EventTarget
  -> Edit EventListenerOpts
  -> (WebEvent.Event -> Effect Unit)
  -> Effect (Effect Unit)
raw_subToEvent eType target opts onE = do
  let o = edit defaultEventListenerOpts opts
  let tgt = target
  el <- raw_addEventListener eType tgt o onE
  pure $ raw_rmEventListener eType tgt o.capture el

xWebR :: XWebR
xWebR = XWebR
  { window: HTML.window
  , document: HTML.window >>= Window.document
  , body: HTML.window >>= Window.document >>= getDocumentBody
  , head: HTML.window >>= Window.document >>= getDocumentHead
  , locationUrl: do
      win <- HTML.window
      loc <- Window.location win
      hashs <- Loc.hash loc
      pathname <- Loc.pathname loc
      search <- Loc.search loc
      host <- Loc.hostname loc
      protocol <- Loc.protocol loc
      ports <- Loc.port loc
      let port = tryParseInt ports
      pure $ url'fromParts
        { hash: if (eq hashs "") then Nothing else Just hashs
        , port
        , username: Nothing
        , password: Nothing
        , protocol
        , host
        , query: Left search
        , path: url'pathSegmentsFromPathname pathname
        }
  , getElementById: \id -> HTML.window >>= Window.document >>=
      getElementById id
  , closest: \(qs /\ et) -> do
      let orEl = Element.fromEventTarget et
      whenJust orEl $ Element.closest (PN.QuerySelector qs)
  , preventDefault: WebEvent.preventDefault
  , stopPropagation: WebEvent.stopPropagation
  , pushState: \(url /\ titleOr_) -> do
      let title = jOr' titleOr_
      let hasTitle = isJust titleOr_
      let
        opts =
          if hasTitle then (encodeForeign { title }) else (encodeForeign {})
      w <- HTML.window
      h <- Window.history w
      History.pushState opts (History.DocumentTitle title) (History.URL url) h
  , setDocumentTitle: \t -> do
      w <- HTML.window
      d <- Window.document w
      HTMLDoc.setTitle t d
  , addEventListener: \(et /\ t /\ o /\ h) -> raw_addEventListener et t o h
  , subToEvent: \(et /\ t /\ o /\ h) -> raw_subToEvent et t o h
  , rmEventListener: \(et /\ t /\ c /\ l) -> raw_rmEventListener et t c l
  , getAttribute: \(attr /\ el) -> Element.getAttribute attr el
  }

runXWeb :: forall x a. Run (X'Web x) a -> Run x a
runXWeb = x'eval @"_'x'web" @(X'R XWebR) xWebR

foreign import js_errorLog :: forall a. a -> Effect Unit

effAffThenExit :: forall e a. RtError e => Future (Either e a) -> Effect Unit
effAffThenExit = future'run onDone onError
  where
  onError e = do
    js_errorLog "process failed with UNHANDLED UNKNOWN error ⌄"
    js_errorLog e
  onDone (Left e) = do
    js_errorLog
      $ "process failed with known error [| "
      <> rtErrName e
      <> " |] ⌄"
    js_errorLog $ rtErrMessage e
  onDone _ = pure unit

runXAThenExit
  :: forall @w @e a. RtError e => (WaEA' w e (X'Web ()) @@> a) -> Effect Unit
runXAThenExit m = effAffThenExit $ async'x $ do
  res /\ w <- w'run $ e'try $ runXWeb m
  when (arr'size w > 0) do
    x'logWarning "collected warnings ⌄"
    x'logWarning w
  pure res
