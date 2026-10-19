module Web.Z.Web.DOM
  ( EventListenerOpts
  , X'Web
  , XEffWeb
  , XWebR
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
import Z.Z.Promise (future'run)

type XEffWeb a = Eff'At "_'x'web" a
type X'Web x = (_'x'web :: X'R XWebR | x)
type XWebR =
  { window :: XEffWeb Window.Window
  , document :: XEffWeb HTMLDoc.HTMLDocument
  , body :: XEffWeb (Maybe T.Element)
  , head :: XEffWeb (Maybe T.Element)
  , locationUrl :: XEffWeb URL
  , getElementById :: String -> XEffWeb (Maybe T.Element)
  , closest :: String -> WET.EventTarget -> XEffWeb (Maybe T.Element)
  , preventDefault :: WET.Event -> XEffWeb Unit
  , stopPropagation :: WET.Event -> XEffWeb Unit
  , pushState :: String -> Maybe String -> XEffWeb Unit
  , setDocumentTitle :: String -> XEffWeb Unit
  , addEventListener ::
      WebEvent.EventType
      -> WebEventT.EventTarget
      -> EventListenerOpts
      -> (WebEvent.Event -> Effect Unit)
      -> XEffWeb WebEventT.EventListener
  , rmEventListener ::
      WebEvent.EventType
      -> WebEventT.EventTarget
      -> Boolean
      -> WebEventT.EventListener
      -> XEffWeb Unit
  , getAttribute :: String -> T.Element -> XEffWeb (Maybe String)
  , subToEvent ::
      WebEvent.EventType
      -> WebEventT.EventTarget
      -> Edit EventListenerOpts
      -> (WebEvent.Event -> Effect Unit)
      -> XEffWeb (XEffWeb Unit)
  }

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
xWindow = x'act @"_'x'web" _.window

xLocationUrl :: forall x. Run (X'Web x) URL
xLocationUrl = x'act @"_'x'web" \r -> r.locationUrl

xDocument :: forall x. Run (X'Web x) HTMLDoc.HTMLDocument
xDocument = x'act @"_'x'web" \r -> r.document

xBody :: forall x. Run (X'Web x) (Maybe T.Element)
xBody = x'act @"_'x'web" \r -> r.body

xHead :: forall x. Run (X'Web x) (Maybe T.Element)
xHead = x'act @"_'x'web" \r -> r.head

xGetElementById :: forall x. String -> Run (X'Web x) (Maybe T.Element)
xGetElementById s = x'act @"_'x'web" \r -> r.getElementById s

xClosest
  :: forall x. WET.EventTarget -> String -> Run (X'Web x) (Maybe T.Element)
xClosest et qs = x'act @"_'x'web" \r -> r.closest qs et

xGetAttribute :: forall x. T.Element -> String -> Run (X'Web x) (Maybe String)
xGetAttribute el attr = x'act @"_'x'web" \r -> r.getAttribute attr el

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
  el <- x'act @"_'x'web" \r -> r.addEventListener eType tgt o onE
  pure $ x'act @"_'x'web" \r -> r.rmEventListener eType tgt o.capture el

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
  x'act @"_'x'web" \r -> r.pushState url titleOr_

xSetDocumentTitle
  :: forall x
   . String
  -> Run (X'Web x) Unit
xSetDocumentTitle title = do
  x'act @"_'x'web" \r -> r.setDocumentTitle title

xPreventDefault :: forall x. WET.Event -> Run (X'Web x) Unit
xPreventDefault e = x'act @"_'x'web" \r -> r.preventDefault e

xStopPropagation :: forall x. WET.Event -> Run (X'Web x) Unit
xStopPropagation e = x'act @"_'x'web" \r -> r.stopPropagation e

tagEffWebX :: forall a. Effect a -> Eff'At "_'x'web" a
tagEffWebX = eff'tag @"_'x'web"

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
  -> Effect (XEffWeb Unit)
raw_subToEvent eType target opts onE = do
  let o = edit defaultEventListenerOpts opts
  let tgt = target
  el <- raw_addEventListener eType tgt o onE
  pure $ tagEffWebX $ raw_rmEventListener eType tgt o.capture el

xWebR :: XWebR
xWebR =
  { window: tagEffWebX HTML.window
  , document: tagEffWebX $ HTML.window >>= Window.document
  , body: tagEffWebX $ HTML.window >>= Window.document >>= getDocumentBody
  , head: tagEffWebX $ HTML.window >>= Window.document >>= getDocumentHead
  , locationUrl: tagEffWebX do
      win <- HTML.window
      loc <- Window.location win
      hashs <- Loc.hash loc
      pathname <- Loc.pathname loc
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
        , query: hm'empty
        , path: url'pathSegmentsFromPathname pathname
        }
  , getElementById: \id -> tagEffWebX $ HTML.window >>= Window.document >>=
      getElementById id
  , closest: \qs et -> tagEffWebX do
      let orEl = Element.fromEventTarget et
      whenJust orEl $ Element.closest (PN.QuerySelector qs)
  , preventDefault: tagEffWebX <<< WebEvent.preventDefault
  , stopPropagation: tagEffWebX <<< WebEvent.stopPropagation
  , pushState: \url titleOr_ -> tagEffWebX do
      let title = jOr' titleOr_
      let hasTitle = isJust titleOr_
      let
        opts =
          if hasTitle then (encodeForeign { title }) else (encodeForeign {})
      w <- HTML.window
      h <- Window.history w
      History.pushState opts (History.DocumentTitle title) (History.URL url) h
  , setDocumentTitle: \t -> tagEffWebX do
      w <- HTML.window
      d <- Window.document w
      HTMLDoc.setTitle t d
  , addEventListener: \et t o h -> tagEffWebX $ raw_addEventListener et t o h
  , subToEvent: \et t o h -> tagEffWebX $ raw_subToEvent et t o h
  , rmEventListener: \et t c l -> tagEffWebX $ raw_rmEventListener et t c l
  , getAttribute: \attr el -> tagEffWebX $ Element.getAttribute attr el
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
