module Web.Z.Web.DOM
  ( EventListenerOpts
  , XEffWeb
  , XWeb
  , XWebR
  , XWebV
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
  , xWebR
  , xWindow
  ) where

import Z.Prelude

import Debug (traceM)
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

type XEffWeb a = Eff'At "xWeb" a
type XWebV x = (xWeb :: R' XWebR | x)
type XWeb x a = XRun (XWebV x) a
type RWeb x a = Run (XWebV x) a
type XWebEA e x = EA e (XWebV x)
type XWebR =
  { window :: XEffWeb Window.Window
  , document :: XEffWeb HTMLDoc.HTMLDocument
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
      -> (WebEvent.Event -> Unit)
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
      -> (WebEvent.Event -> Run () Unit)
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

xWindow :: forall x. RWeb x Window.Window
xWindow = r'act'' @"xWeb" _.window

xLocationUrl :: forall x. RWeb x URL
xLocationUrl = r'act'' @"xWeb" \r -> r.locationUrl

xDocument :: forall x. RWeb x HTMLDoc.HTMLDocument
xDocument = r'act'' @"xWeb" \r -> r.document

xGetElementById :: forall x. String -> RWeb x (Maybe T.Element)
xGetElementById s = r'act'' @"xWeb" \r -> r.getElementById s

xClosest :: forall x. WET.EventTarget -> String -> RWeb x (Maybe T.Element)
xClosest et qs = r'act'' @"xWeb" \r -> r.closest qs et

xGetAttribute :: forall x. T.Element -> String -> RWeb x (Maybe String)
xGetAttribute el attr = r'act'' @"xWeb" \r -> r.getAttribute attr el

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
  -> (WebEvent.Event -> Deferred Unit)
  -> RWeb x (RWeb x Unit)
xAddEventListener eType target opts onE = do
  let o = edit defaultEventListenerOpts opts
  let tgt = toEventTarget target
  let evalEvent = deferred'run <<< onE
  el <- r'act'' @"xWeb" \r -> r.addEventListener eType tgt o evalEvent
  pure $ r'act'' @"xWeb" \r -> r.rmEventListener eType tgt o.capture el

xPushState
  :: forall x
   . String
  -> Maybe String
  -> RWeb x Unit
xPushState url titleOr_ = do
  let title = jOr' titleOr_
  let hasTitle = isJust titleOr_
  let opts = if hasTitle then (encodeForeign { title }) else (encodeForeign {})
  r'act'' @"xWeb" \r -> r.pushState url titleOr_

xSetDocumentTitle
  :: forall x
   . String
  -> RWeb x Unit
xSetDocumentTitle title = do
  r'act'' @"xWeb" \r -> r.setDocumentTitle title

xPreventDefault :: forall x. WET.Event -> RWeb x Unit
xPreventDefault e = r'act'' @"xWeb" \r -> r.preventDefault e

xStopPropagation :: forall x. WET.Event -> RWeb x Unit
xStopPropagation e = r'act'' @"xWeb" \r -> r.stopPropagation e

tagEffWebX :: forall a. Effect a -> Eff'At "xWeb" a
tagEffWebX = eff'tag @"xWeb"

raw_addEventListener
  :: WebEvent.EventType
  -> WebEventT.EventTarget
  -> EventListenerOpts
  -> (WebEvent.Event -> Unit)
  -> Effect WebEventT.EventListener
raw_addEventListener et t o h = do
  el <- WebEventT.eventListener \e -> pure $ h e
  WebEventT.addEventListenerWithOptions et el o t
  pure el

raw_rmEventListener
  :: WebEvent.EventType
  -> WebEventT.EventTarget
  -> Boolean
  -> WebEventT.EventListener
  -> Effect Unit
raw_rmEventListener et t c l = do
  traceM "removing event..."
  traceM { et, l, c, t }
  WebEventT.removeEventListener et l c t

raw_subToEvent
  :: WebEvent.EventType
  -> WebEventT.EventTarget
  -> Edit EventListenerOpts
  -> (WebEvent.Event -> Run () Unit)
  -> Effect (XEffWeb Unit)
raw_subToEvent eType target opts onE = do
  let o = edit defaultEventListenerOpts opts
  let tgt = target
  let evalEvent = eval_ <<< onE
  el <- raw_addEventListener eType tgt o evalEvent
  pure $ tagEffWebX $ raw_rmEventListener eType tgt o.capture el

xWebR :: XWebR
xWebR =
  { window: tagEffWebX HTML.window
  , document: tagEffWebX $ HTML.window >>= Window.document
  , locationUrl: tagEffWebX do
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
        , query: map'empty
        , path: urlPathFromString pathname
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

runXWeb :: forall r. Run (XWebV + r) ~> Run r
runXWeb = g1 @XRunR @"xWeb" xWebR

type XWebRunner = forall a. XWeb () a -> a

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

runXAThenExit
  :: forall @w @e a. RtError e => XRunWA w (XWebEA e) a -> Effect Unit
runXAThenExit m = effAffThenExit $ runXA $ do
  w /\ res <- g @XRunW $ expand $ runXWeb m
  when (arr'size w > 0) do
    xLogWarning "collected warnings ⌄"
    xLogWarning w
  pure res
