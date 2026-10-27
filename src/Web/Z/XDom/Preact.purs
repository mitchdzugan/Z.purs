module Web.Z.XDom.Preact
  ( exec'xdom'web
  , xDomRunWeb
  , xPreactHydrate
  , xweb'historyProvider
  ) where

import Web.Z.Prelude

import Debug (traceM)
import Web.DOM.DOMTokenList as CL
import Web.DOM.Element (classList, toNode)
import Web.DOM.Node (parentElement)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types as WET
import Web.Z.Web.DOM as DOM
import Z.XDom.Preact (DomEl, DomEvent)
import Z.XDom.Prelude as XD
import Z.XDom.UrlState as UrlSt

foreign import js_renderIn :: XD.ReactEl -> Element -> Effect (Promise Unit)

foreign import js_el_webToG :: Element -> DomEl
foreign import js_el_gToWeb :: DomEl -> Element

foreign import js_event_webToG :: WET.Event -> DomEvent
foreign import js_event_gToWeb :: DomEvent -> WET.Event

foreign import js_animationFramePromise :: Effect (Promise Unit)

xPreactHydrate :: forall x. Element -> XD.ReactEl -> EA' JsError x @@> Unit
xPreactHydrate d r = e'runEffPromise $ js_renderIn r d

xDomRunWeb :: forall dr x. XD.MDom dr (X'Web x) Unit -> XD.MDom dr x Unit
xDomRunWeb = XD.dom'withAdapter DOM.runXWeb

xweb'historyProvider :: forall dr x. UrlSt.RProvider dr (X'Web x)
xweb'historyProvider = UrlSt.mkProvider DOM.xLocationUrl provideHistory

provideHistory
  :: forall dr x
   . URL
  -> (URL -> Maybe String)
  -> (UrlSt.T -> XD.MDom dr (X'Web x) Unit)
  -> XD.MDom dr (X'Web x) Unit
provideHistory locUrl toTitleOr_ urlStateToDom = do
  traceM { locUrl }
  let baseUrlState = UrlSt.mk toTitleOr_ locUrl
  let origin = url'origin locUrl
  XD.dom'withNewState baseUrlState \urlState setUrlState -> do
    XD.domEff'once do
      doc <- DOM.xDocument
      win <- DOM.xWindow
      let mkUrlState = UrlSt.mk toTitleOr_ <$> DOM.xLocationUrl
      let xUpUrl = XD.domEff'do <<< setUrlState =<< mkUrlState
      let { pushState, popState, click } = eventType
      runner <- XD.dom'getRunner
      let deferring = runner'mkDeferred runner
      d'pop <- DOM.xAddEventListener popState win pass \_ -> deferring xUpUrl
      d'push <- DOM.xAddEventListener pushState win pass \_ -> deferring xUpUrl
      d'click <- DOM.xAddEventListener click doc pass \e -> deferring do
        whenJust (evTarget e) \target -> do
          DOM.xClosest target "a" >>= flip whenJust \closest -> do
            DOM.xGetAttribute closest "href" >>= flip whenJust \href -> do
              when (str'startsWith "/" href) do
                DOM.xPreventDefault e
                DOM.xStopPropagation e
                let fullHref = origin <> href
                let newUrl = UrlSt.update toTitleOr_ urlState fullHref
                whenNot (urlState == newUrl) do
                  DOM.xPushState href newUrl.titleOr_
                  whenJust newUrl.titleOr_ DOM.xSetDocumentTitle
                  XD.domEff'do $ setUrlState newUrl
      pure $ d'pop *> d'push *> d'click
    urlStateToDom urlState

exec'xdom'web :: XD.XDom () () Unit -> XD.ReactEl
exec'xdom'web = XD.exec'xdom
  { el'classList'add: \el cn -> classList (js_el_gToWeb el) >>= flip CL.add cn
  , el'classList'rm: \el cn -> classList (js_el_gToWeb el) >>= flip CL.remove cn
  , el'parentElement: \el ->
      js_el_gToWeb el # toNode # parentElement <#> map js_el_webToG
  , event'preventDefault: \event -> preventDefault $ js_event_gToWeb event
  , future'animationFrame:
      eff'defer \_ -> future'fromEffPromise js_animationFramePromise
  }