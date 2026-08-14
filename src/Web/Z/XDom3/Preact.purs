module Web.Z.XDom3.Preact
  ( xDomRunWeb
  , xPreactHydrate
  , xProvideHistoryX
  ) where

import Web.Z.Prelude

import Web.Z.Web.DOM as DOM
import Z.XDom.UrlState as UrlSt
import Z.XDom3.Prelude as XD

foreign import js_renderIn :: XD.ReactEl -> Element -> Effect (Promise Unit)

xPreactHydrate :: forall x. Element -> XD.ReactEl -> EA JsError x #> Unit
xPreactHydrate d r = g @XRunEffPromise $ js_renderIn r d

xDomRunWeb
  :: forall dr x
   . XD.MDom dr (XWebV x) Unit
  -> XD.MDom dr x Unit
xDomRunWeb = XD.dom'withAdapter DOM.runXWeb

xProvideHistoryX
  :: forall dr x
   . (URL -> Maybe String)
  -> (UrlSt.T -> XD.MDom dr (XWebV x) Unit)
  -> XD.MDom dr (XWebV x) Unit
xProvideHistoryX toTitleOr_ fx = do
  locUrl <- DOM.xLocationUrl
  let baseUrlState = UrlSt.mk toTitleOr_ locUrl
  let origin = urlOrigin locUrl
  XD.dom'withNewState baseUrlState \st setSt -> do
    XD.domUseEff unit do
      doc <- DOM.xDocument
      win <- DOM.xWindow
      xUpUrl <- pure $ eff'do'' @"domEff" <<< setSt <<< UrlSt.mk toTitleOr_ =<<
        DOM.xLocationUrl
      let { pushState, popState, click } = eventType
      self <- XD.domEff'getSelf
      let pureRun = pure <<< XD.eval'self self
      d'pop <- DOM.xAddEventListener popState win pass \_ -> pureRun xUpUrl
      d'push <- DOM.xAddEventListener pushState win pass \_ -> pureRun xUpUrl
      d'click <- DOM.xAddEventListener click doc pass \e -> pureRun do
        let orTarget = evTarget e
        whenJust orTarget \target -> do
          orClosest <- DOM.xClosest target "a"
          whenJust orClosest \closest -> do
            orHref <- DOM.xGetAttribute closest "href"
            whenJust orHref \href -> do
              when (strStartsWith "/" href) do
                DOM.xPreventDefault e
                DOM.xStopPropagation e
                let fullHref = origin <> href
                let newUrl = UrlSt.update toTitleOr_ st fullHref
                when (not (eq st newUrl)) do
                  DOM.xPushState href newUrl.titleOr_
                  whenJust newUrl.titleOr_ DOM.xSetDocumentTitle
                  eff'do'' @"domEff" $ setSt newUrl
      pure do
        d'pop
        d'push
        d'click
    fx st
