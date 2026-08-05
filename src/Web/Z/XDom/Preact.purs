module Web.Z.XDom.Preact
  ( xDomRunWeb
  , xPreactHydrate
  , xProvideHistoryX
  ) where

import Z.XDom.Prelude as XD
import Z.XDom.UrlState as UrlSt
import Web.Z.Web.DOM as DOM
import Web.Z.Prelude

foreign import js_renderIn :: XD.ReactEl -> Element -> Effect (Promise Unit)

xPreactHydrate :: forall x. Element -> XD.ReactEl -> EA JsError x #> Unit
xPreactHydrate d r = x' @"runEffPromise" $ js_renderIn r d

xDomRunWeb :: forall x. XD.RDom (DOM.XWEB x) -> XD.RDom x
xDomRunWeb m = do
  r <- x @XD.XSelf_ @"ask"
  let runEls = \mm -> r.runEls $ DOM.runXWeb mm
  let runUnit = \mm -> r.runUnit $ DOM.runXWeb mm
  let
    runDisposable = \mm -> r.runDisposable do
      mmm <- DOM.runXWeb mm
      pure $ DOM.runXWeb mmm
  let ir = { runEls, runUnit, runDisposable }
  XD.xRawFragment $ runEls $ x' @"execW" $ x @XD.XSelf_ @"runR" ir m

xProvideHistoryX
  :: forall x
   . (URL -> Maybe String)
  -> (UrlSt.T -> XD.XDom (XWEB x))
  -> XD.XDom (XWEB x)
xProvideHistoryX toTitleOr_ fx = do
  locUrl <- xLocationUrl
  let baseUrlState = UrlSt.mk toTitleOr_ locUrl
  let origin = urlOrigin locUrl
  XD.dwithNewState baseUrlState \st setSt -> do
    XD.d.use1Eff do
      doc <- xDocument
      win <- xWindow
      let xUpUrl = xLocationUrl >>= x' @"$" <<< setSt <<< UrlSt.mk toTitleOr_
      let { pushState, popState } = eventType
      iPopOff <- xAddEventListener popState win default \_ -> xUpUrl
      iPushOff <- xAddEventListener pushState win default \_ -> xUpUrl
      iClickOff <- xAddEventListener eventType.click doc default \e -> do
        let orTarget = evTarget e
        whenJust orTarget \target -> do
          orClosest <- xClosest target "a"
          whenJust orClosest \closest -> do
            orHref <- xGetAttribute closest "href"
            whenJust orHref \href -> do
              when (strStartsWith "/" href) do
                xPreventDefault e
                xStopPropagation e
                let fullHref = origin <> href
                let newUrl = UrlSt.update toTitleOr_ st fullHref
                when (not (eq st newUrl)) do
                  xPushState href newUrl.titleOr_
                  xOut newUrl.titleOr_
                  whenJust newUrl.titleOr_ xSetDocumentTitle
                  x' @"$" $ setSt newUrl
      pure $ xImpure iPopOff *> xImpure iPushOff *> xImpure iClickOff
    fx st
