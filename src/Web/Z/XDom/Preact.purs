module Web.Z.XDom.Preact
  ( xDomRunWeb
  , xPreactHydrate
  , xProvideHistoryX
  ) where

import Web.Z.Prelude

import Debug (traceM)
import Web.Z.Web.DOM as DOM
import Z.XDom.Prelude as XD
import Z.XDom.UrlState as UrlSt

foreign import js_renderIn :: XD.ReactEl -> Element -> Effect (Promise Unit)

xPreactHydrate :: forall x. Element -> XD.ReactEl -> EA JsError x #> Unit
xPreactHydrate d r = x' @"runEffPromise" $ js_renderIn r d

xDomRunWeb :: forall x. XD.RDom (DOM.XWEB x) -> XD.RDom x
xDomRunWeb m = do
  r <- mkDimAt @XD.XSelf_ @Ask
  let
    ir = XD.extXSelf @(XD.IdS) r ((<$>) XD.IdS <<< DOM.runXWeb) XD.unId XD.unId
      XD.unId
  XD.xRawFragment $ XD.runEls ir $ x' @"execW" $ x @XD.XSelf_ @"runR" ir m

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
      xOut "xout A"
      let
        res = pure $ do
          traceM "IN PURE RETURN"
          xOut "xout C"
          xPass *> iPopOff *> iPushOff *> iClickOff
      xOut "xout B"
      res
    fx st
