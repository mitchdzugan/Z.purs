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
xPreactHydrate d r = g @XRunEffPromise $ js_renderIn r d

xDomRunWeb
  :: forall sx de
   . Lacks "xWeb" de
  => XD.MDom sx { xWeb :: XWebR | de } Unit
  -> XD.MDom sx { | de } Unit
xDomRunWeb = XD.domEffR'run @"xWeb" DOM.xWebR

xProvideHistoryX
  :: forall sx de
   . (URL -> Maybe String)
  -> (UrlSt.T -> XD.MDom (sx) { xWeb :: XWebR | de } Unit)
  -> XD.MDom (sx) { xWeb :: XWebR | de } Unit
xProvideHistoryX toTitleOr_ fx = do
  locUrl <- XD.domEffR'act @"xWeb" _.locationUrl
  let baseUrlState = UrlSt.mk toTitleOr_ locUrl
  let origin = urlOrigin locUrl
  XD.dom.withNewState baseUrlState \st setSt -> do
    XD.domUseEff unit do
      let actWeb = XD.domEffR'act @"xWeb"
      doc <- actWeb _.document
      win <- actWeb _.window
      xUpUrl <- pure $ XD.dom.doEff <<< setSt <<< UrlSt.mk toTitleOr_ =<<
        actWeb _.locationUrl
      let { pushState, popState } = eventType
      runner <- XD.domEffR'encapsulate
      d'pop <- actWeb \r -> r.subToEvent popState
        (toEventTarget win)
        pass
        \_ -> pure $ runner xUpUrl
      d'push <- actWeb \r -> r.subToEvent pushState
        (toEventTarget win)
        pass
        \_ -> pure $ runner xUpUrl
      d'click <- actWeb \r -> r.subToEvent eventType.click
        (toEventTarget doc)
        pass
        \e -> pure $ runner do
          let orTarget = evTarget e
          whenJust orTarget \target -> do
            orClosest <- actWeb \r -> r.closest "a" target
            whenJust orClosest \closest -> do
              orHref <- actWeb \r -> r.getAttribute "href" closest
              whenJust orHref \href -> do
                when (strStartsWith "/" href) do
                  actWeb \r -> r.preventDefault e
                  actWeb \r -> r.stopPropagation e
                  let fullHref = origin <> href
                  let newUrl = UrlSt.update toTitleOr_ st fullHref
                  when (not (eq st newUrl)) do
                    actWeb \r -> r.pushState href newUrl.titleOr_
                    xOut newUrl.titleOr_
                    whenJust newUrl.titleOr_ $ \s -> actWeb \r ->
                      r.setDocumentTitle s
                    XD.dom.doEff $ setSt newUrl
      traceM "IN PURE ON"
      pure do
        traceM "IN PURE OFF"
        XD.domEffR'act @"xWeb" \_ -> d'pop
        XD.domEffR'act @"xWeb" \_ -> d'push
        XD.domEffR'act @"xWeb" \_ -> d'click

    fx st
