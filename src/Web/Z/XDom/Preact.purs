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

xDomRunWeb :: forall x. XD.RDom (XWebV x) -> XD.RDom x
xDomRunWeb m = do
  ir <- XD.xSelfExtendX' DOM.runXWeb
  XD.xRawFragment $ XD.runEls ir $ g @XExecW $ g1 @XRunR @XD.XSelf_ ir m

{-}
xDomAddEventListener
  :: forall x t
   . IsEventTarget t
  => WebEventType
  -> t
  -> Edit EventListenerOpts
  -> (WebEvent -> Run (XWebV (xDomEff :: XRunsEffTagged | x)) Unit)
  -> Run (XD.XDOMEFF (XWebV x))
       ( Run (XD.XDOMEFF (XWebV x))
           Unit
       )
xDomAddEventListener et t opts h = do
  rn <- g1 @XAsk @XD.XSelf_
  xAddEventListener et t opts (pure <<< XD.runUnit rn <<< h)
  -}

xProvideHistoryX
  :: forall x
   . (URL -> Maybe String)
  -> (UrlSt.T -> XD.RDom (XWebV x))
  -> XD.RDom (XWebV x)
xProvideHistoryX toTitleOr_ fx = do
  locUrl <- xLocationUrl
  let baseUrlState = UrlSt.mk toTitleOr_ locUrl
  let origin = urlOrigin locUrl
  XD.dwithNewState baseUrlState \st setSt -> do
    XD.d.use1Eff do
      doc <- xDocument
      win <- xWindow
      let xUpUrl = XD.xDomDo <<< setSt <<< UrlSt.mk toTitleOr_ =<< xLocationUrl
      let { pushState, popState } = eventType
      -- d'pop <- xDomAddEventListener popState win pass \_ -> xUpUrl
      pure $ pure unit
    {-
    XD.d.use1Eff do
      doc <- xDocument
      win <- xWindow
      -- let xDomDo = g1 @XRunTaggable @"xDomEff" <<< XD.xDomDo
      let xUpUrl = XD.xDomDo <<< setSt <<< UrlSt.mk toTitleOr_ =<< xLocationUrl
      let { pushState, popState } = eventType
      d'pop <- xDomAddEventListener popState win pass \_ -> xUpUrl
      d'push <- xDomAddEventListener pushState win pass \_ -> xUpUrl
      d'click <- xAddEventListener eventType.click doc pass \e -> do
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
                  xDomDo $ setSt newUrl -}
    -- pure $ d'pop *> d'push -- *> d'click
    fx st
