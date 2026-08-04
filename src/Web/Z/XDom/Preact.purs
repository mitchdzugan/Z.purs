module Web.Z.XDom.Preact
  ( xDomRunWeb
  , xPreactHydrate
  , xWebApp
  ) where

import Z.XDom.Prelude as XD
import Web.Z.Web.DOM as DOM
import Web.Z.Prelude

foreign import js_renderIn :: XD.ReactEl -> Element -> Effect (Promise Unit)

xPreactHydrate :: forall x. Element -> XD.ReactEl -> EA JsError x #> Unit
xPreactHydrate d r = x RunEffPromise $ js_renderIn r d

xDomRunWeb :: forall x. XD.RDom (DOM.XWEB x) -> XD.RDom x
xDomRunWeb m = do
  r <- xAt @XD.XSelf_ Ask
  let runEls = \mm -> r.runEls $ DOM.runXWeb mm
  let runUnit = \mm -> r.runUnit $ DOM.runXWeb mm
  let
    runDisposable = \mm -> r.runDisposable do
      mmm <- DOM.runXWeb mm
      pure $ DOM.runXWeb mmm
  let ir = { runEls, runUnit, runDisposable }
  XD.xRawFragment $ runEls $ x ExecW $ xAt @XD.XSelf_ RunR ir m

type UrlState =
  { href :: Either { actual :: String, parsed :: String } String
  , url :: URL
  , titleOr_ :: Maybe String
  }

urlStateFromURL :: (URL -> Maybe String) -> URL -> UrlState
urlStateFromURL toTitleOr_ url =
  { href: Right $ urlToString url, url, titleOr_: toTitleOr_ url }

parsedHref :: UrlState -> String
parsedHref { href: Left { parsed } } = parsed
parsedHref { href: Right parsed } = parsed

updateUrlState :: (URL -> Maybe String) -> UrlState -> String -> UrlState
updateUrlState toTitleOr_ state s = onParse $ urlFromString s
  where
  onParse (Just url) = urlStateFromURL toTitleOr_ url
  onParse _ =
    { href: Left { actual: s, parsed: parsedHref state }
    , url: state.url
    , titleOr_: toTitleOr_ state.url
    }

xWebApp
  :: forall x
   . (URL -> Maybe String)
  -> (UrlState -> XD.XDom (XWEB x))
  -> XD.XDom (XWEB x)
xWebApp toTitleOr_ fx = do
  locUrl <- xLocationUrl
  let baseUrlState = urlStateFromURL toTitleOr_ locUrl
  let origin = urlOrigin locUrl
  XD.dwithNewState baseUrlState \url setUrl -> do
    xOut url
    XD.d.use1Eff do
      doc <- xDocument
      win <- xWindow
      let xUpUrl = xLocationUrl >>= x <<< setUrl <<< urlStateFromURL toTitleOr_
      let { pushState, popState } = eventType
      popOff' <- xAddEventListener popState win default \_ -> xUpUrl
      pushOff' <- xAddEventListener pushState win default \_ -> xUpUrl
      clickOff' <- xAddEventListener eventType.click doc default \e -> do
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
                let newUrl = updateUrlState toTitleOr_ url fullHref
                when (not (eq url newUrl)) do
                  xPushState href newUrl.titleOr_
                  x $ setUrl newUrl
      pure do
        x Impure popOff'
        x Impure pushOff'
        x Impure clickOff'
    fx url