module Z.XDom3.Router
  ( R
  , T
  , X
  , router'href
  , router'href''
  , router'routeOrE
  , router'routeOrE''
  , router'run
  , router'run''
  , router'urlState
  , router'urlState''
  ) where

import Z.Prelude hiding (R, Run)

import Z.Prelude as Z
import Z.XDom3.Core as XDom
import Z.XDom3.Preact (PropWF(..))
import Z.XDom3.UrlState as UrlSt

type T e r =
  { print :: r -> String, routeOrE :: Either e r, urlState :: UrlSt.T }

type R e r = R' (T e r)
type X e r x = ("router" :: R e r | x)

type T'router'run p =
  forall e r dr x x'
   . IsSymbol p
  => Cons p (R e r) x' x
  => (r -> String)
  -> (URL -> Either e r)
  -> ({ url :: URL, routeOrE :: Either e r } -> Maybe String)
  -> XDom.MDom dr x Unit
  -> UrlSt.RProvider dr x'
  -> XDom.MDom dr x' Unit

router'run'' :: forall @p. T'router'run p
router'run'' print parse mkTitle m provider = do
  provider toTitleOr_ \urlState -> do
    let routeOrE = parse urlState.url
    XDom.domR'run'' @p { routeOrE, print, urlState } m
  where
  toTitleOr_ url = finTitle url $ parse url
  finTitle url routeOrE = mkTitle { url, routeOrE }

router'run :: forall p. T'useAsSym "router" p T'router'run
router'run = router'run'' @p

type T'router'routeOrE p =
  forall e r x x'. IsSymbol p => Cons p (R e r) x' x => Z.Run x (Either e r)

router'routeOrE'' :: forall @p. T'router'routeOrE p
router'routeOrE'' = r'ask'' @p <#> _.routeOrE

router'routeOrE :: forall @p. T'useAsSym "router" p T'router'routeOrE
router'routeOrE = router'routeOrE'' @p

type T'router'urlState p =
  forall e r x x'. IsSymbol p => Cons p (R e r) x' x => Z.Run x UrlSt.T

router'urlState'' :: forall @p. T'router'urlState p
router'urlState'' = r'ask'' @p <#> _.urlState

router'urlState :: forall @p. T'useAsSym "router" p T'router'urlState
router'urlState = router'urlState'' @p

type T'router'href p =
  forall e r x'' x' x
   . IsSymbol p
  => Cons p (R e r) x'' x
  => Cons p (R e r) x' (attr :: W' (Array PropWF) | x)
  => r
  -> Z.Run (attr :: W' (Array PropWF) | x) Unit

router'href'' :: forall @p. T'router'href p
router'href'' route = do
  href <- g1 @XAsk @p <#> _.print <#> (#) route
  w'tell'' @"attr" $ pure $ Href href

router'href :: forall @p. T'useAsSym "router" p T'router'href
router'href = router'href'' @p
