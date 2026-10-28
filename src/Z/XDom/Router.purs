module Z.XDom.Router
  ( R
  , T
  , X
  , router'setHref
  , router'setHref''
  , router'mkRel
  , router'mkRel''
  , router'mkUrl
  , router'mkUrl''
  , router'routeOrE
  , router'routeOrE''
  , router'run
  , router'run''
  , router'urlState
  , router'urlState''
  ) where

import Z.Prelude hiding (Run)

import Z.Prelude as Z
import Z.XDom.Core as XDom
import Z.XDom.UrlState as UrlSt
import Z.XDom.X (AttrW', attrW'addAttr)

type T e r =
  { baseUrl :: URL
  , urlEdits :: r -> Edit UrlParts
  , routeOrE :: Either e r
  , urlState :: UrlSt.T
  }

type R e r = X'R (T e r)
type X e r x = ("router" :: R e r | x)

type T'router'run p =
  forall e r dr x x'
   . IsSymbol p
  => Cons p (R e r) x' x
  => (r -> Edit UrlParts)
  -> (URL -> Either e r)
  -> ({ url :: URL, routeOrE :: Either e r } -> Maybe String)
  -> XDom.MDom dr x Unit
  -> UrlSt.RProvider dr x'
  -> XDom.MDom dr x' Unit

router'run'' :: forall @p. T'router'run p
router'run'' urlEdits parse mkTitle m provider = do
  UrlSt.useProvider provider toTitleOr_ \baseUrl urlState -> do
    let routeOrE = parse urlState.url
    XDom.dom'runReader @p { routeOrE, urlEdits, baseUrl, urlState } m
  where
  toTitleOr_ url = finTitle url $ parse url
  finTitle url routeOrE = mkTitle { url, routeOrE }

router'run :: forall p. T'useAsSym "router" p T'router'run
router'run = router'run'' @p

type T'router'routeOrE p =
  forall e r x x'. IsSymbol p => Cons p (R e r) x' x => Z.Run x (Either e r)

router'routeOrE'' :: forall @p. T'router'routeOrE p
router'routeOrE'' = x'extract @p <#> _.routeOrE

router'routeOrE :: forall @p. T'useAsSym "router" p T'router'routeOrE
router'routeOrE = router'routeOrE'' @p

type T'router'urlState p =
  forall e r x x'. IsSymbol p => Cons p (R e r) x' x => Z.Run x UrlSt.T

router'urlState'' :: forall @p. T'router'urlState p
router'urlState'' = x'extract @p <#> _.urlState

router'urlState :: forall @p. T'useAsSym "router" p T'router'urlState
router'urlState = router'urlState'' @p

type T'router'mkUrl p =
  forall e r x x'. IsSymbol p => Cons p (R e r) x' x => r -> Z.Run x URL

router'mkUrl'' :: forall @p. T'router'mkUrl p
router'mkUrl'' route =
  x'extract @p <#> \r -> url'edit r.baseUrl $ r.urlEdits route

router'mkUrl :: forall @p. T'useAsSym "router" p T'router'mkUrl
router'mkUrl = router'mkUrl'' @p

type T'router'mkRel p =
  forall e r x x'. IsSymbol p => Cons p (R e r) x' x => r -> Z.Run x String

router'mkRel'' :: forall @p. T'router'mkRel p
router'mkRel'' route = router'mkUrl'' @p route <#> url'relative

router'mkRel :: forall @p. T'useAsSym "router" p T'router'mkRel
router'mkRel = router'mkRel'' @p

type T'router'setHref p =
  forall t e r x' x
   . IsSymbol p
  => Cons p (R e r)
       (AttrW' t x')
       (AttrW' t x)
  => r
  -> Z.Run (AttrW' t x) Unit

router'setHref'' :: forall @p. T'router'setHref p
router'setHref'' route = do
  href <- router'mkUrl'' @p route <#> url'relative
  attrW'addAttr "href" $ jsAny href

router'setHref :: forall @p. T'useAsSym "router" p T'router'setHref
router'setHref = router'setHref'' @p