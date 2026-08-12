module Z.XDom2.Router
  ( R
  , T
  , X
  , XDomRouteHref
  , XDomRouteOrE
  , XDomRunRouter
  , XDomUrlState(..)
  , class ConsR
  , router''href
  , router''routeOrE
  , router''run
  , router''urlState
  , router'href
  , router'routeOrE
  , router'run
  , router'urlState
  ) where

import Z.Prelude hiding (R, Run)

import Z.Prelude as Z
import Z.XDom2.Core as XDom
import Z.XDom2.Preact (PropWF(..))
import Z.XDom2.UrlState as UrlSt

type T e r =
  { print :: r -> String, routeOrE :: Either e r, urlState :: UrlSt.T }

type R e r = R' (T e r)
type X e r x = ("router" :: R e r | x)

data XDomRunRouter

instance
  ( GOrDefault "router" gdesc p
  , Cons p (R e r) sx' sx
  , IsSymbol p
  ) =>
  Generable
    XDomRunRouter
    gdesc
    ( (r -> String)
      -> (URL -> Either e r)
      -> ({ url :: URL, routeOrE :: Either e r } -> Maybe String)
      -> XDom.MDom sx de Unit
      -> UrlSt.RProvider sx' de
      -> XDom.MDom sx' de Unit
    ) where
  mkGenerable print parse mkTitle m provider = do
    provider toTitleOr_ \urlState -> do
      let routeOrE = parse urlState.url
      XDom.dom'runR @p { routeOrE, print, urlState } m
    where
    toTitleOr_ url = finTitle url $ parse url
    finTitle url routeOrE = mkTitle { url, routeOrE }

data XDomRouteOrE

class Cons p (R e r) x' x <= ConsR p e r x' x | p e r x' -> x

instance Cons p (R e r) x' x => ConsR p e r x' x

instance
  ( GOrDefault "router" gdesc p
  , ConsR p e r x' x
  , IsSymbol p
  ) =>
  Generable XDomRouteOrE gdesc (Z.Run x (Either e r)) where
  mkGenerable = g1 @XAsk @p <#> _.routeOrE

data XDomUrlState

instance
  ( GOrDefault "router" gdesc p
  , Cons p (R e r) x' x
  , IsSymbol p
  ) =>
  Generable XDomUrlState gdesc (Z.Run x UrlSt.T) where
  mkGenerable = g1 @XAsk @p <#> _.urlState

data XDomRouteHref

instance
  ( GOrDefault "router" gdesc p
  , IsSymbol p
  , Cons p (R e r) x' x
  , Cons p (R e r) x_a (attr :: W' (Array PropWF) | x)
  ) =>
  Generable XDomRouteHref
    gdesc
    (r -> Run' (attr :: W' (Array PropWF) | x)) where
  mkGenerable route = do
    href <- g1 @XAsk @p <#> _.print <#> (#) route
    w'tell @"attr" $ pure $ Href href

-------------------------------------------------------------------------------

router''href :: forall v. Generable XDomRouteHref GDefault v => v
router''href = g @XDomRouteHref

router'href :: forall @at v. Generable XDomRouteHref (G1 at) v => v
router'href = g1 @XDomRouteHref @at

router''urlState :: forall v. Generable XDomUrlState GDefault v => v
router''urlState = g @XDomUrlState

router'urlState :: forall @at v. Generable XDomUrlState (G1 at) v => v
router'urlState = g1 @XDomUrlState @at

router''routeOrE :: forall v. Generable XDomRouteOrE GDefault v => v
router''routeOrE = g @XDomRouteOrE

router'routeOrE :: forall @at v. Generable XDomRouteOrE (G1 at) v => v
router'routeOrE = g1 @XDomRouteOrE @at

router''run :: forall v. Generable XDomRunRouter GDefault v => v
router''run = g @XDomRunRouter

router'run :: forall @at v. Generable XDomRunRouter (G1 at) v => v
router'run = g1 @XDomRunRouter @at