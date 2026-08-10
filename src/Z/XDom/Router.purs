module Z.XDom.Router
  ( R
  , T
  , X
  , XDomRouteHref
  , XDomRouteHrefT
  , XDomRouteOrE
  , XDomRouteOrET
  , XDomRunRouter
  , XDomRunRouterT
  ) where

import Z.Prelude hiding (R, Run)

import Z.Prelude as Z
import Z.XDom.Core as XDom
import Z.XDom.UrlState as UrlSt

type T r = { routeSpec :: RouteDuplex' r, urlState :: UrlSt.T }
type R r = R' (T r)
type X r x = ("router" :: R r | x)

data XDomRunRouterT = XDomRunRouterT
type XDomRunRouter = Generable XDomRunRouterT

instance
  ( GOrDefault "router" gdesc p
  , Cons p (R r) x' x
  , IsSymbol p
  ) =>
  GenerableC
    XDomRunRouterT
    gdesc
    ( RouteDuplex' r
      -> ({ url :: URL, routeOrE :: Either RouteError r } -> Maybe String)
      -> XDom.RDom x
      -> UrlSt.RProvider x'
      -> XDom.RDom x'
    ) where
  mkGenerable routeSpec mkTitle m provider = do
    provider toTitleOr_ \urlState -> do
      g1 @XDom.XDomRunR @p { routeSpec, urlState } m
    where
    toTitleOr_ url = finTitle url $ routeParse routeSpec $ urlRelative url
    finTitle url routeOrE = mkTitle { url, routeOrE }

data XDomRouteOrET = XDomRouteOrET
type XDomRouteOrE = Generable XDomRouteOrET

instance
  ( GOrDefault "router" gdesc p
  , Cons p (R r) x' x
  , IsSymbol p
  ) =>
  GenerableC XDomRouteOrET gdesc (Z.Run x (Either RouteError r)) where
  mkGenerable = do
    r <- g1 @XAsk @p
    pure $ routeParse r.routeSpec $ urlRelative r.urlState.url

data XDomRouteHrefT = XDomRouteHrefT
type XDomRouteHref = Generable XDomRouteHrefT

instance
  ( GOrDefault "router" gdesc p
  , IsSymbol p
  , Cons p (R r) x' x
  , Cons p (R r) x_a (xProps :: W' (Array XDom.PropWF) | x)
  ) =>
  GenerableC XDomRouteHrefT
    gdesc
    (r -> Run' (xProps :: W' (Array XDom.PropWF) | x)) where
  mkGenerable route = do
    r <- g1 @XAsk @p
    g1 @XTell @XDom.XProps_ $ pure $ XDom.Href $ routePrint r.routeSpec
      route

