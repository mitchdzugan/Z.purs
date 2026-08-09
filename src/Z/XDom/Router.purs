module Z.XDom.Router
  ( R
  , T
  , X
  , XDomHrefAttrT
  , XDomRouteHref
  , XDomRouteOrE
  , XDomRouteOrET
  , XDomRunRouter
  , XDomRunRouterT
  ) where

import Z.Prelude hiding (Get(..), R, Run, Set(..))

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
      z @(XDom.XDomRunR @@ G1 p) { routeSpec, urlState } m
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
    r <- mkDimAt @p @Ask
    pure $ routeParse r.routeSpec $ urlRelative r.urlState.url

data XDomHrefAttrT = XDomHrefAttrT
type XDomRouteHref = Generable XDomHrefAttrT

instance
  ( GOrDefault "router" gdesc p
  , IsSymbol p
  , Cons p (R r) x' x
  , Cons p (R r) x_a (xProps :: W' (Array XDom.PropWF) | x)
  ) =>
  GenerableC XDomHrefAttrT
    gdesc
    (r -> Run' (xProps :: W' (Array XDom.PropWF) | x)) where
  mkGenerable route = do
    r <- z @(XAsk @@ G1 p)
    mkDimAt @XDom.XProps_ @Tell $ pure $ XDom.Href $ routePrint r.routeSpec
      route

