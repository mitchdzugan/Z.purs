module Z.XDom.Router
  ( HrefAttr(..)
  , R
  , RouteOrE(..)
  , Run(..)
  , T
  , X
  , class XRouterP
  ) where

import Z.Prelude hiding (Run, Get(..), Set(..), R)

import Z.Prelude as Z
import Z.XDom.Preact as XDom
import Z.XDom.UrlState as UrlSt

type T r = { routeSpec :: RouteDuplex' r, urlState :: UrlSt.T }
type R r = R' (T r)
type X r x = ("router" :: R r | x)

class XRouterP :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Constraint
class XRouterP rp wp sp ep fp | rp wp sp ep -> fp

instance XRouterP "reader" "writer" "state" "except" "router"
else instance XRouterP rp _w _s _e rp

data Run = Run

instance Cons0 Run where
  cons0 = Run

instance
  ( XRouterP ep wp sp rp p
  , Cons p (R r) x' x
  , IsSymbol p
  ) =>
  RWSEFn Run
    ep
    wp
    sp
    rp
    ( RouteDuplex' r
      -> ({ url :: URL, routeOrE :: Either RouteError r } -> Maybe String)
      -> XDom.RDom x
      -> UrlSt.RProvider x'
      -> XDom.RDom x'
    ) where
  rwseApply _ _ _ _ _ routeSpec mkTitle m provider = do
    provider toTitleOr_ \urlState -> do
      x @p @"$" XDom.DomRunR { routeSpec, urlState } m
    where
    toTitleOr_ url = finTitle url $ routeParse routeSpec $ urlRelative url
    finTitle url routeOrE = mkTitle { url, routeOrE }

data RouteOrE = RouteOrE

instance Cons0 RouteOrE where
  cons0 = RouteOrE

instance
  ( XRouterP ep wp sp rp p
  , Cons p (R r) x' x
  , IsSymbol p
  ) =>
  RWSEFn RouteOrE ep wp sp rp (Z.Run x (Either RouteError r)) where
  rwseApply _ _ _ _ _ = do
    r <- x @p @"ask"
    pure $ routeParse r.routeSpec $ urlRelative r.urlState.url

data HrefAttr = HrefAttr

instance Cons0 HrefAttr where
  cons0 = HrefAttr

instance
  ( XRouterP ep wp sp rp p
  , Cons p (R r) x' x
  , Cons pp (W' (Array XDom.PropWF)) x'' x
  , IsSymbol p
  , IsSymbol pp
  , TypeEquals pp XDom.XProps_
  ) =>
  RWSEFn HrefAttr ep wp sp rp (r -> Run' x) where
  rwseApply _ _ _ _ _ route = do
    r <- x @p @"ask"
    x @pp @"tell" $ pure $ XDom.Href $ routePrint r.routeSpec route
