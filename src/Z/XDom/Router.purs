module Z.XDom.Router
  ( R
  , T
  , X
  , XDomRouteHref
  , XDomRouteOrE
  , XDomRunRouter
  , XDomUrlState(..)
  , class ConsR
  ) where

import Z.Prelude hiding (R, Run)

import Z.Prelude as Z
import Z.XDom.Core as XDom
import Z.XDom.UrlState as UrlSt

type T e r =
  { print :: r -> String, routeOrE :: Either e r, urlState :: UrlSt.T }

type R e r = R' (T e r)
type X e r x = ("router" :: R e r | x)

data XDomRunRouter

instance
  ( GOrDefault "router" gdesc p
  , Cons p (R e r) x' x
  , IsSymbol p
  ) =>
  Generable
    XDomRunRouter
    gdesc
    ( (r -> String)
      -> (URL -> Either e r)
      -> ({ url :: URL, routeOrE :: Either e r } -> Maybe String)
      -> XDom.RDom x
      -> UrlSt.RProvider x'
      -> XDom.RDom x'
    ) where
  mkGenerable print parse mkTitle m provider = do
    provider toTitleOr_ \urlState -> do
      let routeOrE = parse urlState.url
      g1 @XDom.XDomRunR @p { routeOrE, print, urlState } m
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
  , Cons p (R e r) x_a (xProps :: W' (Array XDom.PropWF) | x)
  ) =>
  Generable XDomRouteHref
    gdesc
    (r -> Run' (xProps :: W' (Array XDom.PropWF) | x)) where
  mkGenerable route = do
    href <- g1 @XAsk @p <#> _.print <#> (#) route
    g1 @XTell @XDom.XProps_ $ pure $ XDom.Href href

