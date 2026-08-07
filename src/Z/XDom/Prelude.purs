module Z.XDom.Prelude
  ( DomX
  , Reducer
  , XReducer
  , XurlStProviderX
  , class XDomTLS
  , module XDom
  , module ZP
  , xdom
  , xdom'
  ) where

import Z.Prelude (class DimensionedValTag)
import Z.Prelude hiding (div) as ZP
import Z.XDom.Core as XDom
import Z.XDom.Reducer as Rdc
import Z.XDom.Router as Rt
import Z.XDom.State as St
import Z.XDom.UrlState as UrlSt

type Reducer a s = Rdc.R a s
type XReducer a s x = Rdc.X a s x

type XurlStProviderX x = UrlSt.XProvider x

class XDomTLS
  :: forall k1
   . Symbol
  -> k1
  -> Constraint
class XDomTLS sym f | sym -> f

instance XDomTLS "bindE" XDom.DomBindE
else instance XDomTLS "runR" XDom.DomRunR
else instance XDomTLS "Router.routeOrE" Rt.RouteOrE
else instance XDomTLS "Router.href" Rt.HrefAttr
else instance XDomTLS "Router.run" Rt.Run
else instance XDomTLS "get" St.Get
else instance XDomTLS "set" St.Set
else instance XDomTLS "State.run" St.Run
else instance XDomTLS "Reducer.run" Rdc.Run
else instance XDomTLS "dispatch" Rdc.Dispatch

data DomX s = DomX

instance DimensionedValTag (DomX "Router.run") Rt.Run
else instance DimensionedValTag (DomX "Router.hrefAttr") Rt.HrefAttr
else instance DimensionedValTag (DomX "runR") XDom.DomRunR

xdom'
  :: forall @sym o f
   . ZP.Cons0 f
  => XDomTLS sym f
  => ZP.RWSEFn f "reader" "writer" "state" "except" o
  => o
xdom' = ZP.rwseApply (ZP.cons0 :: f) (ZP.p @"reader") (ZP.p @"writer")
  (ZP.p @"state")
  (ZP.p @"except")

xdom
  :: forall @pp @sym f o
   . ZP.Cons0 f
  => XDomTLS sym f
  => ZP.RWSEFn f pp pp pp pp o
  => o
xdom = ZP.rwseApply (ZP.cons0 :: f) (ZP.p @pp) (ZP.p @pp) (ZP.p @pp)
  (ZP.p @pp)
