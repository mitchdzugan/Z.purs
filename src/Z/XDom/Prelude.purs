module Z.XDom.Prelude
  ( class XDomTLS
  , module XDom
  , module ZP
  , xdom
  , xdom'
  ) where

import Z.Prelude hiding (div) as ZP
import Z.XDom.Preact as XDom
import Z.XDom.Reducer as Rdc
import Z.XDom.Router as Rt
import Z.XDom.State as St

class XDomTLS
  :: forall k1
   . Symbol
  -> k1
  -> Constraint
class XDomTLS sym f | sym -> f

instance XDomTLS "bindE" XDom.DomBindE
else instance XDomTLS "runR" XDom.DomRunR
else instance XDomTLS "routeOrE" Rt.RouteOrE
else instance XDomTLS "routeHref" Rt.HrefAttr
else instance XDomTLS "runRouter" Rt.Run
else instance XDomTLS "get" St.Get
else instance XDomTLS "set" St.Set
else instance XDomTLS "runState" St.Run
else instance XDomTLS "runReducer" Rdc.Run
else instance XDomTLS "dispatch" Rdc.Dispatch

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
