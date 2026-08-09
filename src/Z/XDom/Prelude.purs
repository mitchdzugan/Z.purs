module Z.XDom.Prelude
  ( Reducer
  , XReducer
  , XurlStProviderX
  , module ModuleReExports
  , module XDom
  , module ZP
  ) where

import Z.Prelude (class DimensionedValTag)
import Z.Prelude hiding (div) as ZP
import Z.XDom.Core as XDom
import Z.XDom.Reducer (XDomDispatch, XDomRunReducer) as ModuleReExports
import Z.XDom.Reducer as Rdc
import Z.XDom.Router (XDomRouteHref, XDomRouteOrE, XDomRunRouter) as ModuleReExports
import Z.XDom.Router as Rt
import Z.XDom.State (XDomGetState, XDomRunState, XDomSetState) as ModuleReExports
import Z.XDom.State as St
import Z.XDom.UrlState as UrlSt

type Reducer a s = Rdc.R a s
type XReducer a s x = Rdc.X a s x

type XurlStProviderX x = UrlSt.XProvider x
