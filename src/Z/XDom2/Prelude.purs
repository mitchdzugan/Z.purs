module Z.XDom2.Prelude
  ( Reducer
  , XurlStProviderX
  , module ModuleReExports
  ) where

import Z.Prelude as ModuleReExports
import Z.Prelude as Z
import Z.XDom2.Core as Core
import Z.XDom2.Core as ModuleReExports
import Z.XDom2.Preact (ReactEl) as ModuleReExports
import Z.XDom2.Router hiding (R, T, X) as ModuleReExports
import Z.XDom2.UrlState as ModuleReExports
import Z.XDom2.UrlState as UrlSt

type XurlStProviderX sx de = UrlSt.XProvider sx de

type Reducer a s =
  (Z.R' { get :: s, update :: a -> Z.XEffTagged "domEff" Z.Unit })