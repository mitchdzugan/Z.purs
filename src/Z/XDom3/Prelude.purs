module Z.XDom3.Prelude
  ( Reducer
  , XurlStProviderX
  , module ModuleReExports
  ) where

import Z.Prelude as ModuleReExports
import Z.Prelude as Z
import Z.XDom3.Core as Core
import Z.XDom3.Core as ModuleReExports
import Z.XDom3.Preact (ReactEl) as ModuleReExports
import Z.XDom3.Router hiding (R, T, X) as ModuleReExports
import Z.XDom3.UrlState as ModuleReExports
import Z.XDom3.UrlState as UrlSt

type XurlStProviderX sx de = UrlSt.XProvider sx de

type Reducer a s =
  (Z.R' { get :: s, update :: a -> Z.Eff'At "domEff" Z.Unit })