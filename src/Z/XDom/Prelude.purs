module Z.XDom.Prelude
  ( Reducer
  , XurlStProviderX
  , module ModuleReExports
  ) where

import Z.Prelude as ModuleReExports
import Z.Prelude as Z
import Z.XDom.Core as Core
import Z.XDom.Core as ModuleReExports
import Z.XDom.Preact (ReactEl) as ModuleReExports
import Z.XDom.Router hiding (R, T, X) as ModuleReExports
import Z.XDom.UrlState as ModuleReExports
import Z.XDom.UrlState as UrlSt

type XurlStProviderX sx de = UrlSt.XProvider sx de

type Reducer a s =
  (Z.R' { get :: s, update :: a -> Z.Eff'At "domEff" Z.Unit })