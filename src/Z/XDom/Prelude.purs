module Z.XDom.Prelude
  ( X'Reducer
  , Router'
  , XurlStProviderX
  , module ModuleReExports
  ) where

import Z.Prelude as ModuleReExports
import Z.Prelude as Z
import Z.XDom.Core as ModuleReExports
import Z.XDom.Preact (ReactEl) as ModuleReExports
import Z.XDom.Router
  ( router'href
  , router'href''
  , router'routeOrE
  , router'routeOrE''
  , router'run
  , router'run''
  , router'urlState
  , router'urlState''
  ) as ModuleReExports
import Z.XDom.Router as Router
import Z.XDom.UrlState
  ( HrefSpec
  , RProvider
  , T
  , XProvider
  , actualHref
  , hrefFromUrl
  , mk
  , parsedHref
  , update
  ) as ModuleReExports
import Z.XDom.UrlState as UrlSt

type XurlStProviderX sx de = UrlSt.XProvider sx de

type X'Reducer a s =
  (Z.X'R { get :: s, update :: a -> Z.Eff'At "domEff" Z.Unit })

type Router' e r x = Router.X e r x