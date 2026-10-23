module Z.XDom.Prelude
  ( Router'
  , XurlStProviderX
  , module ModuleReExports
  ) where

import Z.Prelude as ModuleReExports
import Z.Prelude as Z
import Z.XDom.Core3 as ModuleReExports
import Z.XDom.FetchCache as ModuleReExports
import Z.XDom.Preact (ReactEl) as ModuleReExports
import Z.XDom.Router
  ( router'mkRel
  , router'mkRel''
  , router'mkUrl
  , router'mkUrl''
  , router'routeOrE
  , router'routeOrE''
  , router'run
  , router'run''
  , router'setHref
  , router'setHref''
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

type Router' e r x = Router.X e r x