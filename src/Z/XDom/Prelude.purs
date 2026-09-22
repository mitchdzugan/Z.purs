module Z.XDom.Prelude
  ( Reducer
  , XurlStProviderX
  , module ModuleReExports
  ) where

import Z.Prelude as ModuleReExports
import Z.Prelude as Z
import Z.XDom.Core
  ( ATTR
  , DomA
  , DomATag
  , DomE
  , DomETag
  , DomEffPermit
  , MDOMEFF
  , MDom
  , MDomEff
  , MDomEl
  , R'Self
  , Self
  , XDom
  , XDomA
  , XDomE
  , XDomEA
  , dom'a
  , dom'article
  , dom'br
  , dom'button
  , dom'div
  , dom'element
  , dom'iframe
  , dom'pre
  , dom'span
  , dom'text
  , dom'withAdapter
  , dom'withKey
  , dom'withNewState
  , domE'bind
  , domE'bind''
  , domE'fail
  , domE'fail''
  , domEff'do
  , domEff'getRunner
  , domEff'getSelf
  , domEff'on
  , domEff'on'1
  , domEff'on'1'post
  , domEff'on'1'pre
  , domEff'on'all
  , domEff'on'all'post
  , domEff'on'all'pre
  , domEff'on'post
  , domEff'on'pre
  , domR'run
  , domR'run''
  , domS'dispatch
  , domS'dispatch''
  , domS'get
  , domS'get''
  , domS'runReducer
  , domS'runReducer''
  , domS'runState
  , domS'runState''
  , domS'set
  , domS'set''
  , el'cn
  , el'cnW
  , el'href
  , el'key
  , el'onClick
  , exec'xdom
  , op'el'text
  , (%%)
  ) as ModuleReExports
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

type Reducer a s =
  (Z.X'R { get :: s, update :: a -> Z.Eff'At "domEff" Z.Unit })
