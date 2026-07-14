module Gql.Node
  ( Client
  , _authToken
  , _url
  , mkClient
  , mkClient_
  , module Gql
  , operateUnknown
  ) where

import Prelude

import Debug as Debug
import Gql.Core (Error(..), OpenOpts, Opts, baseOpts) as Gql
import Z as Z

foreign import js_requestGql
  :: String -> Z.JSON -> String -> Z.JSON -> Z.Effect (Z.Promise Z.JSON)

requestGql
  :: forall x
   . String
  -> Z.JSON
  -> String
  -> Z.JSON
  -> Z.Xea x Gql.Error Z.JSON
requestGql apiUrl authToken query vars = do
  Z.e_map Gql.NetworkError
    $ Z.effectPromiseX
    $ js_requestGql apiUrl authToken query vars

type Client = Gql.OpenOpts (url :: String, authToken :: Z.Maybe String)

_authToken
  :: forall r. Z.Lens' { authToken :: Z.Maybe String | r } (Z.Maybe String)
_authToken = Z.prop (Z.Proxy :: Z.Proxy "authToken")

_url :: forall r. Z.Lens' { url :: String | r } String
_url = Z.prop (Z.Proxy :: Z.Proxy "url")

mkClient :: String -> Z.ModX Client -> Client
mkClient url clientMod = Z.xMod baseClient clientMod
  where
  baseClient = Z.merge { url: url, authToken: Z.Nothing } Gql.baseOpts

mkClient_ :: String -> Client
mkClient_ url = mkClient url Z.pass

operateUnknown
  :: forall x
   . Client
  -> String
  -> Z.JSON
  -> Z.ModX Gql.Opts
  -> Z.Xea x Gql.Error Z.JSON
operateUnknown c query vars optsMod = do
  let baseOpts = { cachePath: c.cachePath, networkControl: c.networkControl }
  let opts = Z.xMod baseOpts optsMod
  Debug.traceM opts
  requestGql c.url Z.null query vars
