module Gql.Node
  ( Client
  , _authToken
  , _url
  , mkClient
  , operateUnknown
  , module Gql
  ) where

import Prelude
import Gql.Core (Error(..), OpenOpts, Opts, baseOpts) as Gql
import X as X
import Z as Z

foreign import jsRequestGql
  :: String -> Z.JSON -> String -> Z.JSON -> Z.Effect (Z.Promise Z.JSON)

requestGqlAff :: String -> Z.JSON -> String -> Z.JSON -> Z.Aff Z.JSON
requestGqlAff apiUrl authToken query vars = do
  promise <- Z.liftEffect $ jsRequestGql apiUrl authToken query vars
  Z.promiseToAff promise

type Client = Gql.OpenOpts (url :: String, authToken :: Z.Maybe String)

_authToken
  :: forall r. Z.Lens' { authToken :: Z.Maybe String | r } (Z.Maybe String)
_authToken = Z.prop (Z.Proxy :: Z.Proxy "authToken")

_url :: forall r. Z.Lens' { url :: String | r } String
_url = Z.prop (Z.Proxy :: Z.Proxy "url")

mkClient :: String -> X.UpdateX Client -> Client
mkClient url clientX = X.updateX
  (Z.merge { url: url, authToken: Z.Nothing } Gql.baseOpts)
  clientX

operateUnknown
  :: forall x
   . Client
  -> String
  -> Z.JSON
  -> X.UpdateX Gql.Opts
  -> X.X (X.E Gql.Error (X.A x)) Z.JSON
operateUnknown client query vars optsX = do
  let
    opts = X.updateX
      { cachePath: client.cachePath, networkControl: client.networkControl }
      optsX
  X.tryAff Gql.NetworkError $ requestGqlAff client.url Z.null query vars