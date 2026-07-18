module Node.Gql.Index
  ( Client
  , Operation
  , _authToken
  , _url
  , defOperation
  , mkClient
  , mkClient'
  , module Gql
  , operate
  , operate'
  , operateUnknown
  , operateUnknown'
  ) where

import Prelude
import Gql.Index (Error(..), OpenOpts, Opts, baseOpts) as Gql
import Z as Z

foreign import js_requestGql
  :: String -> Z.Json -> String -> Z.Json -> Z.Effect (Z.Promise Z.Json)

requestGql
  :: forall x
   . String
  -> Z.Json
  -> String
  -> Z.Json
  -> Z.X (Z.EA Gql.Error x) Z.Json
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

mkClient' :: String -> Client
mkClient' = Z.arg2' Z.pass mkClient

fullOpts
  :: Client
  -> Z.ModX Gql.Opts
  -> Gql.Opts
fullOpts c optsMod = Z.xMod baseOpts optsMod
  where
  baseOpts = { cachePath: c.cachePath, networkControl: c.networkControl }

operateUnknown
  :: forall x
   . Client
  -> String
  -> Z.Json
  -> Z.ModX Gql.Opts
  -> Z.X (Z.EA Gql.Error x) Z.Json
operateUnknown client opString vars optsMod = do
  let opts = fullOpts client optsMod
  let authToken = Z.encodeJson client.authToken
  Z.logInfo opts
  Z.logInfo { vars }
  requestGql client.url authToken opString vars

operateUnknown'
  :: forall x
   . Client
  -> String
  -> Z.Json
  -> Z.X (Z.EA Gql.Error x) Z.Json
operateUnknown' = Z.arg4' Z.pass operateUnknown

data Operation vars res = Operation String (Z.JsonEncodeFn vars)
  (Z.JsonDecodeFn res)

defOperation
  :: forall vars res
   . Z.EncodeJson vars
  => Z.DecodeJson res
  => String
  -> Z.Proxy vars
  -> Z.Proxy res
  -> Operation vars res
defOperation opString _ _ = Operation opString Z.encodeJson Z.decodeJson

operate
  :: forall vars res x
   . Client
  -> Operation vars res
  -> vars
  -> Z.ModX Gql.Opts
  -> Z.X (Z.EA Gql.Error x) res
operate c (Operation opString enc dec) vars optsMod = do
  j <- operateUnknown c opString (enc vars) optsMod
  Z.e_map Gql.ResponseTypeError $ Z.result $ dec j

operate'
  :: forall vars res x
   . Client
  -> Operation vars res
  -> vars
  -> Z.X (Z.EA Gql.Error x) res
operate' = Z.arg4' Z.pass operate
