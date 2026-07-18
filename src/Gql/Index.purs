module Gql.Index
  ( Error(..)
  , NetworkControl(..)
  , OpenOpts
  , Opts
  , _cachePath
  , _networkControl
  , baseOpts
  ) where

import Z as Z

data NetworkControl
  = CacheOnly
  | UseCache
  | ForceFetch

type OpenOpts r =
  { networkControl :: NetworkControl
  , cachePath :: Z.Maybe String
  | r
  }

type Opts = OpenOpts ()

baseOpts :: Opts
baseOpts = { networkControl: CacheOnly, cachePath: Z.Nothing }

_networkControl
  :: forall r. Z.Lens' { networkControl :: NetworkControl | r } NetworkControl
_networkControl = Z.prop (Z.Proxy :: Z.Proxy "networkControl")

_cachePath
  :: forall r. Z.Lens' { cachePath :: Z.Maybe String | r } (Z.Maybe String)
_cachePath = Z.prop (Z.Proxy :: Z.Proxy "cachePath")

data Error
  = NetworkError Z.JsError
  | CachePrep Z.JsError
  | CacheWriter Z.JsError
  | CacheOnlyEmpty Z.JsError
  | ResponseTypeError Z.JsonDecodeError

derive instance genericError :: Z.Generic Error _

instance decodeJsonError :: Z.DecodeJson Error where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonError :: Z.EncodeJson Error where
  encodeJson x = Z.genericEncodeJson x
