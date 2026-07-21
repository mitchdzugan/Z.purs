module Z.Gql.Module
  ( Error(..)
  , NetworkControl(..)
  , OpenOpts
  , Opts
  , Warning(..)
  , _cachePath
  , _networkControl
  , baseOpts
  ) where

import Prelude

import Z.Sys.Module as Sys
import Z as Z

data NetworkControl
  = CacheOnly
  | CacheFirst
  | ForceFetch

derive instance eqNetworkControl :: Eq NetworkControl

type OpenOpts r =
  { networkControl :: NetworkControl
  , cachePath :: Z.Maybe String
  | r
  }

type Opts = OpenOpts ()

baseOpts :: Opts
baseOpts = { networkControl: CacheFirst, cachePath: Z.Nothing }

_networkControl
  :: forall r. Z.Lens' { networkControl :: NetworkControl | r } NetworkControl
_networkControl = Z.prop (Z.p :: Z.P "networkControl")

_cachePath
  :: forall r. Z.Lens' { cachePath :: Z.Maybe String | r } (Z.Maybe String)
_cachePath = Z.prop (Z.p :: Z.P "cachePath")

data Warning
  = CacheDecode Sys.FSDataError
  | CacheWrite Z.JsError

derive instance genericWarning :: Z.Generic Warning _

instance decodeJsonWarning :: Z.DecodeJson Warning where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonWarning :: Z.EncodeJson Warning where
  encodeJson x = Z.genericEncodeJson x

data Error
  = NetworkError Z.JsError
  | CachePrep Z.JsError
  | CacheWriter Z.JsError
  | CacheOnlyEmpty
  | ResponseTypeError Z.JsonDecodeError

derive instance genericError :: Z.Generic Error _

instance decodeJsonError :: Z.DecodeJson Error where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonError :: Z.EncodeJson Error where
  encodeJson x = Z.genericEncodeJson x
