module Z.Gql.Module
  ( Error(..)
  , Warning(..)
  ) where

import Z.Sys.Module as Sys
import Z as Z

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
