module Z.Sys.Module
  ( FSDataError(..)
  ) where

import Z.Prelude

data FSDataError = ReadError JsError | DecodeError JsonDecodeError

derive instance genericFSDataError :: Generic FSDataError _

instance decodeJsonFSDataError :: DecodeJson FSDataError where
  decodeJson x = genericDecodeJson x

instance encodeJsonFSDataError :: EncodeJson FSDataError where
  encodeJson x = genericEncodeJson x