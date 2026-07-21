module Z.Sys.Module
  ( FSDataError(..)
  ) where

import Z as Z

data FSDataError = ReadError Z.JsError | DecodeError Z.JsonDecodeError

derive instance genericFSDataError :: Z.Generic FSDataError _

instance decodeJsonFSDataError :: Z.DecodeJson FSDataError where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonFSDataError :: Z.EncodeJson FSDataError where
  encodeJson x = Z.genericEncodeJson x