module Z.Sys.Module
  ( FSDataError(..)
  ) where

import Z.Prelude

data FSDataError = ReadError JsError | DecodeError JsonDecodeError

derive instance Generic FSDataError _

instance DecodeJson FSDataError where
  decodeJson x = genericDecodeJson x

instance EncodeJson FSDataError where
  encodeJson x = genericEncodeJson x