module Z.Sys.Module
  ( FSDataError'row
  , FSDataError(..)
  ) where

import Z.Prelude

import Z.Z.Json.Decode (class XDecodeJson)

type FSDataError'row = (readError :: JsError, decodeError :: JsonDecodeError)
newtype FSDataError = FSDataError (Variant FSDataError'row)

derive instance Newtype FSDataError _

derive newtype instance XDecodeJson FSDataError

instance EncodeJson FSDataError where
  encodeJson = var'encode