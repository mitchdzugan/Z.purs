module Z.Sys.Module
  ( FSDataError'row
  , FSDataError(..)
  ) where

import Z.Prelude

type FSDataError'row = (readError :: JsError, decodeError :: JsonDecodeError)
newtype FSDataError = FSDataError (Variant FSDataError'row)

derive instance Newtype FSDataError _

instance DecodeJson FSDataError where
  decodeJson = var'decode

instance EncodeJson FSDataError where
  encodeJson = var'encode