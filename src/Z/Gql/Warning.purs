module Z.Gql.Warning
  ( T(..)
  ) where

import Z.Prelude

import Z.Sys.Module (FSDataError)
import Z.Z.Json.Decode (class XDecodeJson)

newtype T = T (Variant (cacheDecode :: FSDataError, cacheWrite :: JsError))

derive instance Newtype T _

derive newtype instance XDecodeJson T

instance EncodeJson T where
  encodeJson = var'encode