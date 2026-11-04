module Z.Gql.Warning
  ( T(..)
  ) where

import Z.Prelude

import Z.Sys.Module (FSDataError)

newtype T = T (Variant (cacheDecode :: FSDataError, cacheWrite :: JsError))

derive instance Newtype T _

instance DecodeJson T where
  decodeJson = var'decode

instance EncodeJson T where
  encodeJson = var'encode