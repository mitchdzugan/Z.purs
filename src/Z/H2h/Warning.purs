module Z.H2h.Warning
  ( T(..)
  ) where

import Z.Prelude

import Z.Gql.Module as Gql
import Z.Z.Json.Decode (class XDecodeJson)

newtype T = T (Variant (gql :: Gql.Warning))

derive instance Newtype T _

derive newtype instance XDecodeJson T

instance EncodeJson T where
  encodeJson = var'encode