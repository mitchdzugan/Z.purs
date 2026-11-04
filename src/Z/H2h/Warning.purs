module Z.H2h.Warning
  ( T(..)
  ) where

import Z.Prelude

import Z.Gql.Module as Gql

newtype T = T (Variant (gql :: Gql.Warning))

derive instance Newtype T _

instance DecodeJson T where
  decodeJson = var'decode

instance EncodeJson T where
  encodeJson = var'encode