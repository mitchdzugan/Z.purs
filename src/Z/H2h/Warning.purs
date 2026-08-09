module Z.H2h.Warning
  ( T(..)
  ) where

import Z.Prelude
import Z.Gql.Module as Gql

data T = Gql Gql.Warning

derive instance Generic T _

instance DecodeJson T where
  decodeJson x = genericDecodeJson x

instance EncodeJson T where
  encodeJson x = genericEncodeJson x