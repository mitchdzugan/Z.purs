module Z.H2h.Warning
  ( T(..)
  ) where

import Z.Prelude
import Z.Gql.Module as Gql

data T = Gql Gql.Warning

derive instance genericT :: Generic T _

instance decodeJsonT :: DecodeJson T where
  decodeJson x = genericDecodeJson x

instance encodeJsonT :: EncodeJson T where
  encodeJson x = genericEncodeJson x