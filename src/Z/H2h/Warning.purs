module Z.H2h.Warning
  ( T(..)
  ) where

import Z as Z
import Z.Gql.Module as Gql

data T = Gql Gql.Warning

derive instance genericT :: Z.Generic T _

instance decodeJsonT :: Z.DecodeJson T where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonT :: Z.EncodeJson T where
  encodeJson x = Z.genericEncodeJson x