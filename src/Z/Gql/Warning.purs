module Z.Gql.Warning
  ( T(..)
  ) where

import Node.Z.Prelude

data T
  = CacheDecode FSDataError
  | CacheWrite JsError

derive instance Generic T _
instance DecodeJson T where
  decodeJson x = genericDecodeJson x

instance EncodeJson T where
  encodeJson x = genericEncodeJson x