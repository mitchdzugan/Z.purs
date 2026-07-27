module Z.Gql.Warning
  ( T(..)
  ) where

import Node.Z.Prelude

data T
  = CacheDecode FSDataError
  | CacheWrite JsError

derive instance gnericT :: Generic T _

instance decodeT :: DecodeJson T where
  decodeJson x = genericDecodeJson x

instance encodeT :: EncodeJson T where
  encodeJson x = genericEncodeJson x