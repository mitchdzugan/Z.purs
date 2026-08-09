module Z.Gql.Error
  ( T(..)
  ) where

import Z.Prelude

data T
  = NetworkError JsError
  | CachePrep JsError
  | CacheWriter JsError
  | CacheOnlyEmpty
  | ResponseTypeError JsonDecodeError

derive instance Generic T _

instance DecodeJson T where
  decodeJson x = genericDecodeJson x

instance EncodeJson T where
  encodeJson x = genericEncodeJson x
