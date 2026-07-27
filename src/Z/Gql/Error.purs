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

derive instance gnericT :: Generic T _

instance decodeT :: DecodeJson T where
  decodeJson x = genericDecodeJson x

instance encodeT :: EncodeJson T where
  encodeJson x = genericEncodeJson x
