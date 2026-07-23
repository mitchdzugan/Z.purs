module Z.Gql.Error
  ( T(..)
  ) where

import Z as Z

data T
  = NetworkError Z.JsError
  | CachePrep Z.JsError
  | CacheWriter Z.JsError
  | CacheOnlyEmpty
  | ResponseTypeError Z.JsonDecodeError

derive instance gnericT :: Z.Generic T _

instance decodeT :: Z.DecodeJson T where
  decodeJson x = Z.genericDecodeJson x

instance encodeT :: Z.EncodeJson T where
  encodeJson x = Z.genericEncodeJson x
