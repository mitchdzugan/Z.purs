module Z.Gql.Warning
  ( T(..)
  ) where

import Z.Sys.Module as Sys
import Z as Z

data T
  = CacheDecode Sys.FSDataError
  | CacheWrite Z.JsError

derive instance gnericT :: Z.Generic T _

instance decodeT :: Z.DecodeJson T where
  decodeJson x = Z.genericDecodeJson x

instance encodeT :: Z.EncodeJson T where
  encodeJson x = Z.genericEncodeJson x