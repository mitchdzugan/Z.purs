module Z.Gql.Error
  ( GqlResponseError
  , T(..)
  ) where

import Z.Prelude

type GqlResponseError =
  { response ::
      { data :: Json
      , errors :: Json
      , extensions :: Json
      , status :: Int
      , headers :: Object String
      }
  , request :: { query :: String, variables :: Json }
  }

data T
  = NetworkError JsError
  | ResponseError GqlResponseError
  | CachePrep JsError
  | CacheWriter JsError
  | CacheOnlyEmpty
  | ResponseTypeError JsonDecodeError

derive instance Generic T _

instance DecodeJson T where
  decodeJson x = genericDecodeJson x

instance EncodeJson T where
  encodeJson x = genericEncodeJson x
