module Z.Gql.Error
  ( GqlResponseError
  , T'row
  , T(..)
  , cacheOnlyEmpty
  , cachePrep
  , cacheWriter
  , networkError
  , responseError
  , responseTypeError
  ) where

import Z.Prelude

import Z.Z.Json.Decode (class XDecodeJson)

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

type T'row =
  ( networkError :: JsError
  , responseError :: GqlResponseError
  , cachePrep :: JsError
  , cacheWriter :: JsError
  , cacheOnlyEmpty :: Unit
  , responseTypeError :: JsonDecodeError
  )

newtype T = T (Variant T'row)

derive instance Newtype T _

derive newtype instance XDecodeJson T

instance EncodeJson T where
  encodeJson = var'encode

networkError :: JsError -> T
networkError = var'inj @"networkError"

responseError :: GqlResponseError -> T
responseError = var'inj @"responseError"

cachePrep :: JsError -> T
cachePrep = var'inj @"cachePrep"

cacheWriter :: JsError -> T
cacheWriter = var'inj @"cacheWriter"

cacheOnlyEmpty :: T
cacheOnlyEmpty = var'inj_ @"cacheOnlyEmpty"

responseTypeError :: JsonDecodeError -> T
responseTypeError = var'inj @"responseTypeError"