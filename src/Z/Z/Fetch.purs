module Z.Z.Fetch
  ( Fetch'RequestHeaders(..)
  , Fetch'RequestOptions
  , Fetch'Response
  , Http'Method(..)
  , fetch
  , fetchRes'json
  , fetchRes'text
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either, note)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Z.Z.Core (encodeOpts)
import Z.Z.Defaultable (class Generable, GDefault, default)
import Z.Z.Future (Promise)
import Z.Z.HashMap (HashMap, hm'fromFoldable, hm'lookup)
import Z.Z.Util (decodeViaString)

foreign import data Fetch'Response :: Type

fetch :: String -> Fetch'RequestOptions -> Effect (Promise Fetch'Response)
fetch url = js_fetch url <<< encodeOpts

fetchRes'text :: Fetch'Response -> Effect (Promise String)
fetchRes'text = js_fetchRes_text

fetchRes'json :: Fetch'Response -> Effect (Promise Json)
fetchRes'json = js_fetchRes_json

data Http'Method
  = Http'Get
  | Http'Post
  | Http'Put
  | Http'Patch
  | Http'Delete
  | Http'Head
  | Http'Options
  | Http'Connect
  | Http'Trace
  | Http'Query

derive instance Eq Http'Method

instance Show Http'Method where
  show Http'Get = "GET"
  show Http'Post = "POST"
  show Http'Put = "PUT"
  show Http'Patch = "PATCH"
  show Http'Delete = "DELETE"
  show Http'Head = "HEAD"
  show Http'Options = "OPTIONS"
  show Http'Connect = "CONNECT"
  show Http'Trace = "TRACE"
  show Http'Query = "QUERY"

allHttpMethods :: Array Http'Method
allHttpMethods =
  [ Http'Get
  , Http'Post
  , Http'Put
  , Http'Patch
  , Http'Delete
  , Http'Head
  , Http'Options
  , Http'Connect
  , Http'Trace
  , Http'Query
  ]

httpMethodByString :: HashMap String Http'Method
httpMethodByString = hm'fromFoldable $ allHttpMethods <#> \m -> show m /\ m

parseHttpMethod :: String -> Either String Http'Method
parseHttpMethod s =
  note ("invalid Http method: " <> s) $ hm'lookup s httpMethodByString

instance EncodeJson Http'Method where
  encodeJson = encodeJson <<< show

instance DecodeJson Http'Method where
  decodeJson = decodeViaString parseHttpMethod

newtype Fetch'RequestHeaders = Fetch'RequestHeaders
  { "Content-Type" :: Maybe String
  }

derive instance Newtype Fetch'RequestHeaders _

instance EncodeJson Fetch'RequestHeaders where
  encodeJson = encodeOpts <<< unwrap

instance DecodeJson Fetch'RequestHeaders where
  decodeJson = map wrap <<< decodeJson

instance Generable Fetch'RequestHeaders GDefault Fetch'RequestHeaders where
  mkGenerable = Fetch'RequestHeaders default

type Fetch'RequestOptions =
  { method :: Maybe Http'Method
  , headers :: Fetch'RequestHeaders
  , body :: Maybe String
  }

foreign import js_fetch
  :: String -> Json -> Effect (Promise Fetch'Response)

foreign import js_fetchRes_json :: Fetch'Response -> Effect (Promise Json)

foreign import js_fetchRes_text :: Fetch'Response -> Effect (Promise String)