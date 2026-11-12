module Z.Z.Fetch
  ( Fetch'RequestHeaders(..)
  , Fetch'RequestOptions
  , Fetch'Response
  , Http'Method(..)
  , fetch
  , fetchRes'json
  , fetchRes'text
  , x'decodeFetched
  , x'fetch
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either, note)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Z.Z.Core (JsError, encodeOpts)
import Z.Z.Defaultable (class Generable, GDefault, default)
import Z.Z.HashMap (HashMap, hm'collect, hm'lookup)
import Z.Z.Json.Decode
  ( class XDecodeJson
  , JsonDecodeError
  , decodeJson
  , xDecodeViaString
  )
import Z.Z.Promise (EffPromise)
import Z.Z.Run (type (@^->))
import Z.Z.X.Index (E', e'runEffPromise)
import Z.Z.X.Nicknames (Edit, edit)

foreign import data Fetch'Response :: Type

----------------------------

x'fetch
  :: forall x
   . String
  -> Edit Fetch'RequestOptions
  -> E' JsError x @^-> Fetch'Response
x'fetch url opts = e'runEffPromise $ fetch url $ edit default opts

x'decodeFetched
  :: forall @t x
   . XDecodeJson t
  => Fetch'Response
  -> E' JsError x @^-> Either JsonDecodeError t
x'decodeFetched res = decodeJson @t <$> e'runEffPromise (fetchRes'json res)

-----------------------------

fetch :: String -> Fetch'RequestOptions -> EffPromise Fetch'Response
fetch url = js_fetch url <<< encodeOpts

fetchRes'text :: Fetch'Response -> EffPromise String
fetchRes'text = js_fetchRes_text

fetchRes'json :: Fetch'Response -> EffPromise Json
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
httpMethodByString = hm'collect $ allHttpMethods <#> \m -> show m /\ m

parseHttpMethod :: String -> Either String Http'Method
parseHttpMethod s =
  note ("invalid Http method: " <> s) $ hm'lookup s httpMethodByString

instance EncodeJson Http'Method where
  encodeJson = encodeJson <<< show

instance XDecodeJson Http'Method where
  xDecodeJson = xDecodeViaString parseHttpMethod

newtype Fetch'RequestHeaders = Fetch'RequestHeaders
  { "Content-Type" :: Maybe String
  }

derive instance Newtype Fetch'RequestHeaders _

derive newtype instance EncodeJson Fetch'RequestHeaders

derive newtype instance XDecodeJson Fetch'RequestHeaders

instance Generable Fetch'RequestHeaders GDefault Fetch'RequestHeaders where
  mkGenerable = Fetch'RequestHeaders default

type Fetch'RequestOptions =
  { method :: Maybe Http'Method
  , headers :: Fetch'RequestHeaders
  , body :: Maybe String
  }

foreign import js_fetch :: String -> Json -> EffPromise Fetch'Response

foreign import js_fetchRes_json :: Fetch'Response -> EffPromise Json

foreign import js_fetchRes_text :: Fetch'Response -> EffPromise String