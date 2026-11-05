module Z.Z.UrlExp where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Z.Z.Core (rec'modify)
import Z.Z.Defaultable (default)
import Z.Z.HashMap (HashMap)
import Z.Z.Json.Decode (class XDecodeJson)
import Z.Z.Url as Url
import Z.Z.Variant (var'encode)
import Z.Z.X.Nicknames (Edit, edit)

type UrlParts = Url.Parts

url'fromParts :: Url.Parts'from -> Url.URL
url'fromParts = Url.fromParts

url'fromString :: String -> Maybe Url.URL
url'fromString = Url.fromString

url'query :: Url.URL -> HashMap String (Array String)
url'query = Url.query

url'pathSegmentsFromPathname :: String -> Array String
url'pathSegmentsFromPathname = Url.pathSegmentsFromPathname

url'toString :: Url.URL -> String
url'toString = Url.toString

url'origin :: Url.URL -> String
url'origin url = Url.protocol url <> "://" <> rest (Url.port url)
  where
  rest (Just n) = Url.host url <> ":" <> show n
  rest _ = Url.host url

url'pathSegments :: Url.URL -> Array String
url'pathSegments = Url.path

url'hash :: Url.URL -> Maybe String
url'hash = Url.hash

url'pathname :: Url.URL -> String
url'pathname = Url.pathname

url'relative :: Url.URL -> String
url'relative = Url.relative

url'parts :: Url.URL -> UrlParts
url'parts = Url.parts

------

url'mk :: Edit UrlParts -> Url.URL
url'mk edits = url'fromParts $ rec'modify @"query" Right $ edit default edits

url'edit :: Url.URL -> Edit UrlParts -> Url.URL
url'edit url edits =
  url'fromParts $ rec'modify @"query" Right $ edit (url'parts url) edits

newtype ResourceStage =
  ResourceStage (Variant (acquire :: Unit, release :: Unit))

derive instance Newtype ResourceStage _

derive newtype instance XDecodeJson ResourceStage

instance EncodeJson ResourceStage where
  encodeJson = var'encode