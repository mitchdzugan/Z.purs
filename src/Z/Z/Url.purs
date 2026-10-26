module Z.Z.Url
  ( Parts
  , URL
  , class QueryParam
  , fromParts
  , fromString
  , hash
  , host
  , parts
  , password
  , path
  , pathSegmentsFromPathname
  , pathname
  , port
  , protocol
  , query
  , queryParamTuple
  , relative
  , setQuery
  , toString
  , username
  ) where

import Prelude

import Data.Filterable (filter)
import Data.Foldable (foldl, intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (wrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (null, replace, split) as String
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Z.Z.Core (arr'drop)
import Z.Z.Defaultable (class Generable, GDefault, default)
import Z.Z.HashMap (HashMap, hm'entries, hm'fromFoldable)
import Z.Z.String (str'drop, str'startsWith)

class QueryParam a where
  queryParamTuple :: a -> String /\ Array String

instance QueryParam String where
  queryParamTuple s = s /\ [ "" ]
else instance QueryParam (String /\ String) where
  queryParamTuple (k /\ v) = k /\ [ v ]
else instance QueryParam (String /\ Array String) where
  queryParamTuple = identity

foreign import data URL :: Type

instance Show URL where
  show u = "(URL " <> show (toString u) <> ")"

instance Eq URL where
  eq a b = parts a == parts b

instance Ord URL where
  compare a b = compare (parts a) (parts b)

instance Generable URL GDefault URL where
  mkGenerable = fromParts default

type Parts =
  { path :: Array String
  , query :: HashMap String (Array String)
  , hash :: Maybe String
  , host :: String
  , port :: Maybe Int
  , protocol :: String
  , username :: Maybe String
  , password :: Maybe String
  }

foreign import fromStringImpl :: String -> Nullable URL

foreign import hashImpl :: URL -> String
foreign import searchImpl :: URL -> String
foreign import hrefImpl :: URL -> String
foreign import hostImpl :: URL -> String
foreign import passwordImpl :: URL -> String
foreign import pathnameImpl :: URL -> String
foreign import portImpl :: URL -> String
foreign import protocolImpl :: URL -> String
foreign import usernameImpl :: URL -> String
foreign import queryKeysImpl :: URL -> Array String
foreign import queryLookupImpl :: String -> URL -> Array String
foreign import querySetAllImpl
  :: Array { k :: String, vs :: Array String } -> URL -> URL

foreign import setHashImpl :: String -> URL -> URL
foreign import setHostImpl :: String -> URL -> URL
foreign import setPasswordImpl :: String -> URL -> URL
foreign import setPathnameImpl :: String -> URL -> URL
foreign import setPortImpl :: String -> URL -> URL
foreign import setProtocolImpl :: String -> URL -> URL
foreign import setUsernameImpl :: String -> URL -> URL
foreign import relativeImpl :: URL -> String

relative :: URL -> String
relative = relativeImpl

fromString :: String -> Maybe URL
fromString = Nullable.toMaybe <<< fromStringImpl

toString :: URL -> String
toString = hrefImpl

fromParts :: Parts -> URL
fromParts u =
  let
    empty = unsafePartial fromJust $ fromString "http://0.0.0.0"

    perhaps :: forall a b. (a -> b -> b) -> Maybe a -> b -> b
    perhaps f a = maybe identity f a
  in
    setQuery u.query
      $ setPathnameImpl ("/" <> intercalate "/" u.path)
      $ setHostImpl u.host
      $ perhaps setUsernameImpl u.username
      $ perhaps setPasswordImpl u.password
      $ perhaps setHashImpl u.hash
      $ perhaps setPort u.port
      $ setProtocolImpl u.protocol
      $ setHostImpl u.host
      $ empty

parts :: URL -> Parts
parts u =
  { path: path u
  , query: query u
  , host: host u
  , port: port u
  , username: username u
  , password: password u
  , hash: hash u
  , protocol: protocol u
  }

query :: URL -> HashMap String (Array String)
query u =
  let
    ks = queryKeysImpl u
    vals k = queryLookupImpl k u
  in
    hm'fromFoldable $ map (\k -> k /\ vals k) ks

setQuery :: HashMap String (Array String) -> URL -> URL
setQuery qs'hm u =
  let
    qs = hm'entries qs'hm
    asRecord = foldl (\a (k /\ vs) -> a <> [ { k, vs } ]) [] qs
  in
    querySetAllImpl asRecord u

pathSegmentsFromPathname :: String -> Array String
pathSegmentsFromPathname pname = splitCleaned cleanPname
  where
  splitCleaned "" = []
  splitCleaned cleaned = String.split (wrap "/") cleaned
  cleanPname = if str'startsWith "/" pname then str'drop 1 pname else pname

path :: URL -> Array String
path = pathSegmentsFromPathname <<< pathnameImpl

pathname :: URL -> String
pathname = pathnameImpl

hash :: URL -> Maybe String
hash = filter (not <<< String.null) <<< Just
  <<< String.replace (wrap "#") (wrap "")
  <<< hashImpl

host :: URL -> String
host = hostImpl

password :: URL -> Maybe String
password = filter (not <<< String.null) <<< Just <<< passwordImpl

port :: URL -> Maybe Int
port = Int.fromString <<< portImpl

protocol :: URL -> String
protocol = String.replace (wrap ":") (wrap "") <<< protocolImpl

username :: URL -> Maybe String
username = filter (not <<< String.null) <<< Just <<< usernameImpl

setPort :: Int -> URL -> URL
setPort = setPortImpl <<< Int.toStringAs Int.decimal
