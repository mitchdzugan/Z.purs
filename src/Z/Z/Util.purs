module Z.Z.Util
  ( JsonDecodeError(..)
  , JsonDecodeFn
  , JsonEncodeFn
  , ResourceStage(..)
  , SorN(..)
  , Type_Ap
  , Type_Ap_R
  , arg2'
  , arg3'
  , arg4'
  , arrReverse
  , arrSort
  , arrSortBy
  , arrSortWith
  , baseDecodeJson
  , class IsStringOrNum
  , decode
  , decode'
  , decodeErrTypeMismatch
  , decodeFailTypeMismatch
  , decodeJson
  , decodeJson'
  , encode
  , id
  , jsonDecode
  , jsonKeys
  , jsonLookup
  , jsonPairs
  , jsonSortedPairs
  , jsonVals
  , nth
  , sOrN
  , type (#)
  , type ($)
  , urlFromParts
  , urlFromString
  , urlOrigin
  , urlPathFromString
  , urlQuery
  , urlRelative
  , urlToString
  ) where

import Prelude

import Data.Argonaut.Core (stringify) as AArg
import Data.Argonaut.Core as Arg
import Data.Argonaut.Decode (class DecodeJson, fromJsonString) as Dec
import Data.Argonaut.Decode (JsonDecodeError(..)) as JDE
import Data.Argonaut.Decode as ADec
import Data.Argonaut.Decode.Generic (genericDecodeJson) as DecodeGeneric
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Enc
import Data.Argonaut.Encode.Generic (genericEncodeJson) as EncodeGeneric
import Data.Array as Array
import Data.Either as Either
import Data.Generic.Rep (class Generic) as Generic
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.Ordering as Ordering
import Data.Tuple as Tup
import Foreign.Object as FO
import Type.Proxy as Proxy
import Z.Z.Core as Z
import Z.Z.Url as Url

urlFromParts :: Url.Parts -> Url.URL
urlFromParts = Url.fromParts

urlFromString :: String -> Maybe.Maybe Url.URL
urlFromString = Url.fromString

urlQuery :: Url.URL -> Map.Map String (Array String)
urlQuery = Url.query

urlPathFromString :: String -> Url.Path
urlPathFromString = Url.pathFromString

urlRelative :: Url.URL -> String
urlRelative = Url.relative

urlToString :: Url.URL -> String
urlToString = Url.toString

urlOrigin :: Url.URL -> String
urlOrigin url = Url.protocol url <> "://" <> rest (Url.port url)
  where
  rest Maybe.Nothing = Url.host url
  rest (Maybe.Just n) = Url.host url <> ":" <> show n

nth :: forall a. Array a -> Int -> Maybe.Maybe a
nth = Array.index

arrSort :: forall a. Ord.Ord a => Array a -> Array a
arrSort = Array.sort

arrSortBy :: forall a. (a -> a -> Ordering.Ordering) -> Array a -> Array a
arrSortBy = Array.sortBy

arrSortWith :: forall a b. Ord.Ord b => (a -> b) -> Array a -> Array a
arrSortWith = Array.sortWith

arrReverse :: forall a. Array a -> Array a
arrReverse = Array.reverse

jsonKeys :: Arg.Json -> Array String
jsonKeys = Arg.caseJsonObject [] FO.keys

jsonVals :: Arg.Json -> Array Arg.Json
jsonVals = Arg.caseJsonObject [] FO.values

jsonPairs :: Arg.Json -> Array (Tup.Tuple String Arg.Json)
jsonPairs = Arg.caseJsonObject [] FO.toUnfoldable

jsonSortedPairs :: Arg.Json -> Array (Tup.Tuple String Arg.Json)
jsonSortedPairs = Arg.caseJsonObject [] FO.toAscUnfoldable

jsonLookup :: String -> Arg.Json -> Maybe.Maybe Arg.Json
jsonLookup k = Arg.caseJsonObject Maybe.Nothing (FO.lookup k)

id :: forall a. a -> a
id a = a

newtype JsonDecodeError = JsonDecodeError JDE.JsonDecodeError

data JsonDecodeErrorInt
  = TypeMismatch String
  | UnexpectedValue Arg.Json
  | AtIndex Int JsonDecodeErrorInt
  | AtKey String JsonDecodeErrorInt
  | Named String JsonDecodeErrorInt
  | MissingValue

decodeErrTypeMismatch :: String -> JDE.JsonDecodeError
decodeErrTypeMismatch = JDE.TypeMismatch

decodeFailTypeMismatch
  :: forall x. String -> Either.Either JDE.JsonDecodeError x
decodeFailTypeMismatch = Either.Left <<< JDE.TypeMismatch

instance Show JsonDecodeError where
  show (JsonDecodeError jde) = show jde

decodeRtoI :: JDE.JsonDecodeError -> JsonDecodeErrorInt
decodeRtoI (JDE.TypeMismatch s) = TypeMismatch s
decodeRtoI (JDE.UnexpectedValue j) = UnexpectedValue j
decodeRtoI (JDE.AtIndex i e) = AtIndex i $ decodeRtoI e
decodeRtoI (JDE.AtKey s e) = AtKey s $ decodeRtoI e
decodeRtoI (JDE.Named s e) = Named s $ decodeRtoI e
decodeRtoI JDE.MissingValue = MissingValue

decodeItoR :: JsonDecodeErrorInt -> JDE.JsonDecodeError
decodeItoR (TypeMismatch s) = JDE.TypeMismatch s
decodeItoR (UnexpectedValue j) = JDE.UnexpectedValue j
decodeItoR (AtIndex i e) = JDE.AtIndex i $ decodeItoR e
decodeItoR (AtKey s e) = JDE.AtKey s $ decodeItoR e
decodeItoR (Named s e) = JDE.Named s $ decodeItoR e
decodeItoR MissingValue = JDE.MissingValue

derive instance Generic.Generic JsonDecodeError _
derive instance
  Generic.Generic JsonDecodeErrorInt _

instance Dec.DecodeJson JsonDecodeErrorInt where
  decodeJson x = DecodeGeneric.genericDecodeJson x

instance Enc.EncodeJson JsonDecodeErrorInt where
  encodeJson x = EncodeGeneric.genericEncodeJson x

instance Dec.DecodeJson JsonDecodeError where
  decodeJson x = do
    j :: JsonDecodeErrorInt <- ADec.decodeJson x
    pure $ JsonDecodeError $ decodeItoR j

instance Enc.EncodeJson JsonDecodeError where
  encodeJson (JsonDecodeError x) = Enc.encodeJson $ decodeRtoI x

type JsonDecodeFn t = Arg.Json -> Either.Either JsonDecodeError t
type JsonEncodeFn t = t -> Arg.Json

jsonDecode :: String -> Either.Either JsonDecodeError Arg.Json
jsonDecode = Dec.fromJsonString >>> Z.mapL JsonDecodeError

decodeJson'
  :: forall v
   . Dec.DecodeJson v
  => Proxy.Proxy v
  -> Arg.Json
  -> Either.Either JsonDecodeError v
decodeJson' _ = ADec.decodeJson >>> Z.mapL JsonDecodeError

decode'
  :: forall v
   . Dec.DecodeJson v
  => Proxy.Proxy v
  -> String
  -> Either.Either JsonDecodeError v
decode' _ = Dec.fromJsonString >>> Z.mapL JsonDecodeError

decodeJson
  :: forall v
   . Dec.DecodeJson v
  => Arg.Json
  -> Either.Either JsonDecodeError v
decodeJson = ADec.decodeJson >>> Z.mapL JsonDecodeError

baseDecodeJson
  :: forall v
   . Dec.DecodeJson v
  => Arg.Json
  -> Either.Either ADec.JsonDecodeError v
baseDecodeJson = ADec.decodeJson

decode
  :: forall @v
   . Dec.DecodeJson v
  => String
  -> Either.Either JsonDecodeError v
decode = Dec.fromJsonString >>> Z.mapL JsonDecodeError

encode
  :: forall v
   . Enc.EncodeJson v
  => v
  -> String
encode v = AArg.stringify $ Enc.encodeJson v

type Type_Ap :: forall k1 k2. (k1 -> k2) -> k1 -> k2
type Type_Ap f x = f x

type Type_Ap_R :: forall k1 k2. k1 -> (k1 -> k2) -> k2
type Type_Ap_R x f = f x

infixr 0 type Type_Ap as $
infixr 0 type Type_Ap_R as #

data SorN = SorN_S String | SorN_I Int

class IsStringOrNum a where
  sOrN :: a -> SorN

derive instance Generic.Generic SorN _
derive instance Eq SorN
derive instance Ord SorN

instance Dec.DecodeJson SorN where
  decodeJson x = DecodeGeneric.genericDecodeJson x

instance Enc.EncodeJson SorN where
  encodeJson x = EncodeGeneric.genericEncodeJson x

instance IsStringOrNum String where
  sOrN = SorN_S

instance IsStringOrNum Int where
  sOrN = SorN_I

instance Show SorN where
  show (SorN_S s) = s
  show (SorN_I i) = show i

arg2' :: forall a1 a2 r. a2 -> (a1 -> a2 -> r) -> (a1 -> r)
arg2' a2 f a1 = f a1 a2

arg3' :: forall a1 a2 a3 r. a3 -> (a1 -> a2 -> a3 -> r) -> (a1 -> a2 -> r)
arg3' a3 f a1 a2 = f a1 a2 a3

arg4'
  :: forall a1 a2 a3 a4 r
   . a4
  -> (a1 -> a2 -> a3 -> a4 -> r)
  -> (a1 -> a2 -> a3 -> r)
arg4' a4 f a1 a2 a3 = f a1 a2 a3 a4

data ResourceStage = Acquire | Release

derive instance Generic.Generic ResourceStage _

instance Dec.DecodeJson ResourceStage where
  decodeJson x = DecodeGeneric.genericDecodeJson x

instance Enc.EncodeJson ResourceStage where
  encodeJson x = EncodeGeneric.genericEncodeJson x