module Z.Z.Util
  ( JsonDecodeError(..)
  , JsonDecodeFn
  , JsonEncodeFn
  , RawJsonDecodeError
  , ResourceStage(..)
  , SorN(..)
  , Type_Ap
  , Type_Ap_R
  , UrlParts
  , arg2'
  , arg3'
  , arg4'
  , arr'first
  , arr'last
  , arr'reverse
  , arr'sort
  , arr'sortBy
  , arr'sortWith
  , arr'splitHead
  , arr'splitTail
  , baseDecodeJson
  , class IsStringOrNum
  , class RevSym
  , class SplitSp1
  , class SplitSp1Impl
  , class UpCat
  , class UpCf
  , class UpCt
  , decode
  , decode'
  , decodeErrTypeMismatch
  , decodeFailTypeMismatch
  , decodeForeign
  , decodeJson
  , decodeJson'
  , decodeViaString
  , encode
  , id
  , jsonDecode
  , jsonKeys
  , jsonLookup
  , jsonPairs
  , jsonSortedPairs
  , jsonVals
  , nth
  , obj'fromHM
  , sOrN
  , type (#)
  , type ($)
  , url'fromParts
  , url'fromString
  , url'hash
  , url'origin
  , url'parts
  , url'pathSegments
  , url'pathSegmentsFromPathname
  , url'pathname
  , url'query
  , url'relative
  , url'toString
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
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord as Ord
import Data.Ordering as Ordering
import Data.Tuple as Tup
import Data.Tuple.Nested (type (/\), (/\))
import Foreign as Foreign
import Foreign.Object as FO
import Prim.Symbol as Symbol
import Type.Proxy as Proxy
import Z.Z.Core (arr'drop, arr'size, arr'take)
import Z.Z.Core as Z
import Z.Z.HashMap (HashMap, hm'entries)
import Z.Z.Id (class Identable, id'of)
import Z.Z.Url as Url

type UrlParts = Url.Parts

url'fromParts :: Url.Parts'from -> Url.URL
url'fromParts = Url.fromParts

url'fromString :: String -> Maybe.Maybe Url.URL
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
  rest (Maybe.Just n) = Url.host url <> ":" <> show n
  rest _ = Url.host url

url'pathSegments :: Url.URL -> Array String
url'pathSegments = Url.path

url'hash :: Url.URL -> Maybe.Maybe String
url'hash = Url.hash

url'pathname :: Url.URL -> String
url'pathname = Url.pathname

url'relative :: Url.URL -> String
url'relative = Url.relative

url'parts :: Url.URL -> UrlParts
url'parts = Url.parts

nth :: forall a. Array a -> Int -> Maybe.Maybe a
nth = Array.index

arr'first :: forall a. Array a -> Maybe.Maybe a
arr'first a = nth a 0

arr'splitHead :: forall a. Array a -> Maybe.Maybe (a /\ Array a)
arr'splitHead a = arr'first a <#> flip (/\) (arr'drop 1 a)

arr'splitTail :: forall a. Array a -> Maybe.Maybe (a /\ Array a)
arr'splitTail a = arr'last a <#> flip (/\) (arr'take ((arr'size a) - 1) a)

arr'last :: forall a. Array a -> Maybe.Maybe a
arr'last a = nth a ((Array.length a) - 1)

arr'sort :: forall a. Ord.Ord a => Array a -> Array a
arr'sort = Array.sort

arr'sortBy :: forall a. (a -> a -> Ordering.Ordering) -> Array a -> Array a
arr'sortBy = Array.sortBy

arr'sortWith :: forall a b. Ord.Ord b => (a -> b) -> Array a -> Array a
arr'sortWith = Array.sortWith

arr'reverse :: forall a. Array a -> Array a
arr'reverse = Array.reverse

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

type RawJsonDecodeError = JDE.JsonDecodeError

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
decodeRtoI _ = MissingValue

decodeItoR :: JsonDecodeErrorInt -> JDE.JsonDecodeError
decodeItoR (TypeMismatch s) = JDE.TypeMismatch s
decodeItoR (UnexpectedValue j) = JDE.UnexpectedValue j
decodeItoR (AtIndex i e) = JDE.AtIndex i $ decodeItoR e
decodeItoR (AtKey s e) = JDE.AtKey s $ decodeItoR e
decodeItoR (Named s e) = JDE.Named s $ decodeItoR e
decodeItoR _ = JDE.MissingValue

decodeViaString
  :: forall a
   . (String -> Either.Either String a)
  -> Arg.Json
  -> Either.Either JDE.JsonDecodeError a
decodeViaString ofString x = do
  (baseDecodeJson x <#> ofString) >>= onEor
  where
  onEor (Either.Right v) = pure v
  onEor (Either.Left msg) = decodeFailTypeMismatch msg

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

decodeForeign
  :: forall @v
   . Dec.DecodeJson v
  => Foreign.Foreign
  -> Either.Either JsonDecodeError v
decodeForeign = Z.jsonFromForeign >>> ADec.decodeJson >>> Z.mapL JsonDecodeError

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
  :: forall @v
   . Dec.DecodeJson v
  => Arg.Json
  -> Either.Either JsonDecodeError v
decodeJson = ADec.decodeJson >>> Z.mapL JsonDecodeError

baseDecodeJson
  :: forall @v
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

newtype SorN = SorN (Either.Either String Int)

derive instance Newtype SorN _

class IsStringOrNum a where
  sOrN :: a -> SorN

derive instance Eq SorN
derive instance Ord SorN

instance Dec.DecodeJson SorN where
  decodeJson x = case ADec.decodeJson x of
    Either.Right v -> pure $ wrap $ Either.Left v
    _ -> ADec.decodeJson x <#> wrap <<< Either.Right

instance Enc.EncodeJson SorN where
  encodeJson = unwrap >>> Either.either Enc.encodeJson Enc.encodeJson

instance IsStringOrNum String where
  sOrN = wrap <<< Either.Left

instance IsStringOrNum Int where
  sOrN = wrap <<< Either.Right

instance IsStringOrNum SorN where
  sOrN = identity

instance Show SorN where
  show = unwrap >>> Either.either identity show

instance Identable SorN where
  ident'get = id'of <<< show

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

class SplitSp1 :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class SplitSp1 i o1 o2 | i -> o1 o2

instance (SplitSp1Impl i "" "" "f" o1 o2) => SplitSp1 i o1 o2

class SplitSp1Impl
  :: forall k1 k2 k3 k4 k5 k6. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> Constraint
class SplitSp1Impl sym cat cf ct tat tf | sym cat cf ct -> tat tf

class UpCat :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Constraint
class UpCat c cat ct cat' | c cat ct -> cat'

instance UpCat " " cat ct cat
else instance UpCat c cat "t" cat
else instance (Symbol.Cons c cat cat') => UpCat c cat "f" cat'

class UpCf :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Constraint
class UpCf c cf ct cf' | c cf ct -> cf'

instance UpCf c cf "f" cf
else instance (Symbol.Cons c cf cf') => UpCf c cf "t" cf'

class UpCt :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class UpCt c ct ct' | c ct -> ct'

instance UpCt " " ct "t"
else instance UpCt c ct ct

class RevSym :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class RevSym s c s' | s c -> s'

instance RevSym "" c c
else instance
  ( Symbol.Cons c1 s' s
  , Symbol.Cons c1 c c'
  , RevSym s' c' f
  ) =>
  RevSym s c f

instance
  ( RevSym cf "" tf
  , RevSym cat "" tat
  ) =>
  SplitSp1Impl "" cat cf ct tat tf
else instance
  ( Symbol.Cons c s' s
  , UpCat c cat ct cat'
  , UpCf c cf ct cf'
  , UpCt c ct ct'
  , SplitSp1Impl s' cat' cf' ct' tat tf
  ) =>
  SplitSp1Impl s cat cf ct tat tf

obj'fromHM :: forall @a. HashMap String a -> FO.Object a
obj'fromHM = FO.fromFoldable <<< hm'entries
