module Z.Z.Util
  ( JsonDecodeError(..)
  , JsonDecodeFn
  , JsonEncodeFn
  , StringOrNum
  , Type_Ap
  , Type_Ap_R
  , arg2'
  , arg3'
  , arg4'
  , arrReverse
  , arrSort
  , arrSortBy
  , arrSortWith
  , asOrNum
  , asStringOr
  , auto
  , class Defaultable
  , decode
  , decode'
  , decodeJson
  , decodeJson'
  , default
  , discard
  , encode
  , id
  , jsonDecode
  , jsonKeys
  , jsonLookup
  , jsonPairs
  , jsonSortedPairs
  , jsonStr
  , jsonVals
  , mapL
  , nth
  , simpleHash
  , stringOrNumString
  , type (#)
  , type ($)
  , unwrap
  ) where

import Prelude

import Control.Promise (Promise) as Promise
import Control.Promise (toAff)
import Data.Argonaut.Core (stringify) as AArg
import Data.Argonaut.Core as Arg
import Data.Argonaut.Decode (JsonDecodeError(..)) as JDE
import Data.Argonaut.Decode (class DecodeJson, fromJsonString) as Dec
import Data.Argonaut.Decode as ADec
import Data.Argonaut.Decode.Generic (genericDecodeJson) as DecodeGeneric
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Enc
import Data.Argonaut.Encode.Generic (genericEncodeJson) as EncodeGeneric
import Data.Array as Array
import Data.Either as Either
import Data.Generic.Rep (class Generic) as Generic
import Data.Lens as Lens
import Data.Maybe as Maybe
import Data.Monoid as Monoid
import Data.Ord as Ord
import Data.Ordering as Ordering
import Data.Tuple as Tup
import Effect (Effect) as Effect
import Effect.Aff as Aff
import Effect.Class (liftEffect) as EffectClass
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as FO
import Run as Run
import Run.Except as RunE
import Run.Reader as RunR
import Run.State as RunS
import Run.Writer as RunW
import Type.Proxy as Proxy
import Z.Z.Core as Core

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

mapL :: forall l1 l2 r. (l1 -> l2) -> Either.Either l1 r -> Either.Either l2 r
mapL f = Either.either (\x -> Either.Left $ f x) Either.Right

discard :: forall m i. Monad m => m i -> m Unit
discard = map $ const unit

class Defaultable a where
  default :: a

instance defaultUnit :: Defaultable Unit where
  default = unit

instance defaultJust :: Defaultable (Maybe.Maybe a) where
  default = Maybe.Nothing

else instance defaultApplicable ::
  ( Defaultable v
  , Applicative a
  ) =>
  Defaultable (a v) where
  default = pure default

auto :: forall d r. Defaultable d => (d -> r) -> r
auto f = f default

unwrap :: forall d. Defaultable d => Maybe.Maybe d -> d
unwrap m = Maybe.fromMaybe default m

foreign import js_jsonStr :: Arg.Json -> String

jsonStr :: Arg.Json -> String
jsonStr = js_jsonStr

foreign import js_simpleHash :: String -> Int

simpleHash :: String -> Int
simpleHash = js_simpleHash

newtype JsonDecodeError = JsonDecodeError JDE.JsonDecodeError

data JsonDecodeErrorInt
  = TypeMismatch String
  | UnexpectedValue Arg.Json
  | AtIndex Int JsonDecodeErrorInt
  | AtKey String JsonDecodeErrorInt
  | Named String JsonDecodeErrorInt
  | MissingValue

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

derive instance genericJsonJsonDecodeError :: Generic.Generic JsonDecodeError _
derive instance genericJsonJsonDecodeErrorInt ::
  Generic.Generic JsonDecodeErrorInt _

instance decodeJsonJsonDecodeErrorInt :: Dec.DecodeJson JsonDecodeErrorInt where
  decodeJson x = DecodeGeneric.genericDecodeJson x

instance encodeJsonJsonDecodeErrorInt :: Enc.EncodeJson JsonDecodeErrorInt where
  encodeJson x = EncodeGeneric.genericEncodeJson x

instance decodeJsonJsonDecodeError :: Dec.DecodeJson JsonDecodeError where
  decodeJson x = do
    j :: JsonDecodeErrorInt <- ADec.decodeJson x
    pure $ JsonDecodeError $ decodeItoR j

instance encodeJsonJsonDecodeError :: Enc.EncodeJson JsonDecodeError where
  encodeJson (JsonDecodeError x) = Enc.encodeJson $ decodeRtoI x

type JsonDecodeFn t = Arg.Json -> Either.Either JsonDecodeError t
type JsonEncodeFn t = t -> Arg.Json

jsonDecode :: String -> Either.Either JsonDecodeError Arg.Json
jsonDecode = Dec.fromJsonString >>> mapL JsonDecodeError

decodeJson'
  :: forall v
   . Dec.DecodeJson v
  => Proxy.Proxy v
  -> Arg.Json
  -> Either.Either JsonDecodeError v
decodeJson' _ = ADec.decodeJson >>> mapL JsonDecodeError

decode'
  :: forall v
   . Dec.DecodeJson v
  => Proxy.Proxy v
  -> String
  -> Either.Either JsonDecodeError v
decode' _ = Dec.fromJsonString >>> mapL JsonDecodeError

decodeJson
  :: forall v
   . Dec.DecodeJson v
  => Arg.Json
  -> Either.Either JsonDecodeError v
decodeJson = ADec.decodeJson >>> mapL JsonDecodeError

decode
  :: forall v
   . Dec.DecodeJson v
  => String
  -> Either.Either JsonDecodeError v
decode = Dec.fromJsonString >>> mapL JsonDecodeError

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

type StringOrNum = Either.Either String Number

asOrNum :: String -> StringOrNum
asOrNum s = Either.Left s

asStringOr :: Number -> StringOrNum
asStringOr n = Either.Right n

stringOrNumString :: StringOrNum -> String
stringOrNumString (Either.Left s) = s
stringOrNumString (Either.Right n) = show n

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

foreign import js_timeout :: Int -> Effect.Effect (Promise.Promise Unit)
