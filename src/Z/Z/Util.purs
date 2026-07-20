module Z.Z.Util
  ( A
  , E
  , EA
  , ID
  , JsonDecodeError(..)
  , JsonDecodeFn
  , JsonEncodeFn
  , ModX
  , R
  , RA
  , RW
  , RWE
  , RWEA
  , RWS
  , RWSE
  , RWSEA
  , Result
  , RunX
  , S
  , SE
  , SEA
  , StringOrNum
  , Type_Ap
  , Type_Ap_R
  , W
  , WE
  , WEA
  , WS
  , WSE
  , WSEA
  , X
  , X_op
  , X_op'
  , X_op_R
  , X_op_R'
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
  , effectPromiseToAff
  , effectPromiseX
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
  , promiseToAff
  , simpleHash
  , stringOrNumString
  , type (#)
  , type (#@>)
  , type ($)
  , type (<@$)
  , type (<@)
  , type (@>)
  , unwrap
  , xAff
  , xAsk
  , xEff
  , xEval
  , xEvalAff
  , xExec
  , xExecAff
  , xFail
  , xHush
  , xMod
  , xOk
  , xRead
  , xReading
  , xResult
  , xTell
  , xTimeout
  , xTry
  , xUnwrap
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
import Z.Z.X as X

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

xBase :: forall a x. a <@$ x -> Run.Run x a
xBase = X.runEff

xEval :: forall a. a <@$ () -> a
xEval r = unsafePerformEffect $ Run.runBaseEffect $ Run.expand $ xBase r

xExec :: forall e a. a <@ E e $ () -> Either.Either e a
xExec = xEval <<< xTry

xEvalAff :: forall a. a <@ A $ () -> Aff.Aff a
xEvalAff x = Run.match { aff: \(X.AffCmd a) -> a } # Run.run $ xBase x

xExecAff :: forall e a. a <@ EA e $ () -> Aff.Aff $ Either.Either e a
xExecAff = xEvalAff <<< xTry

xReading :: forall x r a. r -> Run.Run (RunR.READER r x) a -> Run.Run x a
xReading = RunR.runReader

xAsk :: forall x r. Run.Run (RunR.READER r x) r
xAsk = RunR.ask

xRead :: forall x r a. Lens.Lens' r a -> Run.Run (RunR.READER r x) a
xRead l = RunR.ask <#> Lens.view l

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

promiseToAff :: forall a. Promise.Promise a -> Aff.Aff a
promiseToAff = toAff

effectPromiseToAff :: forall a. Effect.Effect (Promise.Promise a) -> Aff.Aff a
effectPromiseToAff e = EffectClass.liftEffect e >>= promiseToAff

effectPromiseX
  :: forall a x
   . Effect.Effect $ Promise.Promise a
  -> a <@ EA Core.JsError $ x
effectPromiseX = effectPromiseToAff >>> xAff >=> xOk

type ID :: forall k. k -> k
type ID a = a

xTry
  :: forall x e a. Run.Run (RunE.EXCEPT e x) a -> Run.Run x (Either.Either e a)
xTry = RunE.runExcept

xHush
  :: forall x e a. Run.Run (RunE.EXCEPT e x) a -> Run.Run x (Maybe.Maybe a)
xHush m = xTry m <#> Either.hush

xFail :: forall x e a. e -> Run.Run (RunE.EXCEPT e x) a
xFail e = RunE.throw e

xOk :: forall x e a. Either.Either e a -> Run.Run (RunE.EXCEPT e x) a
xOk (Either.Left e) = xFail e
xOk (Either.Right a) = pure a

xAff
  :: forall f x. Aff.Aff f -> Either.Either Core.JsError f <@ A $ x
xAff a = Aff.attempt a # X.aff <#> mapL Core.JsError

xEff
  :: forall f x. Effect.Effect f -> Either.Either Core.JsError f <@ A $ x
xEff a = EffectClass.liftEffect a # xAff

type RunX m r x = Run.Run (m (X.EFF x)) r

type X_op r m x = RunX m r x
type X_op_R m r x = RunX m r x
type X_op' x r = RunX ID r x
type X_op_R' r x = RunX ID r x

infixr 1 type X_op as <@
infixr 1 type X_op_R as @>

infixr 1 type X_op' as #@>
infixr 1 type X_op_R' as <@$

type X x m r = X_op r m x

type Type_Ap :: forall k1 k2. (k1 -> k2) -> k1 -> k2
type Type_Ap f x = f x

type Type_Ap_R :: forall k1 k2. k1 -> (k1 -> k2) -> k2
type Type_Ap_R x f = f x

infixr 0 type Type_Ap as $
infixr 0 type Type_Ap_R as #

type R r x = X.R r x
type RA r x = X.R r (X.A x)
type RW r w x = X.R r (X.W w x)
type RWS r w s x = X.R r (X.W w (X.S s x))
type RWE r w e x = X.R r (X.W w (X.E e x))
type RWSE r w s e x = X.R r (X.W w (X.S s (X.E e x)))
type RWSEA r w s e x = X.R r (X.W w (X.S s (X.E e (X.A x))))
type RWEA r w e x = X.R r (X.W w (X.E e (X.A x)))
type W w x = X.W w x
type WS w s x = X.W w (X.S s x)
type WSE w s e x = X.W w (X.S s (X.E e x))
type WE w e x = X.W w (X.E e x)
type WSEA w s e x = X.W w (X.S s (X.E e (X.A x)))
type WEA w e x = X.W w (X.E e (X.A x))
type S s x = X.S s x
type SE s e x = X.S s (X.E e x)
type SEA s e x = X.S s (X.E e (X.A x))

type E :: forall k. Type -> Row (k -> Type) -> Row (k -> Type)
type E e x = X.E e x

type EA e x = X.E e (X.A x)
type A x = X.A x

type ModX a = Run.Run (X.S a ()) Unit

xMod :: forall a. a -> ModX a -> a
xMod init m = Run.extract $ RunS.execState init m

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

type Result w e a = { w :: (Array w), v :: (Either.Either e a) }

xResult :: forall x w e a. x # WE (Array w) e @> a -> x #@> Result w e a
xResult m = do
  w <- RunW.runWriter $ RunE.runExcept m
  pure $ { w: (Tup.fst w), v: (Tup.snd w) }

xUnwrap :: forall x e a. e -> Maybe.Maybe a -> x # E e @> a
xUnwrap _ (Maybe.Just a) = pure a
xUnwrap e _ = xFail e

xTell :: forall x w. Monoid.Monoid w => w -> x # W w @> Unit
xTell w = RunW.tell w

foreign import js_timeout :: Int -> Effect.Effect (Promise.Promise Unit)

xTimeout :: forall x. Int -> X x A Unit
xTimeout ms = discard $ xTry $ effectPromiseX $ js_timeout ms