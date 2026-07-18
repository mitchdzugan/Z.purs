module Z
  ( A
  , E
  , EA
  , JsonDecodeError(..)
  , JsonDecodeFn
  , JsonEncodeFn
  , ModX
  , R
  , RW
  , RWS
  , RWSE
  , RWSEA
  , S
  , SE
  , SEA
  , StringOrNum
  , W
  , WS
  , WSE
  , WSEA
  , X
  , X'
  , arg2'
  , arg3'
  , arg4'
  , asOrNum
  , asStringOr
  , decode
  , decode'
  , decodeJson
  , decodeJson'
  , discard
  , effectPromiseToAff
  , effectPromiseX
  , encode
  , id
  , jsonDecode
  , mapL
  , module Aff
  , module Arg
  , module CA
  , module Core
  , module DC
  , module Dec
  , module DecodeGeneric
  , module Effect
  , module EffectClass
  , module Either
  , module Enc
  , module EncodeGeneric
  , module Generic
  , module Lens
  , module LensRecord
  , module Maybe
  , module Promise
  , module Proxy
  , module Record
  , module Run
  , module RunE
  , module RunS
  , module Symbol
  , module XX
  , promiseToAff
  , stringOrNumString
  , xMod
  ) where

import Prelude

import Control.Promise (Promise) as Promise
import Control.Promise (toAff)
import Core (JsError(..), jsErrorMessage, jsErrorName, jsErrorStack) as Core
import Data.Argonaut.Core (Json, caseJsonString, caseJsonNumber, fromString, jsonNull) as Arg
import Data.Argonaut.Core (stringify) as AArg
import Data.Argonaut.Decode (JsonDecodeError(..)) as JDE
import Data.Argonaut.Decode (class DecodeJson, fromJsonString) as Dec
import Data.Argonaut.Decode as ADec
import Data.Argonaut.Decode.Generic (genericDecodeJson) as DecodeGeneric
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Enc
import Data.Argonaut.Encode.Generic (genericEncodeJson) as EncodeGeneric
import Data.Codec (Codec, Codec') as DC
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (Either(..), either) as Either
import Data.Generic.Rep (class Generic) as Generic
import Data.Lens (Lens, Lens') as Lens
import Data.Lens.Record (prop) as LensRecord
import Data.Maybe (Maybe(..)) as Maybe
import Data.Symbol (class IsSymbol, reifySymbol, reflectSymbol) as Symbol
import Effect (Effect) as Effect
import Effect.Aff (Aff, launchAff, launchAff_) as Aff
import Effect.Class (liftEffect) as EffectClass
import Record (merge, get, set, modify) as Record
import Run (Run, extract) as Run
import Run.Except (runExcept) as RunE
import Run.State (execState) as RunS
import Type.Proxy (Proxy(..)) as Proxy
import X (pass, tryAff, result, RunX, R, W, S, E, A, e_map, s_set, runBaseAff, logInfo) as X
import X (pass, tryAff, result, e_map, s_set, runBaseAff, logInfo) as XX

id :: forall a. a -> a
id a = a

mapL :: forall l1 l2 r. (l1 -> l2) -> Either.Either l1 r -> Either.Either l2 r
mapL f = Either.either (\x -> Either.Left $ f x) Either.Right

discard :: forall m i. Monad m => m i -> m Unit
discard = map $ const unit

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
   . Effect.Effect (Promise.Promise a)
  -> X (EA Core.JsError x) a
effectPromiseX = X.tryAff <<< effectPromiseToAff

type X m r = X.RunX m r
type X' m = X m Unit

type R r x = X.R r x
type RW r w x = X.R r (X.W w x)
type RWS r w s x = X.R r (X.W w (X.S s x))
type RWSE r w s e x = X.R r (X.W w (X.S s (X.E e x)))
type RWSEA r w s e x = X.R r (X.W w (X.S s (X.E e (X.A x))))
type W w x = X.W w x
type WS w s x = X.W w (X.S s x)
type WSE w s e x = X.W w (X.S s (X.E e x))
type WSEA w s e x = X.W w (X.S s (X.E e (X.A x)))
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
