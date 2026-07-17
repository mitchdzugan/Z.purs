module Z
  ( ModX
  , StringOrNum
  , X_a
  , X_a_
  , X_e
  , X_e_
  , X_ea
  , X_ea_
  , X_r
  , X_r_
  , X_ra
  , X_ra_
  , X_re
  , X_re_
  , X_rs
  , X_rs_
  , X_rw
  , X_rw_
  , X_s
  , X_s_
  , X_sa
  , X_sa_
  , X_se
  , X_se_
  , X_w
  , X_w_
  , X_wa
  , X_wa_
  , X_we
  , X_we_
  , X_ws
  , X_ws_
  , Xa
  , Xa_
  , Xe
  , Xe_
  , Xea
  , Xea_
  , Xr
  , Xr_
  , Xra
  , Xra_
  , Xre
  , Xre_
  , Xrs
  , Xrs_
  , Xrw
  , Xrw_
  , Xs
  , Xs_
  , Xsa
  , Xsa_
  , Xse
  , Xse_
  , Xw
  , Xw_
  , Xwa
  , Xwa_
  , Xwe
  , Xwe_
  , Xws
  , Xws_
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
  , module X
  , ofStringOrNum
  , promiseToAff
  , xMod
  , xUpdate
  ) where

import Prelude

import Control.Promise (Promise) as Promise
import Control.Promise (toAff)
import Core
  ( JsError(..)
  , jsErrorMessage
  , jsErrorName
  , jsErrorStack
  ) as Core
import Data.Argonaut.Core (Json, caseJsonString, caseJsonNumber, fromString, jsonNull) as Arg
import Data.Argonaut.Core (stringify) as AArg
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), fromJsonString) as Dec
import Data.Argonaut.Decode as ADec
import Data.Argonaut.Decode.Generic (genericDecodeJson) as DecodeGeneric
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Enc
import Data.Argonaut.Encode.Generic (genericEncodeJson) as EncodeGeneric
import Data.Codec (Codec, Codec') as DC
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (Either(..)) as Either
import Data.Generic.Rep (class Generic) as Generic
import Data.Lens (Lens, Lens') as Lens
import Data.Lens.Record (prop) as LensRecord
import Data.Maybe (Maybe(..)) as Maybe
import Effect (Effect) as Effect
import Effect.Aff (Aff, launchAff, launchAff_) as Aff
import Effect.Class (liftEffect) as EffectClass
import Record (merge, get, set, modify) as Record
import Run (Run, extract) as Run
import Run.Except (runExcept) as RunE
import Run.State (execState) as RunS
import Type.Proxy (Proxy(..)) as Proxy
import Data.Symbol (class IsSymbol, reifySymbol, reflectSymbol) as Symbol
import X (pass, tryAff, result, X, R, W, S, E, A, e_map, s_set, runBaseAff, logInfo) as X

id :: forall a. a -> a
id a = a

discard :: forall m i. Monad m => m i -> m Unit
discard = map $ const unit

jsonDecode :: String -> Either.Either Dec.JsonDecodeError Arg.Json
jsonDecode = Dec.fromJsonString

decodeJson'
  :: forall v
   . Dec.DecodeJson v
  => Proxy.Proxy v
  -> Arg.Json
  -> Either.Either Dec.JsonDecodeError v
decodeJson' _ = ADec.decodeJson

decode'
  :: forall v
   . Dec.DecodeJson v
  => Proxy.Proxy v
  -> String
  -> Either.Either Dec.JsonDecodeError v
decode' _ = Dec.fromJsonString

decodeJson
  :: forall v
   . Dec.DecodeJson v
  => Arg.Json
  -> Either.Either Dec.JsonDecodeError v
decodeJson = ADec.decodeJson

decode
  :: forall v
   . Dec.DecodeJson v
  => String
  -> Either.Either Dec.JsonDecodeError v
decode = Dec.fromJsonString

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
  -> Xea x Core.JsError a
effectPromiseX = X.tryAff <<< effectPromiseToAff

type Xr x r a = X.X (X.R r x) a
type Xw x w a = X.X (X.W w x) a
type Xs x s a = X.X (X.S s x) a
type Xe x e a = X.X (X.E e x) a
type Xa x a = X.X (X.A x) a
type Xrw x r w a = X.X (X.W w (X.R r x)) a
type Xrs x r s a = X.X (X.S s (X.R r x)) a
type Xre x r e a = X.X (X.E e (X.R r x)) a
type Xra x r a = X.X (X.A (X.R r x)) a
type Xws x w s a = X.X (X.S s (X.W w x)) a
type Xwe x w e a = X.X (X.E e (X.W w x)) a
type Xwa x w a = X.X (X.A (X.W w x)) a
type Xse x s e a = X.X (X.E e (X.S s x)) a
type Xsa x s a = X.X (X.A (X.S s x)) a
type Xea x e a = X.X (X.A (X.E e x)) a
type X_r r a = X.X (X.R r ()) a
type X_w w a = X.X (X.W w ()) a
type X_s s a = X.X (X.S s ()) a
type X_e e a = X.X (X.E e ()) a
type X_a a = X.X (X.A ()) a
type X_rw r w a = X.X (X.W w (X.R r ())) a
type X_rs r s a = X.X (X.S s (X.R r ())) a
type X_re r e a = X.X (X.E e (X.R r ())) a
type X_ra r a = X.X (X.A (X.R r ())) a
type X_ws w s a = X.X (X.S s (X.W w ())) a
type X_we w e a = X.X (X.E e (X.W w ())) a
type X_wa w a = X.X (X.A (X.W w ())) a
type X_se s e a = X.X (X.E e (X.S s ())) a
type X_sa s a = X.X (X.A (X.S s ())) a
type X_ea e a = X.X (X.A (X.E e ())) a

type Xr_ x r = X.X (X.R r x) Unit
type Xw_ x w = X.X (X.W w x) Unit
type Xs_ x s = X.X (X.S s x) Unit
type Xe_ x e = X.X (X.E e x) Unit
type Xa_ x = X.X (X.A x) Unit
type Xrw_ x r w = X.X (X.W w (X.R r x)) Unit
type Xrs_ x r s = X.X (X.S s (X.R r x)) Unit
type Xre_ x r e = X.X (X.E e (X.R r x)) Unit
type Xra_ x r = X.X (X.A (X.R r x)) Unit
type Xws_ x w s = X.X (X.S s (X.W w x)) Unit
type Xwe_ x w e = X.X (X.E e (X.W w x)) Unit
type Xwa_ x w = X.X (X.A (X.W w x)) Unit
type Xse_ x s e = X.X (X.E e (X.S s x)) Unit
type Xsa_ x s = X.X (X.A (X.S s x)) Unit
type Xea_ x e = X.X (X.A (X.E e x)) Unit
type X_r_ r = X.X (X.R r ()) Unit
type X_w_ w = X.X (X.W w ()) Unit
type X_s_ s = X.X (X.S s ()) Unit
type X_e_ e = X.X (X.E e ()) Unit
type X_a_ = X.X (X.A ()) Unit
type X_rw_ r w = X.X (X.W w (X.R r ())) Unit
type X_rs_ r s = X.X (X.S s (X.R r ())) Unit
type X_re_ r e = X.X (X.E e (X.R r ())) Unit
type X_ra_ r = X.X (X.A (X.R r ())) Unit
type X_ws_ w s = X.X (X.S s (X.W w ())) Unit
type X_we_ w e = X.X (X.E e (X.W w ())) Unit
type X_wa_ w = X.X (X.A (X.W w ())) Unit
type X_se_ s e = X.X (X.E e (X.S s ())) Unit
type X_sa_ s = X.X (X.A (X.S s ())) Unit
type X_ea_ e = X.X (X.A (X.E e ())) Unit

xUpdate :: forall a. a -> Run.Run (X.S a ()) Unit -> a
xUpdate init m = Run.extract $ RunS.execState init m

type ModX a = Run.Run (X.S a ()) Unit

xMod :: forall a. a -> ModX a -> a
xMod init m = Run.extract $ RunS.execState init m

type StringOrNum = Either.Either String Number

asOrNum :: String -> StringOrNum
asOrNum s = Either.Left s

asStringOr :: Number -> StringOrNum
asStringOr n = Either.Right n

ofStringOrNum :: StringOrNum -> String
ofStringOrNum (Either.Left s) = s
ofStringOrNum (Either.Right n) = show n

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