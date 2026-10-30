module Z.Z.Boxed where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Effect (Effect)
import Z.Z.Core (eff'defer)
import Z.Z.Defaultable (class Generable, GDefault, default)
import Z.Z.Id (class Identable, ident'key)
import Z.Z.Method (class Method, call, method'id)

foreign import data Boxed :: Type -> Type

boxed'mk :: forall value. value -> Boxed value
boxed'mk = js_newBoxed

boxed'v :: forall value. Boxed value -> value
boxed'v = js_boxedVal

boxed'call
  :: forall @name value param res
   . Method name (Boxed value) param res
  => Identable param
  => Boxed value
  -> param
  -> res
boxed'call b args =
  let
    methodId = method'id @name @(Boxed value)
    paramId = ident'key args
    mk = eff'defer \_ -> call @name b args
  in
    js_boxedGetCachedOrEval methodId paramId mk b

boxed'call_
  :: forall @name value param res
   . Method name (Boxed value) param res
  => Identable param
  => Generable param GDefault param
  => Boxed value
  -> res
boxed'call_ b = boxed'call @name b default

instance (Eq value) => Eq (Boxed value) where
  eq b1 b2 = boxed'v b1 == boxed'v b2

instance (EncodeJson value) => EncodeJson (Boxed value) where
  encodeJson = encodeJson <<< boxed'v

instance (DecodeJson value) => DecodeJson (Boxed value) where
  decodeJson = map boxed'mk <<< decodeJson

foreign import js_newBoxed
  :: forall value. value -> Boxed value

foreign import js_boxedVal :: forall value. Boxed value -> value

foreign import js_boxedGetCachedOrEval
  :: forall v res. Int -> String -> (Effect res) -> Boxed v -> res