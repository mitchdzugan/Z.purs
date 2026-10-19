module Z.Z.Boxed
  ( Boxed
  , boxable'mkResponds
  , boxed'call
  , boxed'call_
  , boxed'mk
  , boxed'v
  , class Boxable
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Functor.Variant (VariantF, inj)
import Data.Symbol (reflectSymbol)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Type.Proxy (Proxy(..))
import Z.Z.Core (class ConsSymbol, eff'defer)
import Z.Z.Defaultable (class Generable, GDefault, default)
import Z.Z.Id (class Identable, ident'key)
import Z.Z.X.Responds (Responds, responds'id)

foreign import data Boxed :: Type -> Type

class Boxable :: forall k. Type -> k -> (Type -> Type) -> Constraint
class Boxable tag value responds | tag -> value responds where
  boxable'mkResponds :: forall a. Boxed tag -> responds a -> a

boxed'mk
  :: forall tag value responds. Boxable tag value responds => value -> Boxed tag
boxed'mk = js_newBoxed

boxed'v
  :: forall tag value responds. Boxable tag value responds => Boxed tag -> value
boxed'v = js_boxedVal

boxed'call
  :: forall @method tag value args res resp' resp
   . ConsSymbol method (Responds args res) resp' resp
  => Boxable tag value (VariantF resp)
  => Identable args
  => Boxed tag
  -> args
  -> res
boxed'call b args =
  let
    proxy = Proxy @method
    varf = inj proxy (responds'id args)
    mk = eff'defer \_ -> boxable'mkResponds @tag b varf
    fnAndArgId = ident'key $ reflectSymbol proxy /\ args
  in
    js_boxedGetCachedOrEval fnAndArgId mk b

boxed'call_
  :: forall @method tag value args res resp' resp
   . ConsSymbol method (Responds args res) resp' resp
  => Boxable tag value (VariantF resp)
  => Identable args
  => Generable args GDefault args
  => Boxed tag
  -> res
boxed'call_ b = boxed'call @method b default

instance (Boxable tag value responds, Eq value) => Eq (Boxed tag) where
  eq b1 b2 = boxed'v b1 == boxed'v b2

instance (Boxable tag val resp, EncodeJson val) => EncodeJson (Boxed tag) where
  encodeJson = encodeJson <<< boxed'v

instance (Boxable tag val resp, DecodeJson val) => DecodeJson (Boxed tag) where
  decodeJson = map boxed'mk <<< decodeJson

foreign import js_newBoxed
  :: forall tag value. value -> Boxed tag

foreign import js_boxedVal :: forall tag value. Boxed tag -> value

foreign import js_boxedGetCachedOrEval
  :: forall v res. String -> (Effect res) -> Boxed v -> res