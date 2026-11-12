module Z.Z.Eff.Array
  ( Eff'Array
  , Eff'PartialArray
  , eff'array'clear
  , eff'array'cons
  , eff'array'empty
  , eff'array'freeze
  , eff'array'getPartial
  , eff'array'new
  , eff'array'reset
  ) where

import Prelude

import Effect (Effect)

data Eff'Array :: forall k. k -> Type
data Eff'Array t

data Eff'PartialArray :: forall k. k -> Type
data Eff'PartialArray t

foreign import js_effArray_new :: forall t. Array t -> Effect (Eff'Array t)

foreign import js_effArray_getPartial :: Eff'Array ~> Eff'PartialArray

foreign import js_effArray_freeze :: forall t. Eff'Array t -> Effect (Array t)

foreign import js_effArray_clear :: forall t. Eff'Array t -> Effect Unit

foreign import js_effArray_reset :: forall t. Eff'Array t -> Effect Unit

foreign import js_effArray_cons :: forall t. t -> Eff'Array t -> Effect Unit

eff'array'new :: forall @t. Array t -> Effect (Eff'Array t)
eff'array'new = js_effArray_new

eff'array'empty :: forall @t. Effect (Eff'Array t)
eff'array'empty = js_effArray_new []

eff'array'getPartial :: Eff'Array ~> Eff'PartialArray
eff'array'getPartial = js_effArray_getPartial

eff'array'freeze :: forall t. Eff'Array t -> Effect (Array t)
eff'array'freeze = js_effArray_freeze

eff'array'clear :: forall t. Eff'Array t -> Effect Unit
eff'array'clear = js_effArray_clear

eff'array'reset :: forall t. Eff'Array t -> Effect Unit
eff'array'reset = js_effArray_reset

eff'array'cons :: forall t. t -> Eff'Array t -> Effect Unit
eff'array'cons = js_effArray_cons