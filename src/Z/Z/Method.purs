module Z.Z.Method
  ( call
  , class Method
  , method'id
  ) where

foreign import js_getMethodId :: forall param res. (param -> res) -> Int

class Method :: forall k. k -> Type -> Type -> Type -> Constraint
class Method name t param res | name t -> param res where
  call :: t -> param -> res

method'id :: forall @name @t param res. Method name t param res => Int
method'id = js_getMethodId (call @name @t)