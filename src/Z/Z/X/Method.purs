module Z.Z.X.Method
  ( XM'assign
  , class X'Method
  , x'call
  ) where

import Prelude

import Data.Identity (Identity(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Type.Proxy (Proxy)
import Z.EXTERNAL.RUN.Run (Run)
import Z.EXTERNAL.RUN.Run.Reader (Reader, asksAt)
import Z.Z.Core (class ConsSymbol)
import Z.Z.Method (class Method, call)
import Z.Z.X.Core (R'Tagged(..), RW'Tagged(..), W'Tagged(..))

class X'Method :: forall k. k -> (Type -> Type) -> Type -> Type -> Constraint
class X'Method name m param res | name m -> param res where
  x'call :: forall p x' x. ConsSymbol p m x' x => Proxy p -> param -> Run x res

class Eff'Method :: forall k. k -> Type -> Type -> Type -> Constraint
class Eff'Method name t param res | name t -> param res where
  eff'call :: t -> param -> Effect res

class Eff'RW'Method :: forall k. k -> Type -> Type -> Type -> Constraint
class Eff'RW'Method name t param res | name t -> param res where
  eff'rw'call :: t -> param -> Effect res

class Eff'R'Method :: forall k. k -> Type -> Type -> Type -> Constraint
class Eff'R'Method name t param res | name t -> param res where
  eff'r'call :: t -> param -> Effect res

class Eff'W'Method :: forall k. k -> Type -> Type -> Type -> Constraint
class Eff'W'Method name t param res | name t -> param res where
  eff'w'call :: t -> param -> Effect res

instance
  ( Method name r param res
  ) =>
  X'Method name (Reader (Identity r)) param res where
  x'call p param = asksAt p \(Identity r) -> call @name r param

else instance
  ( Eff'R'Method name r param res
  ) =>
  X'Method name (Reader (R'Tagged r)) param res where
  x'call p param =
    asksAt p \(R'Tagged r) -> unsafePerformEffect $ eff'r'call @name r param

else instance
  ( Eff'W'Method name r param res
  ) =>
  X'Method name (Reader (W'Tagged r)) param res where
  x'call p param =
    asksAt p \(W'Tagged r) -> unsafePerformEffect $ eff'w'call @name r param

else instance
  ( Eff'RW'Method name r param res
  ) =>
  X'Method name (Reader (RW'Tagged r)) param res where
  x'call p param =
    asksAt p \(RW'Tagged r) -> unsafePerformEffect $ eff'rw'call @name r param

else instance
  ( Eff'Method name r param res
  ) =>
  X'Method name (Reader r) param res where
  x'call p param = asksAt p \r -> unsafePerformEffect $ eff'call @name r param

foreign import data XM'assign :: Type