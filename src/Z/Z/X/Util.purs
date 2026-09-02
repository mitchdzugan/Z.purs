module Z.Z.X.Util
  ( Eff'At
  , Run'
  , Run_
  , XEffTag
  , eff'untag
  , evalTagged
  , eval_
  , eff'tag
  , useTag
  ) where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Unsafe as Unsafe
import Run as R
import Type.Equality (class TypeEquals)

eval_ :: forall a. Run_ a -> a
eval_ m = R.extract m

type Run' x = R.Run x Unit

type Run_ a = R.Run () a

data Eff'At :: forall k. k -> Type -> Type
data Eff'At t a = Eff'At (Effect a)

derive instance Functor (Eff'At t)

data XEffTag :: forall @k. k -> Type
data XEffTag t = XEffTag

evalTagged :: forall t a. XEffTag t -> Eff'At t a -> a
evalTagged _ (Eff'At eff) = Unsafe.unsafePerformEffect eff

useTag :: forall @t v a. ((Eff'At t a -> a) -> v) -> v
useTag runTagged = runTagged $ evalTagged $ XEffTag @t

eff'tag :: forall @tag a. Effect a -> Eff'At tag a
eff'tag eff = Eff'At eff

eff'untag :: forall @tag a. forall t. TypeEquals t tag => Eff'At t a -> Effect a
eff'untag (Eff'At e) = e