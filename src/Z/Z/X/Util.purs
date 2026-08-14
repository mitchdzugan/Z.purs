module Z.Z.X.Util
  ( Run'
  , Run_
  , XEffTag
  , Eff'At
  , evalTagged
  , eval_
  , tagEffX
  , useTag
  ) where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Unsafe as Unsafe
import Run as R

eval_ :: forall a. Run_ a -> a
eval_ m = Unsafe.unsafePerformEffect $ R.runBaseEffect $ R.expand m

type Run' x = R.Run x Unit

type Run_ a = R.Run () a

data Eff'At :: forall k. k -> Type -> Type
data Eff'At t a = Eff'At (Effect a)

data XEffTag :: forall @k. k -> Type
data XEffTag t = XEffTag

evalTagged :: forall t a. XEffTag t -> Eff'At t a -> a
evalTagged _ (Eff'At eff) = Unsafe.unsafePerformEffect eff

useTag :: forall @t v a. ((Eff'At t a -> a) -> v) -> v
useTag runTagged = runTagged $ evalTagged $ XEffTag @t

tagEffX :: forall @tag a. Effect a -> Eff'At tag a
tagEffX eff = Eff'At eff