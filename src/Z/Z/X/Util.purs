module Z.Z.X.Util
  ( Run'
  , Run_
  , XEff(..)
  , XEffTag
  , XEffTagged
  , evalTagged
  , eval_
  , tagEffX
  , useTag
  , xDo
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

newtype XEff a = XEff (Run_ a)

derive instance Newtype (XEff a) _

xDo :: forall x a. XEff a -> R.Run x a
xDo = pure <<< eval_ <<< unwrap

data XEffTagged :: forall k. k -> Type -> Type
data XEffTagged t a = XEffTagged (Effect a)

data XEffTag :: forall @k. k -> Type
data XEffTag t = XEffTag

evalTagged :: forall t a. XEffTag t -> XEffTagged t a -> a
evalTagged _ (XEffTagged eff) = Unsafe.unsafePerformEffect eff

useTag :: forall @t v a. ((XEffTagged t a -> a) -> v) -> v
useTag runTagged = runTagged $ evalTagged $ XEffTag @t

tagEffX :: forall @tag a. Effect a -> XEffTagged tag a
tagEffX eff = XEffTagged eff