module Z.Z.X.Util
  ( Run'
  , Run_
  , XEff(..)
  , eval_
  , xDo
  ) where

import Prelude

import Data.Newtype (class Newtype, unwrap)
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
