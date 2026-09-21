module Z.Z.X.Async
  ( AffF(..)
  , x'aff''
  ) where

import Prelude

import Effect.Aff (Aff)
import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Z.Z.Core (class ConsSymbol)

data AffF a = AffCmd (Aff a)

derive instance Functor AffF

x'aff'' :: forall @p f x' x. ConsSymbol p AffF x' x => (Aff f) -> Run x f
x'aff'' f = lift (Proxy @p) (AffCmd f)

