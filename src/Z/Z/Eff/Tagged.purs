module Z.Z.Eff.Tagged
  ( Eff'Tagged
  , eff'evalTagged
  , eff'tag
  , eff'tagWith
  , eff'untag
  , eff'useTag
  ) where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy)
import Z.Z.Wraps (class Unwraps, class Wraps, wrapped'from, wrapped'get)

data Eff'Tagged :: forall k. k -> Type -> Type
data Eff'Tagged t a = Eff'Tagged (Effect a)

derive instance Functor (Eff'Tagged t)

instance Wraps (Eff'Tagged t a) (Effect a) where
  wrapped'get (Eff'Tagged e) = e

instance Unwraps (Eff'Tagged t a) Unit (Effect a) where
  wrapped'mk e _ = (Eff'Tagged e)
  wrapped'rest _ = unit

eff'evalTagged :: forall @t a. Eff'Tagged t a -> a
eff'evalTagged = unsafePerformEffect <<< wrapped'get

eff'useTag :: forall @t v a. ((Eff'Tagged t a -> a) -> v) -> v
eff'useTag runTagged = runTagged $ eff'evalTagged @t

eff'tag :: forall @tag a. Effect a -> Eff'Tagged tag a
eff'tag = wrapped'from

eff'tagWith :: forall tag a. Proxy tag -> Effect a -> Eff'Tagged tag a
eff'tagWith _ = wrapped'from

eff'untag
  :: forall @tag a. forall t. TypeEquals t tag => Eff'Tagged t a -> Effect a
eff'untag = wrapped'get