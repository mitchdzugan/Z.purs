module Z.Z.Defaultable.Generable
  ( GDescDefault
  , GenerableNickname
  , Generable
  , GWrappedTag(..)
  , class GOrDefault
  , class GTaggedDefaultable
  , class GenerableC
  , class GenerableNicknameC
  , gTaggedDefault
  , mkGenerable
  , type (@@)
  ) where

import Z.Z.Defaultable.Core (class Defaultable, default)

class GTaggedDefaultable :: forall @k. k -> Type -> Constraint
class GTaggedDefaultable ga a | ga -> a where
  gTaggedDefault :: a

data Generable :: forall k. k -> Type
data Generable a = Generable

data GWrappedTag :: forall k1 k2. k1 -> k2 -> Type
data GWrappedTag tag gdesc = GWrappedTag

infixr 0 type GWrappedTag as @@

instance
  ( GenerableC tag GDescDefault a
  ) =>
  GTaggedDefaultable (Generable tag) a where
  gTaggedDefault = mkGenerable @tag @GDescDefault
else instance
  ( GenerableC tagOut GDescDefault a
  , GenerableNicknameC tagIn tagOut
  ) =>
  GTaggedDefaultable (GenerableNickname tagIn) a where
  gTaggedDefault = mkGenerable @tagOut @GDescDefault
else instance
  ( GenerableC tag gspec a
  ) =>
  GTaggedDefaultable (GWrappedTag (Generable tag) gspec) a where
  gTaggedDefault = mkGenerable @tag @gspec
else instance
  ( GenerableC tagOut gspec a
  , GenerableNicknameC tagIn tagOut
  ) =>
  GTaggedDefaultable (GWrappedTag (GenerableNickname tagIn) gspec) a where
  gTaggedDefault = mkGenerable @tagOut @gspec
else instance
  ( Defaultable a
  ) =>
  GTaggedDefaultable a a where
  gTaggedDefault = default

data GDescDefault = GDescDefault

class GOrDefault :: forall @k1 @k2 @k3. k1 -> k2 -> k3 -> Constraint
class GOrDefault s i o | s i -> o

instance GOrDefault s GDescDefault s
else instance GOrDefault s t t

class GenerableC :: forall @k1 @k2. k1 -> k2 -> Type -> Constraint
class GenerableC tag gspec v | tag gspec -> v where
  mkGenerable :: v

class GenerableNicknameC :: forall @k1 @k2. k1 -> k2 -> Constraint
class GenerableNicknameC tagIn tagOut | tagIn -> tagOut

data GenerableNickname :: forall @k. k -> Type
data GenerableNickname tagIn

