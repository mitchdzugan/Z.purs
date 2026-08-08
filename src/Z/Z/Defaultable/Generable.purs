module Z.Z.Defaultable.Generable
  ( GAt
  , GDescAt(..)
  , GDescDefault(..)
  , GTag
  , Generable
  , GenerableP
  , GenerableW
  , class GDefaultable
  , class GOrDefault_
  , class GTaggedDefaultable
  , class GenerableC
  , class GenerableWUnwrap
  , gDefault
  , gTaggedDefault
  , mkGenerable
  ) where

import Prelude

import Z.Z.Defaultable.Core (class Defaultable, default)

class GDefaultable :: forall @k. k -> Type -> Constraint
class GDefaultable ga a | ga -> a where
  gDefault :: a

class GTaggedDefaultable :: forall @k. k -> Type -> Constraint
class GTaggedDefaultable ga a | ga -> a where
  gTaggedDefault :: a

data GTag :: forall k. k -> Type
data GTag a = GTag

instance
  ( GDefaultable ga a
  ) =>
  GTaggedDefaultable (GTag ga) a where
  gTaggedDefault = gDefault @ga
else instance
  ( Defaultable a
  ) =>
  GTaggedDefaultable a a where
  gTaggedDefault = default

data GDescAt at = GDescAt
data GDescDefault = GDescDefault

class GOrDefault_ s i o | s i -> o

instance GOrDefault_ s (GDescAt t) t
instance GOrDefault_ s GDescDefault s

data GenerableP t = GenerableP

type Generable t = GTag (GenerableP t)

data GenerableW w = GenerableW

class GenerableWUnwrap w tag gspec | w -> tag gspec

newtype GAt at tag = GAt tag

instance GenerableWUnwrap (GAt at tag) (GDescAt at) tag

class GenerableC :: forall @k1 @k2. k1 -> k2 -> Type -> Constraint
class GenerableC tag gspec v | tag gspec -> v where
  mkGenerable :: v

instance
  ( GenerableC tag GDescDefault v
  ) =>
  GDefaultable (GenerableP tag) v where
  gDefault = mkGenerable @tag @GDescDefault

else instance
  ( GenerableC tag gspec v
  , GenerableWUnwrap w tag gspec
  ) =>
  GDefaultable (GenerableW w) v where
  gDefault = mkGenerable @tag @gspec
