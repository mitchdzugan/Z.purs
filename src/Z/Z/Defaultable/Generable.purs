module Z.Z.Defaultable.Generable
  ( G1
  , G2
  , GDefault
  , GWrapped1Tag
  , GWrappedNTag
  , Generable
  , class G2OrDefault
  , class GOrDefault
  , class GTagMap
  , class GTaggedDefaultable
  , class GenerableC
  , class HasGTag
  , g
  , g'
  , g1
  , g2
  , gTaggedDefault
  , mkGenerable
  ) where

import Prelude

import Control.Monad as Monad
import Data.Maybe as May

class GTaggedDefaultable :: forall @k. k -> Type -> Constraint
class GTaggedDefaultable ga a | ga -> a where
  gTaggedDefault :: a

data GWrapped1Tag :: forall k1 k2. k1 -> k2 -> Type
data GWrapped1Tag tag t1 = GWrapped1Tag

data GWrappedNTag :: forall k1 k2. k1 -> k2 -> Type
data GWrappedNTag tag ts = GWrappedNTag

data Generable :: forall @k. k -> Type
data Generable t = Generable

class HasGTag :: forall k1 k2. k1 -> k2 -> Constraint
class HasGTag tIn tagOut | tIn -> tagOut

class GTagMap :: forall k1 k2. k1 -> k2 -> Constraint
class GTagMap tagIn tagOut | tagIn -> tagOut

instance HasGTag (Generable tag) tag
else instance GTagMap tagIn tagOut => HasGTag tagIn tagOut

instance
  ( GenerableC tagOut (G1 gspec) a
  , HasGTag tagIn tagOut
  ) =>
  GTaggedDefaultable (GWrapped1Tag tagIn gspec) a where
  gTaggedDefault = mkGenerable @tagOut @(G1 gspec)
else instance
  ( GenerableC tagOut gspec a
  , HasGTag tagIn tagOut
  ) =>
  GTaggedDefaultable (GWrappedNTag tagIn gspec) a where
  gTaggedDefault = mkGenerable @tagOut @gspec
else instance
  ( GenerableC tagOut GDefault a
  , HasGTag tagIn tagOut
  ) =>
  GTaggedDefaultable tagIn a where
  gTaggedDefault = mkGenerable @tagOut @GDefault

data G2 :: forall @k1 @k2. k1 -> k2 -> Type
data G2 t1 t2 = G2

data G1 :: forall @k1. k1 -> Type
data G1 t1 = G1

data GDefault

----------------------------------

class GOrDefault :: forall @k1 @k2 @k3. k1 -> k2 -> k3 -> Constraint
class GOrDefault s i o | s i -> o

instance GOrDefault s GDefault s

instance GOrDefault s (G1 GDefault) s
else instance GOrDefault s (G1 t1) t1

instance GOrDefault s (G2 GDefault _t2) s
else instance GOrDefault s (G2 t1 _t2) t1

----------------------------------

class G2OrDefault :: forall @k1 @k2 @k3. k1 -> k2 -> k3 -> Constraint
class G2OrDefault s i o | s i -> o

instance G2OrDefault s GDefault s
instance G2OrDefault s (G1 _t1) s
instance G2OrDefault s (G2 _t1 GDefault) s
else instance G2OrDefault s (G2 _t1 t2) t2

----------------------------------

class GenerableC :: forall @k1 @k2. k1 -> k2 -> Type -> Constraint
class GenerableC tag gspec v | tag gspec -> v where
  mkGenerable :: v

instance GenerableC String _gdesc String where
  mkGenerable = ""

instance GenerableC Unit _gdesc Unit where
  mkGenerable = unit

instance GenerableC (Array a) _gdesc (Array a) where
  mkGenerable = []

instance GenerableC (May.Maybe a) _gdesc (May.Maybe a) where
  mkGenerable = May.Nothing

g
  :: forall @tspec tag v
   . HasGTag tspec tag
  => GenerableC tag GDefault v
  => v
g = mkGenerable @tag @GDefault

g'
  :: forall @tspec @gdesc tag v
   . HasGTag tspec tag
  => GenerableC tag gdesc v
  => v
g' = mkGenerable @tag @gdesc

g1
  :: forall @tspec @t1 tag v. HasGTag tspec tag => GenerableC tag (G1 t1) v => v
g1 = mkGenerable @tag @(G1 t1)

g2
  :: forall @tspec @t1 @t2 tag v
   . HasGTag tspec tag
  => GenerableC tag (G2 t1 t2) v
  => v
g2 = mkGenerable @tag @(G2 t1 t2)