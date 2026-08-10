module Z.Z.Defaultable.Generable
  ( G1
  , G2
  , GDefault
  , class G2OrDefault
  , class GOrDefault
  , class Generable
  , g
  , g'
  , g1
  , g2
  , mkGenerable
  ) where

import Prelude

import Data.Maybe as May

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

class Generable :: forall @k1 @k2. k1 -> k2 -> Type -> Constraint
class Generable tag gspec v | tag gspec -> v where
  mkGenerable :: v

instance Generable String _gdesc String where
  mkGenerable = ""

instance Generable Unit _gdesc Unit where
  mkGenerable = unit

instance Generable (Array a) _gdesc (Array a) where
  mkGenerable = []

instance Generable (May.Maybe a) _gdesc (May.Maybe a) where
  mkGenerable = May.Nothing

g
  :: forall @tag v
   . Generable tag GDefault v
  => v
g = mkGenerable @tag @GDefault

g'
  :: forall @tag @gdesc v
   . Generable tag gdesc v
  => v
g' = mkGenerable @tag @gdesc

g1
  :: forall @tag @t1 v. Generable tag (G1 t1) v => v
g1 = mkGenerable @tag @(G1 t1)

g2
  :: forall @tag @t1 @t2 v
   . Generable tag (G2 t1 t2) v
  => v
g2 = mkGenerable @tag @(G2 t1 t2)