module Z.Z.Defaultable.Generable
  ( D'Int'0
  , D'Int'1
  , G1
  , G2
  , GDefault
  , class DefaultValueRecord
  , class G2OrDefault
  , class GOrDefault
  , class Generable
  , g
  , g'
  , g1
  , g2
  , mkDefaultRecord
  , mkGenerable
  ) where

import Prelude

import Data.List (List(..))
import Data.Maybe as May
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row (class Cons)
import Prim.RowList as RL
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))

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

instance Generable (List a) _gdesc (List a) where
  mkGenerable = Nil

instance Generable (Proxy a) _gdesc (Proxy a) where
  mkGenerable = Proxy

instance
  ( Generable l GDefault l
  , Generable r GDefault r
  ) =>
  Generable (l /\ r) _gdesc (l /\ r) where
  mkGenerable = mkGenerable @l @GDefault /\ mkGenerable @r @GDefault

class DefaultValueRecord :: RL.RowList Type -> Row Type -> Constraint
class DefaultValueRecord rowList row | rowList -> row where
  mkDefaultRecord :: Proxy rowList -> Record row

instance DefaultValueRecord RL.Nil () where
  mkDefaultRecord _ = {}

instance
  ( IsSymbol key
  , Generable focus GDefault focus
  , Cons key focus rowTail row
  , DefaultValueRecord rowListTail rowTail
  ) =>
  DefaultValueRecord (RL.Cons key focus rowListTail) row where
  mkDefaultRecord _ = insert (mkGenerable @focus @GDefault) tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    insert = unsafeSet key :: focus -> Record rowTail -> Record row
    tail = mkDefaultRecord (Proxy :: Proxy rowListTail)

instance defaultValueForRecord ::
  ( RL.RowToList row list
  , DefaultValueRecord list row
  ) =>
  Generable (Record row) gdesc (Record row) where
  mkGenerable = mkDefaultRecord (Proxy :: Proxy list)

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

---------------------------------------------------------------------

foreign import data D'Int'0 :: Type
foreign import data D'Int'1 :: Type

instance Generable D'Int'1 _gdesc Int where
  mkGenerable = 1

instance Generable D'Int'0 _gdesc Int where
  mkGenerable = 0