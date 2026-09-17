module Z.Z.HashSet
  ( HashSet(..)
  , hs'add
  , hs'empty
  , hs'fromFoldable
  , hs'has
  , hs'size
  , hs'vals
  ) where

import Prelude

import Data.Argonaut.Decode as Dec
import Data.Foldable (class Foldable)
import Data.Maybe (isJust)
import Data.Tuple.Nested ((/\))
import Z.Z.Bin as Bin
import Z.Z.Core (arr'fromFoldable)
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (class Newtype)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable, class Identable'Functor)

newtype HashSet a = HashSet (Bin.Bin a)

derive instance Newtype (HashSet a) _

instance Identable'Functor HashSet where
  identable'map f hs = hs'fromFoldable $ hs'vals hs <#> f

instance Identable a => Generable (HashSet a) gdesc (HashSet a) where
  mkGenerable = hs'empty

instance (Z.EncodeJson a) => Z.EncodeJson (HashSet a) where
  encodeJson = Z.encodeJson <<< Z.unwrap

instance (Z.DecodeJson a) => Z.DecodeJson (HashSet a) where
  decodeJson v = Z.wrap <$> Dec.decodeJson v

hs'empty :: forall @a. Identable a => HashSet a
hs'empty = Z.wrap $ Bin.bin'empty

hs'add :: forall @a. Identable a => a -> HashSet a -> HashSet a
hs'add v = Z.wrap <<< Bin.bin'insert v v <<< Z.unwrap

hs'fromFoldable :: forall @f @a. Identable a => Foldable f => f a -> HashSet a
hs'fromFoldable f = Z.wrap $ Bin.bin'fromFoldable $ arr'fromFoldable f <#>
  \v -> v /\ v

hs'size :: forall @a. Identable a => HashSet a -> Int
hs'size = Bin.bin'size <<< Z.unwrap

hs'has :: forall @a. Identable a => a -> HashSet a -> Boolean
hs'has v s = isJust $ Bin.bin'lookup v $ Z.unwrap s

hs'vals :: forall @a. Identable a => HashSet a -> Array a
hs'vals = Bin.bin'vals <<< Z.unwrap
