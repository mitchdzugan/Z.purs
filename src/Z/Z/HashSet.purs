module Z.Z.HashSet
  ( HashSet
  , hs'add
  , hs'empty
  , hs'fromFoldable
  , hs'has
  , hs'size
  , hs'vals
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Maybe (isJust)
import Data.Tuple.Nested ((/\))
import Z.Z.Bin as Bin
import Z.Z.Core (arr'fromFoldable)
import Z.Z.Key (class Keyed)

type HashSet a = Bin.Bin a

hs'empty :: forall @a. Keyed a => HashSet a
hs'empty = Bin.bin'empty

hs'add :: forall @a. Keyed a => a -> HashSet a -> HashSet a
hs'add v = Bin.bin'insert v v

hs'fromFoldable :: forall @f @a. Keyed a => Foldable f => f a -> HashSet a
hs'fromFoldable f = Bin.bin'fromFoldable $ arr'fromFoldable f <#> \v -> v /\ v

hs'size :: forall @a. Keyed a => HashSet a -> Int
hs'size = Bin.bin'size

hs'has :: forall @a. Keyed a => a -> HashSet a -> Boolean
hs'has v s = isJust $ Bin.bin'lookup v s

hs'vals :: forall @a. Keyed a => HashSet a -> Array a
hs'vals = Bin.bin'vals