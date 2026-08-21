module Z.Z.HashMap
  ( HashMap
  , hm'empty
  , hm'entries
  , hm'fromFoldable
  , hm'has
  , hm'keys
  , hm'lookup
  , hm'set
  , hm'size
  , hm'vals
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Maybe (Maybe, isJust)
import Data.Tuple.Nested (type (/\), (/\))
import Z.Z.Bin as Bin
import Z.Z.Core (arr'fromFoldable)
import Z.Z.Key (class Keyed)

type HashMap k v = Bin.Bin { k :: k, v :: v }

hm'empty :: forall @k @v. Keyed k => HashMap k v
hm'empty = Bin.bin'empty

hm'set :: forall @k @v. Keyed k => k -> v -> HashMap k v -> HashMap k v
hm'set k v = Bin.bin'insert k { k, v }

hm'fromFoldable
  :: forall @f @k @v. Keyed k => Foldable f => f (k /\ v) -> HashMap k v
hm'fromFoldable f =
  Bin.bin'fromFoldable $ arr'fromFoldable f <#> \(k /\ v) -> k /\ { k, v }

hm'size :: forall @k @v. Keyed k => HashMap k v -> Int
hm'size = Bin.bin'size

hm'lookup :: forall @k @v. Keyed k => k -> HashMap k v -> Maybe v
hm'lookup v s = Bin.bin'lookup v s <#> _.v

hm'has :: forall @k @v. Keyed k => k -> HashMap k v -> Boolean
hm'has v s = isJust $ Bin.bin'lookup v s

hm'entries :: forall @k @v. Keyed k => HashMap k v -> Array (k /\ v)
hm'entries hm = Bin.bin'vals hm <#> \{ k, v } -> k /\ v

hm'keys :: forall @k @v. Keyed k => HashMap k v -> Array k
hm'keys hm = Bin.bin'vals hm <#> \{ k } -> k

hm'vals :: forall @k @v. Keyed k => HashMap k v -> Array v
hm'vals hm = Bin.bin'vals hm <#> \{ v } -> v