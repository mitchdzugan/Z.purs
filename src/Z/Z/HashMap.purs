module Z.Z.HashMap
  ( HashMap(..)
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

import Data.Argonaut.Decode as Dec
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe, isJust)
import Data.Tuple.Nested (type (/\), (/\))
import Z.Z.Bin as Bin
import Z.Z.Core (arr'fromFoldable)
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (class Newtype)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable)

newtype HashMap k v = HashMap (Bin.Bin { k :: k, v :: v })

derive instance Newtype (HashMap k v) _

instance Functor (HashMap k) where
  map f (HashMap hm) = HashMap $ hm <#> \d -> { k: d.k, v: f d.v }

instance Identable k => Generable (HashMap k v) gdesc (HashMap k v) where
  mkGenerable = hm'empty

instance (Z.EncodeJson { k :: k, v :: v }) => Z.EncodeJson (HashMap k v) where
  encodeJson = Z.encodeJson <<< Z.unwrap

instance (Z.DecodeJson { k :: k, v :: v }) => Z.DecodeJson (HashMap k v) where
  decodeJson v = Z.wrap <$> Dec.decodeJson v

hm'empty :: forall @k @v. Identable k => HashMap k v
hm'empty = Z.wrap Bin.bin'empty

hm'set :: forall @k @v. Identable k => k -> v -> HashMap k v -> HashMap k v
hm'set k v = Z.wrap <<< Bin.bin'insert k { k, v } <<< Z.unwrap

hm'fromFoldable
  :: forall @f @k @v. Identable k => Foldable f => f (k /\ v) -> HashMap k v
hm'fromFoldable f =
  Z.wrap $ Bin.bin'fromFoldable $ arr'fromFoldable f <#> \(k /\ v) -> k /\
    { k, v }

hm'size :: forall @k @v. Identable k => HashMap k v -> Int
hm'size = Bin.bin'size <<< Z.unwrap

hm'lookup :: forall @k @v. Identable k => k -> HashMap k v -> Maybe v
hm'lookup v (HashMap hm) = Bin.bin'lookup v hm <#> _.v

hm'has :: forall @k @v. Identable k => k -> HashMap k v -> Boolean
hm'has v (HashMap hm) = isJust $ Bin.bin'lookup v hm

hm'entries :: forall @k @v. Identable k => HashMap k v -> Array (k /\ v)
hm'entries (HashMap hm) = Bin.bin'vals hm <#> \{ k, v } -> k /\ v

hm'keys :: forall @k @v. Identable k => HashMap k v -> Array k
hm'keys (HashMap hm) = Bin.bin'vals hm <#> \{ k } -> k

hm'vals :: forall @k @v. Identable k => HashMap k v -> Array v
hm'vals (HashMap hm) = Bin.bin'vals hm <#> \{ v } -> v

