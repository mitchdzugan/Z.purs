module Z.Z.HashMap
  ( HashMap(..)
  , hm'alter
  , hm'empty
  , hm'entries
  , hm'fromFoldable
  , hm'has
  , hm'insert
  , hm'keys
  , hm'lookup
  , hm'merge
  , hm'modify
  , hm'remove
  , hm'replace
  , hm'set
  , hm'size
  , hm'vals
  ) where

import Prelude

import Data.Argonaut.Decode as Dec
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple.Nested (type (/\), (/\))
import Z.Z.Bin as Bin
import Z.Z.Core (arr'concat, arr'fromFoldable, arr'remove)
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (class Newtype)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable, ident'key)

newtype HashMap k v = HashMap (Bin.Bin { k :: k, v :: v })

derive instance Newtype (HashMap k v) _

instance Eq v => Eq (HashMap k v) where
  eq hm1 hm2 = false

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

--------------------------------- MUTABLE ------------------------------------

hm'merge :: forall @k @v. Identable k => Array (HashMap k v) -> HashMap k v
hm'merge hms = hm'fromFoldable $ arr'concat $ map hm'entries hms

hm'insert
  :: forall @k @v. Eq v => Identable k => k -> v -> HashMap k v -> HashMap k v
hm'insert k v hm = if willNotChange then hm else rebuilt unit
  where
  willNotChange = Just v == hm'lookup k hm
  rebuilt _ = hm'fromFoldable $ arr'concat [ hm'entries hm, [ k /\ v ] ]

hm'remove
  :: forall @k @v. Identable k => k -> HashMap k v -> HashMap k v
hm'remove k hm = if willNotChange then hm else rebuilt unit
  where
  willNotChange = isNothing $ hm'lookup k hm
  kString = ident'key k
  pred (kIn /\ _) = ident'key kIn == kString
  rebuilt _ = hm'fromFoldable $ arr'remove pred $ hm'entries hm

hm'replace
  :: forall @k @v. Eq v => Identable k => k -> v -> HashMap k v -> HashMap k v
hm'replace k v hm = if lacksK then hm else hm'insert k v hm
  where
  lacksK = isNothing $ hm'lookup k hm

hm'alter
  :: forall @k @v
   . Eq v
  => Identable k
  => k
  -> (Maybe v -> Maybe v)
  -> HashMap k v
  -> HashMap k v
hm'alter k f hm = case f $ hm'lookup k hm of
  Nothing -> hm'remove k hm
  Just v -> hm'insert k v hm

hm'modify
  :: forall @k @v
   . Eq v
  => Identable k
  => k
  -> (v -> v)
  -> HashMap k v
  -> HashMap k v
hm'modify k f = hm'alter k $ map f

--------------------------------------------------------------------

instance (Ord k, Ord v, Identable k) => Ord (HashMap k v) where
  compare a b = compare (hm'entries a) (hm'entries b)