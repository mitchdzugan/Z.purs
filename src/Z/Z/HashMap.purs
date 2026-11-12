module Z.Z.HashMap
  ( HM'El
  , HashMap(..)
  , hm'alter
  , hm'collect
  , hm'empty
  , hm'entries
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
  , hm2d'collect
  ) where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Z.Z.Bin as Bin
import Z.Z.Core (arr'concat, arr'remove)
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (fst, snd)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable, ident'key)
import Z.Z.It
  ( class Collectable
  , class Iterable
  , Object'Els
  , collect
  , it'collect
  , iterable'mk
  , objEls'collect
  )
import Z.Z.Json.Decode (class XDecodeJson)

type HM'El k v = { k :: k, v :: v }

newtype HashMap k v = HashMap (Bin.Bin (HM'El k v))

derive instance Z.Newtype (HashMap k v) _

derive newtype instance XDecodeJson (HM'El k v) => XDecodeJson (HashMap k v)

instance Eq v => Eq (HashMap k v) where
  eq hm1 hm2 = false

instance Functor (HashMap k) where
  map f (HashMap hm) = HashMap $ hm <#> \d -> { k: d.k, v: f d.v }

instance Identable k => Generable (HashMap k v) gdesc (HashMap k v) where
  mkGenerable = hm'empty

instance (Z.EncodeJson (HM'El k v)) => Z.EncodeJson (HashMap k v) where
  encodeJson = Z.encodeJson <<< Z.unwrap

instance Iterable (HashMap k v) (k /\ v) where
  iterable'mk it =
    map (\r -> r.k /\ r.v) $ iterable'mk $ Z.unwrap $ Z.unwrap it

instance
  Identable k =>
  Collectable HashMap (k /\ v) (HashMap k v) where
  collect it = Z.wrap $ Z.wrap $ collect @Object $
    it <#> \(k /\ v) -> ident'key k /\ { k, v }

foreign import data HashMap'2D :: Type

instance
  ( Identable k1
  , Identable k2
  ) =>
  Collectable HashMap'2D (k1 /\ k2 /\ v) (HashMap k1 (HashMap k2 v)) where
  collect it = hm2d
    where
    objEls = objEls'collect $ it <#> \(k1 /\ p) -> ident'key k1 /\ (k1 /\ p)
    hmEls = objEls <#> \((k /\ p1) /\ es) -> { k, v: [ p1 ] <> map snd es }
    hm2d = Z.wrap (Z.wrap hmEls) <#> it'collect @HashMap

hm'empty :: forall @k @v. Identable k => HashMap k v
hm'empty = Z.wrap Bin.bin'empty

hm'set :: forall @k @v. Identable k => k -> v -> HashMap k v -> HashMap k v
hm'set k v = Z.wrap <<< Bin.bin'insert k { k, v } <<< Z.unwrap

hm'collect
  :: forall it k v. Iterable it (k /\ v) => Identable k => it -> HashMap k v
hm'collect = it'collect @HashMap

hm2d'collect
  :: forall it k1 k2 v
   . Iterable it (k1 /\ k2 /\ v)
  => Identable k1
  => Identable k2
  => it
  -> HashMap k1 (HashMap k2 v)
hm2d'collect = it'collect @HashMap'2D

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
hm'merge hms = hm'collect $ arr'concat $ map hm'entries hms

hm'insert
  :: forall @k @v. Eq v => Identable k => k -> v -> HashMap k v -> HashMap k v
hm'insert k v hm = if willNotChange then hm else rebuilt unit
  where
  willNotChange = Just v == hm'lookup k hm
  rebuilt _ = hm'collect $ arr'concat [ hm'entries hm, [ k /\ v ] ]

hm'remove
  :: forall @k @v. Identable k => k -> HashMap k v -> HashMap k v
hm'remove k hm = if willNotChange then hm else rebuilt unit
  where
  willNotChange = isNothing $ hm'lookup k hm
  kString = ident'key k
  pred (kIn /\ _) = ident'key kIn == kString
  rebuilt _ = hm'collect $ arr'remove pred $ hm'entries hm

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