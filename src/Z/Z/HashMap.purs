module Z.Z.HashMap
  ( HashMap(..)
  , XHM'R
  , XHM2d'R
  , hm'empty
  , hm'entries
  , hm'fromFoldable
  , hm'groupBy
  , hm'has
  , hm'keyBy
  , hm'keys
  , hm'lookup
  , hm'set
  , hm'size
  , hm'vals
  , hs2d'fromFoldable
  , xhm'clear
  , xhm'delete
  , xhm'entries
  , xhm'eval
  , xhm'exec
  , xhm'freeze
  , xhm'insert
  , xhm'keys
  , xhm'lookup
  , xhm'merge
  , xhm'run
  , xhm'size
  , xhm'vals
  , xhm2d'clear
  , xhm2d'clearAt
  , xhm2d'delete
  , xhm2d'entries
  , xhm2d'entriesAt
  , xhm2d'eval
  , xhm2d'exec
  , xhm2d'freezeAt
  , xhm2d'insert
  , xhm2d'keys
  , xhm2d'keysAt
  , xhm2d'lookup
  , xhm2d'mergeAt
  , xhm2d'run
  , xhm2d'size
  , xhm2d'sizeAt
  , xhm2d'valsAt
  , xhs2d'exec
  , xhs2d'run
  ) where

import Prelude

import Data.Argonaut.Decode as Dec
import Data.Foldable (class Foldable, for_)
import Data.Maybe (Maybe, isJust)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row (class Cons)
import Z.Z.Bin as Bin
import Z.Z.Core (arr'fromFoldable, arr'withInd, forM)
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (class IsSymbol, class Newtype, Run, fst, snd, unwrap, wrap)
import Z.Z.Ext as Z
import Z.Z.HashSet
  ( HashSet
  , XHS2d_h'
  , hs'fromFoldable
  , hs'vals
  , xhs2d'add
  , xhs2d'eval
  , xhs2d'freezeAt
  , xhs2d'keys
  )
import Z.Z.Key (class HasKey, Keyed(..), key, keyed'v)
import Z.Z.X (eval_)

newtype HashMap k v = HashMap (Bin.Bin { k :: k, v :: v })

derive instance Newtype (HashMap k v) _

instance Functor (HashMap k) where
  map f (HashMap hm) = HashMap $ hm <#> \d -> { k: d.k, v: f d.v }

instance HasKey k => Generable (HashMap k v) gdesc (HashMap k v) where
  mkGenerable = hm'empty

instance (Z.EncodeJson { k :: k, v :: v }) => Z.EncodeJson (HashMap k v) where
  encodeJson = Z.encodeJson <<< Z.unwrap

instance (Z.DecodeJson { k :: k, v :: v }) => Z.DecodeJson (HashMap k v) where
  decodeJson v = Z.wrap <$> Dec.decodeJson v

hm'empty :: forall @k @v. HasKey k => HashMap k v
hm'empty = Z.wrap Bin.bin'empty

hm'set :: forall @k @v. HasKey k => k -> v -> HashMap k v -> HashMap k v
hm'set k v = Z.wrap <<< Bin.bin'insert k { k, v } <<< Z.unwrap

hm'fromFoldable
  :: forall @f @k @v. HasKey k => Foldable f => f (k /\ v) -> HashMap k v
hm'fromFoldable f =
  Z.wrap $ Bin.bin'fromFoldable $ arr'fromFoldable f <#> \(k /\ v) -> k /\
    { k, v }

hm'size :: forall @k @v. HasKey k => HashMap k v -> Int
hm'size = Bin.bin'size <<< Z.unwrap

hm'lookup :: forall @k @v. HasKey k => k -> HashMap k v -> Maybe v
hm'lookup v (HashMap hm) = Bin.bin'lookup v hm <#> _.v

hm'has :: forall @k @v. HasKey k => k -> HashMap k v -> Boolean
hm'has v (HashMap hm) = isJust $ Bin.bin'lookup v hm

hm'entries :: forall @k @v. HasKey k => HashMap k v -> Array (k /\ v)
hm'entries (HashMap hm) = Bin.bin'vals hm <#> \{ k, v } -> k /\ v

hm'keys :: forall @k @v. HasKey k => HashMap k v -> Array k
hm'keys (HashMap hm) = Bin.bin'vals hm <#> \{ k } -> k

hm'vals :: forall @k @v. HasKey k => HashMap k v -> Array v
hm'vals (HashMap hm) = Bin.bin'vals hm <#> \{ v } -> v

type XHM_h' p k v x' x rest =
  IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'R { k :: k, v :: v } p)) x' x
  => rest

type XHM_hk p k v x rest =
  forall x'
   . HasKey k
  => IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'R { k :: k, v :: v } p)) x' x
  => rest

type XHM_h_ p k v x rest =
  forall x'
   . IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'R { k :: k, v :: v } p)) x' x
  => rest

xhm'eval :: forall @p @k @v x' x a. XHM_h' p k v x' x (Run x a -> Run x' a)
xhm'eval = Bin.xbin'run @p @{ k :: k, v :: v }

xhm'run
  :: forall @p @k @v x' x a
   . XHM_h' p k v x' x (Run x a -> Run x' (HashMap k v Z./\ a))
xhm'run m = xhm'eval @p do
  res <- m
  hm <- xhm'freeze @p
  pure $ hm Z./\ res

xhm'exec
  :: forall @p @k @v x' x
   . XHM_h' p k v x' x (Run x Unit -> Run x' (HashMap k v))
xhm'exec m = xhm'run @p m <#> Z.fst

xhm'lookup :: forall @p k v x. XHM_hk p k v x (k -> Run x (Z.Maybe v))
xhm'lookup k = Bin.xbin'lookup @p k <#> (<$>) _.v

xhm'insert :: forall @p k v x. XHM_hk p k v x (k -> v -> Run x Unit)
xhm'insert k v = Bin.xbin'insert @p k { k, v }

xhm'delete :: forall @p k v x. XHM_hk p k v x (k -> Run x Unit)
xhm'delete k = Bin.xbin'delete @p k

xhm'clear :: forall @p k v x. XHM_h_ p k v x (Run x Unit)
xhm'clear = Bin.xbin'clear @p

xhm'size :: forall @p k v x. XHM_h_ p k v x (Run x Int)
xhm'size = Bin.xbin'size @p

xhm'vals :: forall @p k v x. XHM_h_ p k v x (Run x (Array v))
xhm'vals = Bin.xbin'vals @p <#> (<$>) _.v

xhm'keys :: forall @p k v x. XHM_h_ p k v x (Run x (Array k))
xhm'keys = Bin.xbin'vals @p <#> (<$>) _.k

xhm'entries :: forall @p k v x. XHM_h_ p k v x (Run x (Array (k /\ v)))
xhm'entries = Bin.xbin'vals @p <#> (<$>) \d -> d.k /\ d.v

xhm'merge :: forall @p k v x. XHM_h_ p k v x (HashMap k v -> Run x Unit)
xhm'merge = Bin.xbin'merge @p <<< Z.unwrap

xhm'freeze :: forall @p k v x. XHM_h_ p k v x (Run x (HashMap k v))
xhm'freeze = Z.wrap <$> Bin.xbin'freeze @p

type XHM'R :: forall k1. k1 -> Type -> Type -> Type -> Type
type XHM'R p k v = Z.Reader (Bin.Bin'Eff'R { k :: k, v :: v } p)

type XHM2d_h' p k1 k2 v x' x rest =
  IsSymbol p
  => HasKey k1
  => HasKey k2
  => Cons p (Z.Reader (Bin.Bin'Eff'2d'R { k1 :: k1, k2 :: k2, v :: v } p)) x' x
  => rest

type XHM2d_hk p k1 k2 v x rest =
  forall x'
   . HasKey k1
  => HasKey k2
  => IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'2d'R { k1 :: k1, k2 :: k2, v :: v } p)) x' x
  => rest

type XHM2d_h_ p k1 k2 v x rest =
  forall x'
   . IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'2d'R { k1 :: k1, k2 :: k2, v :: v } p)) x' x
  => rest

xhm2d'eval
  :: forall @p @k1 @k2 @v x' x a. XHM2d_h' p k1 k2 v x' x (Run x a -> Run x' a)
xhm2d'eval = Bin.xbin2d'run @p @{ k1 :: k1, k2 :: k2, v :: v }

xhm2d'run
  :: forall @p @k1 @k2 @v x' x a
   . XHM2d_h' p k1 k2 v x' x
       (Run x a -> Run x' (HashMap k1 (HashMap k2 v) Z./\ a))
xhm2d'run m = xhm2d'eval @p @k1 @k2 @v do
  res <- m
  k1s <- xhm2d'keys @p
  entries <- forM k1s \k1 -> xhm2d'freezeAt @p k1 <#> \hm -> k1 Z./\ hm
  pure $ hm'fromFoldable entries Z./\ res

xhm2d'exec
  :: forall @p @k1 @k2 @v x' x
   . XHM2d_h' p k1 k2 v x' x
       (Run x Unit -> Run x' (HashMap k1 (HashMap k2 v)))
xhm2d'exec m = xhm2d'run @p @k1 @k2 @v m <#> Z.fst

xhs2d'run
  :: forall @p @k @a x' x res
   . XHS2d_h' p k a x' x
       (Run x res -> Run x' (HashMap k (HashSet a) Z./\ res))
xhs2d'run m = xhs2d'eval @p @k @a do
  res <- m
  k1s <- xhs2d'keys @p
  entries <- forM k1s \k1 -> xhs2d'freezeAt @p k1 <#> \hm -> k1 Z./\ hm
  pure $ hm'fromFoldable entries Z./\ res

xhs2d'exec
  :: forall @p @k @a x' x
   . XHS2d_h' p k a x' x (Run x Unit -> Run x' (HashMap k (HashSet a)))
xhs2d'exec m = xhs2d'run @p @k @a m <#> Z.fst

hs2d'fromFoldable
  :: forall f k a
   . Foldable f
  => HasKey k
  => HasKey a
  => f (k /\ a)
  -> HashMap k (HashSet a)
hs2d'fromFoldable f =
  eval_ $ xhs2d'exec @"" $ for_ f \(k /\ a) -> xhs2d'add @"" k a

xhm2d'lookup
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> k2 -> Run x (Z.Maybe v))
xhm2d'lookup k1 k2 = Bin.xbin2d'lookup @p k1 k2 <#> (<$>) _.v

xhm2d'insert
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> k2 -> v -> Run x Unit)
xhm2d'insert k1 k2 v = Bin.xbin2d'insert @p k1 k2 { k1, k2, v }

xhm2d'delete
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> k2 -> Run x Unit)
xhm2d'delete k1 k2 = Bin.xbin2d'delete @p k1 k2

xhm2d'clear :: forall @p k1 k2 v x. XHM2d_h_ p k1 k2 v x (Run x Unit)
xhm2d'clear = Bin.xbin2d'clear @p

xhm2d'clearAt :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x Unit)
xhm2d'clearAt k1 = Bin.xbin2d'clearAt @p k1

xhm2d'size :: forall @p k1 k2 v x. XHM2d_h_ p k1 k2 v x (Run x Int)
xhm2d'size = Bin.xbin2d'size @p

xhm2d'sizeAt :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x Int)
xhm2d'sizeAt k1 = Bin.xbin2d'sizeAt @p k1

xhm2d'keysAt
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x (Array k2))
xhm2d'keysAt k1 = Bin.xbin2d'valsAt @p k1 <#> (<$>) _.k2

xhm2d'valsAt
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x (Array v))
xhm2d'valsAt k1 = Bin.xbin2d'valsAt @p k1 <#> (<$>) _.v

xhm2d'entriesAt
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x (Array (k2 /\ v)))
xhm2d'entriesAt k1 = Bin.xbin2d'valsAt @p k1 <#> (<$>) \r -> r.k2 /\ r.v

xhm2d'mergeAt
  :: forall @p k1 k2 v x
   . XHM2d_hk p k1 k2 v x (k1 -> HashMap k2 v -> Run x Unit)
xhm2d'mergeAt k1 (HashMap hm) = Bin.xbin2d'mergeAt @p k1 $ hm <#> \{ k, v } ->
  { k1, k2: k, v }

xhm2d'freezeAt
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x (HashMap k2 v))
xhm2d'freezeAt k1 =
  Bin.xbin2d'freezeAt @p k1 <#> (map \{ k2, v } -> { k: k2, v }) <#> Z.wrap

xhm2d'keys :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (Run x (Array k1))
xhm2d'keys = hs'vals <$> hs'fromFoldable <$> map _.k1 <$> Bin.xbin2d'all @p

xhm2d'entries
  :: forall @p k1 k2 v x
   . XHM2d_hk p k1 k2 v x (Run x (Array (k1 Z./\ HashMap k2 v)))
xhm2d'entries = do
  keys <- xhm2d'keys @p
  forM keys \k -> (xhm2d'freezeAt @p k <#> \hs -> k /\ hs)

type XHM2d'R :: forall k. k -> Type -> Type -> Type -> Type -> Type
type XHM2d'R p k1 k2 v = Z.Reader
  (Bin.Bin'Eff'2d'R { k1 :: k1, k2 :: k2, v :: v } p)

hm'keyBy
  :: forall f t k
   . HasKey k
  => Foldable f
  => (t -> k)
  -> f t
  -> HashMap k t
hm'keyBy f c = hm'fromFoldable $ arr'fromFoldable c <#> \t -> f t /\ t

newtype HasKey k v = HasKey (k /\ v)

instance (HasKey k) => HasKey (HasKey k v) where
  key (HasKey e) = key $ fst e

hm'groupBy
  :: forall f t k
   . HasKey k
  => Foldable f
  => (t -> k)
  -> f t
  -> HashMap k (Array t)
hm'groupBy f c = arr'fromFoldable c
  # arr'withInd
  # map (\e -> f (snd e) /\ Keyed e)
  # hs2d'fromFoldable
  <#> map keyed'v <<< hs'vals
