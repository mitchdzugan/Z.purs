module Z.Z.HashMap
  ( HM'Eff'T
  , HashMap(..)
  , ST'HashMap'R
  , ST'_'HashMap
  , XHM'R
  , hm'empty
  , hm'entries
  , hm'fromFoldable
  , hm'has
  , hm'keys
  , hm'lookup
  , hm'set
  , hm'size
  , hm'vals
  , st'HashMap
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
  ) where

import Prelude

import Data.Argonaut.Decode as Dec
import Data.Foldable (class Foldable, for_)
import Data.Maybe (Maybe, isJust)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row (class Cons)
import Z.Z.Bin (Bin'Eff'R, Bin'Eff'T)
import Z.Z.Bin as Bin
import Z.Z.Core (arr'fromFoldable, arr'withInd, forM)
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (class IsSymbol, class Newtype, Run, fst, snd, unwrap, wrap)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable, Idented, ident'key, idented'mk, idented'v)
import Z.Z.X (class EffAdapter, XST'Init(..), effAdapter'mk, eval_)

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

type XHM_h' p k v x' x rest =
  IsSymbol p
  => Cons p
       ( Z.Reader
           (Bin.Bin'Eff'R { k :: k, v :: v } p Z./\ Z.Proxy (HM'Eff'T k v))
       )
       x'
       x
  => rest

type XHM_hk p k v x rest =
  forall x'
   . Identable k
  => IsSymbol p
  => Cons p
       ( Z.Reader
           (Bin.Bin'Eff'R { k :: k, v :: v } p Z./\ Z.Proxy (HM'Eff'T k v))
       )
       x'
       x
  => rest

type XHM_h_ p k v x rest =
  forall x'
   . IsSymbol p
  => Cons p
       ( Z.Reader
           (Bin.Bin'Eff'R { k :: k, v :: v } p Z./\ Z.Proxy (HM'Eff'T k v))
       )
       x'
       x
  => rest

data HM'Eff'T :: forall k1 k2. k1 -> k2 -> Type
data HM'Eff'T k v

instance
  EffAdapter (HM'Eff'T k v)
    p
    Unit
    (Bin'Eff'R { k :: k, v :: v } p)
    (HashMap k v) where
  effAdapter'mk = effAdapter'mk @(Bin'Eff'T { k :: k, v :: v }) @p
  effAdapter'res r = HashMap <<< Bin.Bin <$> r.freeze

xhm'eval :: forall @p @k @v x' x a. XHM_h' p k v x' x (Run x a -> Run x' a)
xhm'eval m = xhm'run @p m <#> Z.snd

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
xhm'lookup k = Bin.xbin'lookup @(HM'Eff'T k v) @p k <#> (<$>) _.v

xhm'insert :: forall @p k v x. XHM_hk p k v x (k -> v -> Run x Unit)
xhm'insert k v = Bin.xbin'insert @(HM'Eff'T k v) @p k { k, v }

xhm'delete :: forall @p k v x. XHM_hk p k v x (k -> Run x Unit)
xhm'delete k = Bin.xbin'delete @(HM'Eff'T k v) @p k

xhm'clear :: forall @p k v x. XHM_h_ p k v x (Run x Unit)
xhm'clear = Bin.xbin'clear @(HM'Eff'T k v) @p

xhm'size :: forall @p k v x. XHM_h_ p k v x (Run x Int)
xhm'size = Bin.xbin'size @(HM'Eff'T k v) @p

xhm'vals :: forall @p k v x. XHM_h_ p k v x (Run x (Array v))
xhm'vals = Bin.xbin'vals @(HM'Eff'T k v) @p <#> (<$>) _.v

xhm'keys :: forall @p k v x. XHM_h_ p k v x (Run x (Array k))
xhm'keys = Bin.xbin'vals @(HM'Eff'T k v) @p <#> (<$>) _.k

xhm'entries :: forall @p k v x. XHM_h_ p k v x (Run x (Array (k /\ v)))
xhm'entries = Bin.xbin'vals @(HM'Eff'T k v) @p <#> (<$>) \d -> d.k /\ d.v

xhm'merge :: forall @p k v x. XHM_h_ p k v x (HashMap k v -> Run x Unit)
xhm'merge = Bin.xbin'merge @(HM'Eff'T k v) @p <<< Z.unwrap

xhm'freeze :: forall @p k v x. XHM_h_ p k v x (Run x (HashMap k v))
xhm'freeze = Z.wrap <$> Bin.xbin'freeze @(HM'Eff'T k v) @p

type XHM'R :: forall k1. k1 -> Type -> Type -> Type -> Type
type XHM'R p k v = Z.Reader
  (Bin.Bin'Eff'R { k :: k, v :: v } p Z./\ Z.Proxy (HM'Eff'T k v))

st'HashMap :: forall @k @v. ST'_'HashMap k v
st'HashMap = XST'Init $ Z.Proxy @(HM'Eff'T k v) /\ unit

type ST'_'HashMap k v = XST'Init (HM'Eff'T k v) Unit

type ST'HashMap'R k v p = Bin.Bin'Eff'R { k :: k, v :: v } p