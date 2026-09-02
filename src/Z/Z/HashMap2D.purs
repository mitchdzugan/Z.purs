module Z.Z.HashMap2D
  ( HM'Eff'2d'T
  , ST'_'HashMap'2d
  , XHM2d'R
  , hm'groupBy
  , hm'keyBy
  , st'HashMap'2d
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
  ) where

import Prelude

import Data.Argonaut.Decode as Dec
import Data.Foldable (class Foldable, for_)
import Data.Maybe (Maybe, isJust)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row (class Cons)
import Z.Z.Bin (Bin'Eff'2d'R, Bin'Eff'2d'T)
import Z.Z.Bin as Bin
import Z.Z.Core (arr'fromFoldable, arr'withInd, forM)
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (class IsSymbol, class Newtype, Run, fst, snd, unwrap, wrap)
import Z.Z.Ext as Z
import Z.Z.HashMap (HashMap(..), hm'fromFoldable)
import Z.Z.HashSet
  ( HashSet
  , XHS2d_h'
  , hs'fromFoldable
  , hs'vals
  , hs2d'fromFoldable
  , xhs2d'add
  , xhs2d'eval
  , xhs2d'freezeAt
  , xhs2d'keys
  )
import Z.Z.Id (class Identable, Idented, ident'key, idented'mk, idented'v)
import Z.Z.X
  ( class EffAdapter
  , XST'Init(..)
  , adapter'run
  , eff'tag
  , eff'untag
  , effAdapter'mk
  , eval_
  )

type XHM2d_h' p k1 k2 v x' x rest =
  IsSymbol p
  => Identable k1
  => Identable k2
  => Cons p
       ( Z.Reader
           ( Bin.Bin'Eff'2d'R { k1 :: k1, k2 :: k2, v :: v } p Z./\ Z.Proxy
               (HM'Eff'2d'T k1 k2 v)
           )
       )
       x'
       x
  => rest

type XHM2d_hk p k1 k2 v x rest =
  forall x'
   . Identable k1
  => Identable k2
  => IsSymbol p
  => Cons p
       ( Z.Reader
           ( Bin.Bin'Eff'2d'R { k1 :: k1, k2 :: k2, v :: v } p Z./\ Z.Proxy
               (HM'Eff'2d'T k1 k2 v)
           )
       )
       x'
       x
  => rest

type XHM2d_h_ p k1 k2 v x rest =
  forall x'
   . IsSymbol p
  => Cons p
       ( Z.Reader
           ( Bin.Bin'Eff'2d'R { k1 :: k1, k2 :: k2, v :: v } p Z./\ Z.Proxy
               (HM'Eff'2d'T k1 k2 v)
           )
       )
       x'
       x
  => rest

data HM'Eff'2d'T :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type
data HM'Eff'2d'T k1 k2 v

instance
  ( Identable k1
  , Identable k2
  ) =>
  EffAdapter (HM'Eff'2d'T k1 k2 a)
    p
    Unit
    (Bin'Eff'2d'R { k1 :: k1, k2 :: k2, v :: v } p)
    (HashMap k1 (HashMap k2 v)) where
  effAdapter'mk = effAdapter'mk @(Bin'Eff'2d'T { k1 :: k1, k2 :: k2, v :: v })
    @p
  effAdapter'res r = eff'tag @p do
    k1s <- eff'untag @p $ r.all <#> map _.k1
    entries <- forM k1s \k1 -> do
      f' <- eff'untag @p $ r.freezeAt (ident'key k1) <#> Bin.Bin
      let hs = HashMap $ f' <#> \{ k2, v } -> { k: k2, v }
      pure $ k1 Z./\ hs
    pure $ hm'fromFoldable entries

xhm2d'eval
  :: forall @p @k1 @k2 @v x' x a. XHM2d_h' p k1 k2 v x' x (Run x a -> Run x' a)
xhm2d'eval m = xhm2d'run @p m <#> Z.snd

xhm2d'run
  :: forall @p @k1 @k2 @v x' x a
   . XHM2d_h' p k1 k2 v x' x
       (Run x a -> Run x' (HashMap k1 (HashMap k2 v) Z./\ a))
xhm2d'run = adapter'run @(HM'Eff'2d'T k1 k2 v) @p unit

xhm2d'exec
  :: forall @p @k1 @k2 @v x' x
   . XHM2d_h' p k1 k2 v x' x
       (Run x Unit -> Run x' (HashMap k1 (HashMap k2 v)))
xhm2d'exec m = xhm2d'run @p @k1 @k2 @v m <#> Z.fst

xhm2d'lookup
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> k2 -> Run x (Z.Maybe v))
xhm2d'lookup k1 k2 = Bin.xbin2d'lookup @(HM'Eff'2d'T k1 k2 v) @p k1 k2 <#> (<$>)
  _.v

xhm2d'insert
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> k2 -> v -> Run x Unit)
xhm2d'insert k1 k2 v = Bin.xbin2d'insert @(HM'Eff'2d'T k1 k2 v) @p k1 k2
  { k1, k2, v }

xhm2d'delete
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> k2 -> Run x Unit)
xhm2d'delete k1 k2 = Bin.xbin2d'delete @(HM'Eff'2d'T k1 k2 v) @p k1 k2

xhm2d'clear :: forall @p k1 k2 v x. XHM2d_h_ p k1 k2 v x (Run x Unit)
xhm2d'clear = Bin.xbin2d'clear @(HM'Eff'2d'T k1 k2 v) @p

xhm2d'clearAt :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x Unit)
xhm2d'clearAt k1 = Bin.xbin2d'clearAt @(HM'Eff'2d'T k1 k2 v) @p k1

xhm2d'size :: forall @p k1 k2 v x. XHM2d_h_ p k1 k2 v x (Run x Int)
xhm2d'size = Bin.xbin2d'size @(HM'Eff'2d'T k1 k2 v) @p

xhm2d'sizeAt :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x Int)
xhm2d'sizeAt k1 = Bin.xbin2d'sizeAt @(HM'Eff'2d'T k1 k2 v) @p k1

xhm2d'keysAt
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x (Array k2))
xhm2d'keysAt k1 = Bin.xbin2d'valsAt @(HM'Eff'2d'T k1 k2 v) @p k1 <#> (<$>) _.k2

xhm2d'valsAt
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x (Array v))
xhm2d'valsAt k1 = Bin.xbin2d'valsAt @(HM'Eff'2d'T k1 k2 v) @p k1 <#> (<$>) _.v

xhm2d'entriesAt
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x (Array (k2 /\ v)))
xhm2d'entriesAt k1 = Bin.xbin2d'valsAt @(HM'Eff'2d'T k1 k2 v) @p k1 <#> (<$>)
  \r -> r.k2 /\ r.v

xhm2d'mergeAt
  :: forall @p k1 k2 v x
   . XHM2d_hk p k1 k2 v x (k1 -> HashMap k2 v -> Run x Unit)
xhm2d'mergeAt k1 (HashMap hm) = Bin.xbin2d'mergeAt @(HM'Eff'2d'T k1 k2 v) @p k1
  $ hm <#> \{ k, v } ->
      { k1, k2: k, v }

xhm2d'freezeAt
  :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (k1 -> Run x (HashMap k2 v))
xhm2d'freezeAt k1 =
  Bin.xbin2d'freezeAt @(HM'Eff'2d'T k1 k2 v) @p k1
    <#> (map \{ k2, v } -> { k: k2, v })
    <#> Z.wrap

xhm2d'keys :: forall @p k1 k2 v x. XHM2d_hk p k1 k2 v x (Run x (Array k1))
xhm2d'keys = hs'vals <$> hs'fromFoldable <$> map _.k1 <$> Bin.xbin2d'all
  @(HM'Eff'2d'T k1 k2 v)
  @p

xhm2d'entries
  :: forall @p k1 k2 v x
   . XHM2d_hk p k1 k2 v x (Run x (Array (k1 Z./\ HashMap k2 v)))
xhm2d'entries = do
  keys <- xhm2d'keys @p
  forM keys \k -> (xhm2d'freezeAt @p k <#> \hs -> k /\ hs)

type XHM2d'R :: forall k. k -> Type -> Type -> Type -> Type -> Type
type XHM2d'R p k1 k2 v = Z.Reader
  ( Bin.Bin'Eff'2d'R { k1 :: k1, k2 :: k2, v :: v } p Z./\ Z.Proxy
      (HM'Eff'2d'T k1 k2 v)
  )

hm'keyBy
  :: forall f t k
   . Identable k
  => Foldable f
  => (t -> k)
  -> f t
  -> HashMap k t
hm'keyBy f c = hm'fromFoldable $ arr'fromFoldable c <#> \t -> f t /\ t

hm'groupBy
  :: forall f t k
   . Identable k
  => Foldable f
  => (t -> k)
  -> f t
  -> HashMap k (Array t)
hm'groupBy f c = arr'fromFoldable c
  # arr'withInd
  # map (\(ind /\ v) -> f v /\ idented'mk ind v)
  # hs2d'fromFoldable
  <#> map idented'v <<< hs'vals

st'HashMap'2d :: forall @k1 @k2 @v. ST'_'HashMap'2d k1 k2 v
st'HashMap'2d = XST'Init $ Z.Proxy @(HM'Eff'2d'T k1 k2 v) /\ unit

type ST'_'HashMap'2d k1 k2 v = XST'Init (HM'Eff'2d'T k1 k2 v) Unit