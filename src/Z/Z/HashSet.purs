module Z.Z.HashSet
  ( HS'Eff'2d'T
  , HS'Eff'T
  , HashSet(..)
  , ST'HashSet'2d'R
  , ST'_'HashSet
  , ST'_'HashSet'2d
  , XHS'R
  , XHS2d'R
  , XHS2d_h'
  , hs'add
  , hs'empty
  , hs'fromFoldable
  , hs'has
  , hs'size
  , hs'vals
  , hs2d'fromFoldable
  , st'HashSet
  , st'HashSet'2d
  , xhs'add
  , xhs'clear
  , xhs'eval
  , xhs'exec
  , xhs'freeze
  , xhs'has
  , xhs'merge
  , xhs'remove
  , xhs'run
  , xhs'size
  , xhs'vals
  , xhs2d'add
  , xhs2d'clear
  , xhs2d'clearAt
  , xhs2d'entries
  , xhs2d'eval
  , xhs2d'freezeAt
  , xhs2d'has
  , xhs2d'keys
  , xhs2d'mergeAt
  , xhs2d'remove
  , xhs2d'size
  , xhs2d'sizeAt
  , xhs2d'valsAt
  ) where

import Prelude

import Data.Argonaut.Decode as Dec
import Data.Foldable (class Foldable, for_)
import Data.Maybe (isJust)
import Data.Tuple.Nested ((/\))
import Prim.Row (class Cons)
import Z.Z.Bin (Bin'Eff'2d'R, Bin'Eff'2d'T, Bin'Eff'R, Bin'Eff'T)
import Z.Z.Bin as Bin
import Z.Z.Core (arr'fromFoldable, forM)
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (class IsSymbol, class Newtype, Run)
import Z.Z.Ext as Z
import Z.Z.HashMap (HashMap, hm'fromFoldable)
import Z.Z.Id (class Identable, ident'key)
import Z.Z.X
  ( class EffAdapter
  , class M'st'put
  , XST'Init(..)
  , adapter'run
  , eff'tag
  , eff'untag
  , effAdapter'mk
  , eval_
  )

newtype HashSet a = HashSet (Bin.Bin a)

derive instance Newtype (HashSet a) _

instance Functor HashSet where
  map f (HashSet hs) = HashSet $ hs <#> f

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

type XHS_h' p a x' x rest =
  IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'R a p Z./\ Z.Proxy (HS'Eff'T a))) x' x
  => rest

type XHS_hk p a x rest =
  forall x'
   . Identable a
  => IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'R a p Z./\ Z.Proxy (HS'Eff'T a))) x' x
  => rest

type XHS_h_ p a x rest =
  forall x'
   . IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'R a p Z./\ Z.Proxy (HS'Eff'T a))) x' x
  => rest

data HS'Eff'T :: forall k. k -> Type
data HS'Eff'T a

instance EffAdapter (HS'Eff'T a) p Unit (Bin'Eff'R a p) (HashSet a) where
  effAdapter'mk = effAdapter'mk @(Bin'Eff'T a) @p
  effAdapter'res r = HashSet <<< Bin.Bin <$> r.freeze

instance M'st'put (HS'Eff'T a) p (Bin'Eff'R a p) (HashSet a) where
  m'st'put r v = eff'tag @p do
    eff'untag @p $ r.clear
    eff'untag @p $ r.add $ Z.unwrap $ Z.unwrap v

st'HashSet :: forall @a. ST'_'HashSet a
st'HashSet = XST'Init $ Z.Proxy @(HS'Eff'T a) /\ unit

type ST'_'HashSet a = XST'Init (HS'Eff'T a) Unit

xhs'run
  :: forall @p @a x' x res
   . XHS_h' p a x' x (Run x res -> Run x' (HashSet a Z./\ res))
xhs'run = adapter'run @(HS'Eff'T a) @p unit

xhs'eval
  :: forall @p @a x' x res
   . XHS_h' p a x' x (Run x res -> Run x' res)
xhs'eval m = xhs'run @p m <#> Z.snd

xhs'exec
  :: forall @p @a x' x. XHS_h' p a x' x (Run x Unit -> Run x' (HashSet a))
xhs'exec m = xhs'run @p m <#> Z.fst

xhs'has :: forall @p a x. XHS_hk p a x (a -> Run x Boolean)
xhs'has k = Bin.xbin'lookup @(HS'Eff'T a) @p k <#> isJust

xhs'add :: forall @p a x. XHS_hk p a x (a -> Run x Unit)
xhs'add k = Bin.xbin'insert @(HS'Eff'T a) @p k k

xhs'remove :: forall @p a x. XHS_hk p a x (a -> Run x Unit)
xhs'remove k = Bin.xbin'delete @(HS'Eff'T a) @p k

xhs'clear :: forall @p a x. XHS_h_ p a x (Run x Unit)
xhs'clear = Bin.xbin'clear @(HS'Eff'T a) @p

xhs'size :: forall @p a x. XHS_h_ p a x (Run x Int)
xhs'size = Bin.xbin'size @(HS'Eff'T a) @p

xhs'vals :: forall @p a x. XHS_h_ p a x (Run x (Array a))
xhs'vals = Bin.xbin'vals @(HS'Eff'T a) @p

xhs'merge :: forall @p a x. XHS_h_ p a x (HashSet a -> Run x Unit)
xhs'merge = Bin.xbin'merge @(HS'Eff'T a) @p <<< Z.unwrap

xhs'freeze :: forall @p a x. XHS_h_ p a x (Run x (HashSet a))
xhs'freeze = Bin.xbin'freeze @(HS'Eff'T a) @p <#> Z.wrap

type XHS'R :: forall k. k -> Type -> Type -> Type
type XHS'R p a = Z.Reader (Bin.Bin'Eff'R a p Z./\ Z.Proxy (HS'Eff'T a))

type XHS2d_h' p k a x' x rest =
  IsSymbol p
  => Identable k
  => Identable a
  => Cons p
       ( Z.Reader
           ( Bin.Bin'Eff'2d'R { k :: k, a :: a } p Z./\ Z.Proxy
               (HS'Eff'2d'T k a)
           )
       )
       x'
       x
  => rest

type XHS2d_hk p k a x rest =
  forall x'
   . Identable k
  => Identable a
  => IsSymbol p
  => Cons p
       ( Z.Reader
           ( Bin.Bin'Eff'2d'R { k :: k, a :: a } p Z./\ Z.Proxy
               (HS'Eff'2d'T k a)
           )
       )
       x'
       x
  => rest

type XHS2d_h_ p k a x rest =
  forall x'
   . IsSymbol p
  => Cons p
       ( Z.Reader
           ( Bin.Bin'Eff'2d'R { k :: k, a :: a } p Z./\ Z.Proxy
               (HS'Eff'2d'T k a)
           )
       )
       x'
       x
  => rest

data HS'Eff'2d'T :: forall k1 k2. k1 -> k2 -> Type
data HS'Eff'2d'T k a

instance
  ( Identable k
  , Identable a
  ) =>
  EffAdapter (HS'Eff'2d'T k a)
    p
    Unit
    (Bin'Eff'2d'R { k :: k, a :: a } p)
    (HashMap k (HashSet a)) where
  effAdapter'mk = effAdapter'mk @(Bin'Eff'2d'T { k :: k, a :: a }) @p
  effAdapter'res r = eff'tag @p do
    k1s <- eff'untag @p $ r.all <#> map _.k
    entries <- forM k1s \k1 -> do
      f' <- eff'untag @p $ r.freezeAt (ident'key k1) <#> Bin.Bin
      let hs = HashSet $ f' <#> \{ a } -> a
      pure $ k1 Z./\ hs
    pure $ hm'fromFoldable entries

xhs2d'run
  :: forall @p @k @a x' x res
   . XHS2d_h' p k a x' x
       (Run x res -> Run x' ((HashMap k (HashSet a)) Z./\ res))
xhs2d'run = adapter'run @(HS'Eff'2d'T k a) @p unit

xhs2d'eval
  :: forall @p @k @a x' x res. XHS2d_h' p k a x' x (Run x res -> Run x' res)
xhs2d'eval m = xhs2d'run @p m <#> Z.snd

xhs2d'exec
  :: forall @p @k @a x' x
   . XHS2d_h' p k a x' x (Run x Unit -> Run x' (HashMap k (HashSet a)))
xhs2d'exec m = xhs2d'run @p m <#> Z.fst

xhs2d'has :: forall @p k a x. XHS2d_hk p k a x (k -> a -> Run x Boolean)
xhs2d'has k1 k2 = Bin.xbin2d'lookup @(HS'Eff'2d'T k a) @p k1 k2 <#> isJust

xhs2d'add :: forall @p k a x. XHS2d_hk p k a x (k -> a -> Run x Unit)
xhs2d'add k1 k2 = Bin.xbin2d'insert @(HS'Eff'2d'T k a) @p k1 k2 { a: k2, k: k1 }

xhs2d'remove :: forall @p k a x. XHS2d_hk p k a x (k -> a -> Run x Unit)
xhs2d'remove k1 k2 = Bin.xbin2d'delete @(HS'Eff'2d'T k a) @p k1 k2

xhs2d'clear :: forall @p k a x. XHS2d_h_ p k a x (Run x Unit)
xhs2d'clear = Bin.xbin2d'clear @(HS'Eff'2d'T k a) @p

xhs2d'clearAt :: forall @p k a x. XHS2d_hk p k a x (k -> Run x Unit)
xhs2d'clearAt k = Bin.xbin2d'clearAt @(HS'Eff'2d'T k a) @p k

xhs2d'size :: forall @p k a x. XHS2d_h_ p k a x (Run x Int)
xhs2d'size = Bin.xbin2d'size @(HS'Eff'2d'T k a) @p

xhs2d'sizeAt :: forall @p k a x. XHS2d_hk p k a x (k -> Run x Int)
xhs2d'sizeAt k = Bin.xbin2d'sizeAt @(HS'Eff'2d'T k a) @p k

xhs2d'valsAt :: forall @p k a x. XHS2d_hk p k a x (k -> Run x (Array a))
xhs2d'valsAt k = Bin.xbin2d'valsAt @(HS'Eff'2d'T k a) @p k <#> map _.a

xhs2d'keys :: forall @p k a x. XHS2d_hk p k a x (Run x (Array k))
xhs2d'keys = hs'vals <$> hs'fromFoldable <$> map _.k <$> Bin.xbin2d'all
  @(HS'Eff'2d'T k a)
  @p

xhs2d'entries
  :: forall @p k a x. XHS2d_hk p k a x (Run x (Array (k Z./\ HashSet a)))
xhs2d'entries = do
  keys <- xhs2d'keys @p
  forM keys \k -> (xhs2d'freezeAt @p k <#> \hs -> k /\ hs)

xhs2d'mergeAt
  :: forall @p k a x
   . XHS2d_hk p k a x (k -> HashSet a -> Run x Unit)
xhs2d'mergeAt k1 hm = Bin.xbin2d'mergeAt @(HS'Eff'2d'T k a) @p k1 $ Z.unwrap hm
  <#> \a ->
    { k: k1, a }

xhs2d'freezeAt
  :: forall @p k a x. XHS2d_hk p k a x (k -> Run x (HashSet a))
xhs2d'freezeAt k = Bin.xbin2d'freezeAt @(HS'Eff'2d'T k a) @p k <#> Z.wrap <<<
  map \{ a } -> a

type XHS2d'R :: forall k1. k1 -> Type -> Type -> Type -> Type
type XHS2d'R p k a = Z.Reader
  ( Bin.Bin'Eff'2d'R { k :: k, a :: a } p Z./\ Z.Proxy
      (HS'Eff'2d'T k a)
  )

hs2d'fromFoldable
  :: forall f k a
   . Foldable f
  => Identable k
  => Identable a
  => f (k Z./\ a)
  -> HashMap k (HashSet a)
hs2d'fromFoldable f =
  eval_ $ xhs2d'exec @"" $ for_ f \(k /\ a) -> xhs2d'add @"" k a

st'HashSet'2d :: forall @k @a. ST'_'HashSet'2d k a
st'HashSet'2d = XST'Init $ Z.Proxy @(HS'Eff'2d'T k a) /\ unit

type ST'_'HashSet'2d k a = XST'Init (HS'Eff'2d'T k a) Unit
type ST'HashSet'2d'R k a p = Bin.Bin'Eff'2d'R { k :: k, a :: a } p