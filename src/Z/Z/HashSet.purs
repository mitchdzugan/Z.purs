module Z.Z.HashSet
  ( HashSet(..)
  , XHS'R
  , XHS2d'R
  , XHS2d_h'
  , hs'add
  , hs'empty
  , hs'fromFoldable
  , hs'has
  , hs'size
  , hs'vals
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
import Data.Foldable (class Foldable)
import Data.Maybe (isJust)
import Data.Tuple.Nested ((/\))
import Prim.Row (class Cons)
import Z.Z.Bin as Bin
import Z.Z.Core (arr'fromFoldable, forM)
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (class IsSymbol, class Newtype, Run)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable)

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
hs'fromFoldable f = Z.wrap $ Bin.bin'fromFoldable $ arr'fromFoldable f <#> \v ->
  v /\ v

hs'size :: forall @a. Identable a => HashSet a -> Int
hs'size = Bin.bin'size <<< Z.unwrap

hs'has :: forall @a. Identable a => a -> HashSet a -> Boolean
hs'has v s = isJust $ Bin.bin'lookup v $ Z.unwrap s

hs'vals :: forall @a. Identable a => HashSet a -> Array a
hs'vals = Bin.bin'vals <<< Z.unwrap

type XHS_h' p a x' x rest =
  IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'R a p)) x' x
  => rest

type XHS_hk p a x rest =
  forall x'
   . Identable a
  => IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'R a p)) x' x
  => rest

type XHS_h_ p a x rest =
  forall x'
   . IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'R a p)) x' x
  => rest

xhs'eval :: forall @p @a x' x res. XHS_h' p a x' x (Run x res -> Run x' res)
xhs'eval = Bin.xbin'run @p @a

xhs'run
  :: forall @p @a x' x res
   . XHS_h' p a x' x (Run x res -> Run x' (HashSet a Z./\ res))
xhs'run m = Bin.xbin'run @p @a do
  res <- m
  hs <- xhs'freeze @p
  pure $ hs Z./\ res

xhs'exec
  :: forall @p @a x' x. XHS_h' p a x' x (Run x Unit -> Run x' (HashSet a))
xhs'exec m = xhs'run @p m <#> Z.fst

xhs'has :: forall @p a x. XHS_hk p a x (a -> Run x Boolean)
xhs'has k = Bin.xbin'lookup @p k <#> isJust

xhs'add :: forall @p a x. XHS_hk p a x (a -> Run x Unit)
xhs'add k = Bin.xbin'insert @p k k

xhs'remove :: forall @p a x. XHS_hk p a x (a -> Run x Unit)
xhs'remove k = Bin.xbin'delete @p k

xhs'clear :: forall @p a x. XHS_h_ p a x (Run x Unit)
xhs'clear = Bin.xbin'clear @p

xhs'size :: forall @p a x. XHS_h_ p a x (Run x Int)
xhs'size = Bin.xbin'size @p

xhs'vals :: forall @p a x. XHS_h_ p a x (Run x (Array a))
xhs'vals = Bin.xbin'vals @p

xhs'merge :: forall @p a x. XHS_h_ p a x (HashSet a -> Run x Unit)
xhs'merge = Bin.xbin'merge @p <<< Z.unwrap

xhs'freeze :: forall @p a x. XHS_h_ p a x (Run x (HashSet a))
xhs'freeze = Bin.xbin'freeze @p <#> Z.wrap

type XHS'R :: forall k. k -> Type -> Type -> Type
type XHS'R p a = Z.Reader (Bin.Bin'Eff'R a p)

type XHS2d_h' p k a x' x rest =
  IsSymbol p
  => Identable k
  => Identable a
  => Cons p (Z.Reader (Bin.Bin'Eff'2d'R { k :: k, a :: a } p)) x' x
  => rest

type XHS2d_hk p k a x rest =
  forall x'
   . Identable k
  => Identable a
  => IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'2d'R { k :: k, a :: a } p)) x' x
  => rest

type XHS2d_h_ p k a x rest =
  forall x'
   . IsSymbol p
  => Cons p (Z.Reader (Bin.Bin'Eff'2d'R { k :: k, a :: a } p)) x' x
  => rest

xhs2d'eval
  :: forall @p @k @a x' x res. XHS2d_h' p k a x' x (Run x res -> Run x' res)
xhs2d'eval = Bin.xbin2d'run @p @{ k :: k, a :: a }

xhs2d'has :: forall @p k a x. XHS2d_hk p k a x (k -> a -> Run x Boolean)
xhs2d'has k1 k2 = Bin.xbin2d'lookup @p k1 k2 <#> isJust

xhs2d'add :: forall @p k a x. XHS2d_hk p k a x (k -> a -> Run x Unit)
xhs2d'add k1 k2 = Bin.xbin2d'insert @p k1 k2 { a: k2, k: k1 }

xhs2d'remove :: forall @p k a x. XHS2d_hk p k a x (k -> a -> Run x Unit)
xhs2d'remove k1 k2 = Bin.xbin2d'delete @p k1 k2

xhs2d'clear :: forall @p k a x. XHS2d_h_ p k a x (Run x Unit)
xhs2d'clear = Bin.xbin2d'clear @p

xhs2d'clearAt :: forall @p k a x. XHS2d_hk p k a x (k -> Run x Unit)
xhs2d'clearAt k = Bin.xbin2d'clearAt @p k

xhs2d'size :: forall @p k a x. XHS2d_h_ p k a x (Run x Int)
xhs2d'size = Bin.xbin2d'size @p

xhs2d'sizeAt :: forall @p k a x. XHS2d_hk p k a x (k -> Run x Int)
xhs2d'sizeAt k = Bin.xbin2d'sizeAt @p k

xhs2d'valsAt :: forall @p k a x. XHS2d_hk p k a x (k -> Run x (Array a))
xhs2d'valsAt k = Bin.xbin2d'valsAt @p k <#> map _.a

xhs2d'keys :: forall @p k a x. XHS2d_hk p k a x (Run x (Array k))
xhs2d'keys = hs'vals <$> hs'fromFoldable <$> map _.k <$> Bin.xbin2d'all @p

xhs2d'entries
  :: forall @p k a x. XHS2d_hk p k a x (Run x (Array (k Z./\ HashSet a)))
xhs2d'entries = do
  keys <- xhs2d'keys @p
  forM keys \k -> (xhs2d'freezeAt @p k <#> \hs -> k /\ hs)

xhs2d'mergeAt
  :: forall @p k a x
   . XHS2d_hk p k a x (k -> HashSet a -> Run x Unit)
xhs2d'mergeAt k1 hm = Bin.xbin2d'mergeAt @p k1 $ Z.unwrap hm <#> \a ->
  { k: k1, a }

xhs2d'freezeAt
  :: forall @p k a x. XHS2d_hk p k a x (k -> Run x (HashSet a))
xhs2d'freezeAt k = Bin.xbin2d'freezeAt @p k <#> Z.wrap <<< map \{ a } -> a

type XHS2d'R :: forall k1. k1 -> Type -> Type -> Type -> Type
type XHS2d'R p k a = Z.Reader (Bin.Bin'Eff'2d'R { k :: k, a :: a } p)
