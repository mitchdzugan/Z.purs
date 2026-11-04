module Z.Z.HashSet
  ( HashSet(..)
  , hs'add
  , hs'collect
  , hs'empty
  , hs'has
  , hs'size
  , hs'vals
  ) where

import Prelude

import Data.Argonaut.Decode as Dec
import Data.Maybe (isJust)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Z.Z.Bin as Bin
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (class Newtype)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable, class Identable'Functor, ident'key)
import Z.Z.It
  ( class Collectable
  , class Iterable
  , collect
  , it'collect
  , iterable'mk
  )

newtype HashSet a = HashSet (Bin.Bin a)

derive instance Newtype (HashSet a) _

instance Identable'Functor HashSet where
  identable'map f hs = hs'collect $ hs'vals hs <#> f

instance Identable a => Generable (HashSet a) gdesc (HashSet a) where
  mkGenerable = hs'empty

instance (Z.EncodeJson a) => Z.EncodeJson (HashSet a) where
  encodeJson = Z.encodeJson <<< Z.unwrap

instance (Z.DecodeJson a) => Z.DecodeJson (HashSet a) where
  decodeJson v = Z.wrap <$> Dec.decodeJson v

instance Iterable (HashSet a) a where
  iterable'mk it = iterable'mk $ Z.unwrap $ Z.unwrap it

instance
  Identable a =>
  Collectable HashSet a (HashSet a) where
  collect it = Z.wrap $ Z.wrap $ collect @Object $ it <#> \a -> ident'key a /\ a

hs'empty :: forall @a. Identable a => HashSet a
hs'empty = Z.wrap $ Bin.bin'empty

hs'add :: forall @a. Identable a => a -> HashSet a -> HashSet a
hs'add v = Z.wrap <<< Bin.bin'insert v v <<< Z.unwrap

hs'collect :: forall it a. Iterable it a => Identable a => it -> HashSet a
hs'collect = it'collect @HashSet

hs'size :: forall @a. Identable a => HashSet a -> Int
hs'size = Bin.bin'size <<< Z.unwrap

hs'has :: forall @a. Identable a => a -> HashSet a -> Boolean
hs'has v s = isJust $ Bin.bin'lookup v $ Z.unwrap s

hs'vals :: forall @a. Identable a => HashSet a -> Array a
hs'vals = Bin.bin'vals <<< Z.unwrap
