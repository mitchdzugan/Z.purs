module Z.Z.Pair
  ( (~)
  , Pair(..)
  , pair'curry
  , pair'get
  , pair'getOppo
  , pair'swap
  , pair'uncurry
  ) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Z.Z.Ext (encodeJson)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable, id'of, ident'get)
import Z.Z.It (class Iterable, iterable'mk)
import Z.Z.Json.Decode (class XDecodeJson, xDecodeJson)
import Z.Z.PairKey as PK

data Pair p = Pair p p

infixr 0 Pair as ~

pos ∷ ∀ a. Pair a → a
pos (x ~ _) = x

-- | Returns the second component of a pair.
neg ∷ ∀ a. Pair a → a
neg (_ ~ y) = y

-- | Turn a function that expects a pair into a function of two arguments.
pair'curry ∷ ∀ a b. (Pair a → b) → a → a → b
pair'curry f x y = f (x ~ y)

-- | Turn a function of two arguments into a function that expects a pair.
pair'uncurry ∷ ∀ a b. (a → a → b) → Pair a → b
pair'uncurry f (x ~ y) = f x y

-- | Exchange the two components of the pair
pair'swap ∷ ∀ a. Pair a -> Pair a
pair'swap (x ~ y) = y ~ x

pair'get ∷ ∀ a. PK.PairKey -> Pair a -> a
pair'get pk (x ~ y) = if pk == PK.pos then x else y

pair'getOppo ∷ ∀ a. PK.PairKey -> Pair a -> a
pair'getOppo pk p = pair'get (PK.pairKey'inv pk) p

derive instance Eq a => Eq (Pair a)
derive instance Ord a => Ord (Pair a)

instance Z.EncodeJson p => Z.EncodeJson (Pair p) where
  encodeJson (x ~ y) = encodeJson (x /\ y)

instance XDecodeJson (p /\ p) => XDecodeJson (Pair p) where
  xDecodeJson json = xDecodeJson @(p /\ p) json <#> \(l /\ r) -> l ~ r

instance Show a ⇒ Show (Pair a) where
  show (x ~ y) = "(" <> show x <> " ~ " <> show y <> ")"

instance Functor Pair where
  map f (x ~ y) = f x ~ f y

instance Apply Pair where
  apply (f ~ g) (x ~ y) = f x ~ g y

instance Applicative Pair where
  pure x = x ~ x

instance Bind Pair where
  bind (x ~ y) f = pos (f x) ~ neg (f y)

instance Monad Pair

instance Semigroup a ⇒ Semigroup (Pair a) where
  append (x1 ~ y1) (x2 ~ y2) = (x1 <> x2) ~ (y1 <> y2)

instance Monoid a ⇒ Monoid (Pair a) where
  mempty = mempty ~ mempty

instance Iterable (Pair a) a where
  iterable'mk (Pair x y) = iterable'mk [ x, y ]

instance (Identable t) => Identable (Pair t) where
  ident'get (l ~ r) = id'of $ ident'get l /\ ident'get r