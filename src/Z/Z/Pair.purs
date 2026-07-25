module Z.Z.Pair
  ( (~)
  , Pair(..)
  ) where

import Prelude
import Z.Z.Ext as Z
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Data.Distributive (class Distributive)

data Pair p = Pair p p

infixr 0 Pair as ~

pos ∷ ∀ a. Pair a → a
pos (x ~ _) = x

-- | Returns the second component of a pair.
neg ∷ ∀ a. Pair a → a
neg (_ ~ y) = y

{-
-- | Turn a function that expects a pair into a function of two arguments.
pcurry ∷ ∀ a b. (Pair a → b) → a → a → b
pcurry f x y = f (x ~ y)

-- | Turn a function of two arguments into a function that expects a pair.
puncurry ∷ ∀ a b. (a → a → b) → Pair a → b
puncurry f (x ~ y) = f x y

-- | Exchange the two components of the pair
swap ∷ ∀ a. Pair a → Pair a
swap (x ~ y) = y ~ x
-}

derive instance genericPair :: Z.Generic (Pair p) _

derive instance eqPair ∷ Eq a => Eq (Pair a)

derive instance ordPair ∷ Ord a => Ord (Pair a)

instance decodeT :: Z.DecodeJson p => Z.DecodeJson (Pair p) where
  decodeJson x = Z.genericDecodeJson x

instance encodeT :: Z.EncodeJson p => Z.EncodeJson (Pair p) where
  encodeJson x = Z.genericEncodeJson x

instance showPair ∷ Show a ⇒ Show (Pair a) where
  show (x ~ y) = "(" <> show x <> " ~ " <> show y <> ")"

instance functorPair ∷ Functor Pair where
  map f (x ~ y) = f x ~ f y

instance applyPair ∷ Apply Pair where
  apply (f ~ g) (x ~ y) = f x ~ g y

instance applicativePair ∷ Applicative Pair where
  pure x = x ~ x

instance bindPair ∷ Bind Pair where
  bind (x ~ y) f = pos (f x) ~ neg (f y)

instance monadPair ∷ Monad Pair

instance semigroupPair ∷ Semigroup a ⇒ Semigroup (Pair a) where
  append (x1 ~ y1) (x2 ~ y2) = (x1 <> x2) ~ (y1 <> y2)

instance monoidPair ∷ Monoid a ⇒ Monoid (Pair a) where
  mempty = mempty ~ mempty

instance foldablePair ∷ Foldable Pair where
  foldr f z (Pair x y) = x `f` (y `f` z)
  foldl f z (Pair x y) = (z `f` x) `f` y
  foldMap f (Pair x y) = f x <> f y

instance traversablePair ∷ Traversable Pair where
  traverse f (Pair x y) = Pair <$> f x <*> f y
  sequence (Pair mx my) = Pair <$> mx <*> my

instance distributivePair ∷ Distributive Pair where
  distribute xs = map pos xs ~ map neg xs
  collect f xs = map (pos <<< f) xs ~ map (neg <<< f) xs
