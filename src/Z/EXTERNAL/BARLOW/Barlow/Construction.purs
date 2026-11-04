module Z.EXTERNAL.BARLOW.Barlow.Construction where

import Data.Either (Either)
import Data.Lens (Optic, _Just, _Left, _Right)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
-- import Data.Traversable (class Traversable)
import Prelude ((<<<))
import Prim.Row as Row
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Z.EXTERNAL.BARLOW.Barlow.Types
  ( ExclamationMark
  , LeftArrow
  , QuestionMark
  , RecordField
  , RightArrow
  , TCons
  , TList
  , TNil
  )

-- Typeclass for constructing a barlow lenses from a typelevel list and an input type.
class ConstructBarlow (lenses :: TList) p s t a b | lenses s -> t a b where
  constructBarlow :: Proxy lenses -> Optic p s t a b

-- Nil instance for question mark 
instance
  ( Choice p
  ) =>
  ConstructBarlow (TCons QuestionMark TNil) p (Maybe a) (Maybe b) a b where
  constructBarlow _ = _Just
-- Nil instance for right arrow
else instance
  ( Choice p
  ) =>
  ConstructBarlow (TCons RightArrow TNil) p (Either l a) (Either l b) a b where
  constructBarlow _ = _Right
-- Nil instance for left arrow
else instance
  ( Choice p
  ) =>
  ConstructBarlow (TCons LeftArrow TNil) p (Either a r) (Either b r) a b where
  constructBarlow _ = _Left
-- Nil instance for plus
{-
else instance
  ( Wander p
  , Traversable t
  ) =>
  ConstructBarlow (TCons Plus TNil) p (t a) (t b) a b where
  constructBarlow _ = traversed
-}
-- Nil instance for exclamation mark  
else instance constructBarlowNilExclamationMark ::
  ( Profunctor p
  , Newtype nt a
  ) =>
  ConstructBarlow (TCons ExclamationMark TNil) p nt nt a a where
  constructBarlow _ = _Newtype
-- Nil instance for record selector
else instance
  ( IsSymbol sym
  , Row.Cons sym a rc x
  , Row.Cons sym b rc y
  , Strong p
  ) =>
  ConstructBarlow (TCons (RecordField sym) TNil) p (Record x) (Record y) a b where
  constructBarlow _ = prop (Proxy :: Proxy sym)
-- Cons instance for question mark
else instance
  ( ConstructBarlow rest p restA restB a b
  , Choice p
  ) =>
  ConstructBarlow
    (TCons QuestionMark rest)
    p
    (Maybe restA)
    (Maybe restB)
    a
    b where
  constructBarlow _ = _Just <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for right arrow
else instance
  ( ConstructBarlow rest p restA restB a b
  , Choice p
  ) =>
  ConstructBarlow
    (TCons RightArrow rest)
    p
    (Either l restA)
    (Either l restB)
    a
    b where
  constructBarlow _ = _Right <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for left arrow
else instance
  ( ConstructBarlow rest p restA restB a b
  , Choice p
  ) =>
  ConstructBarlow
    (TCons LeftArrow rest)
    p
    (Either restA r)
    (Either restB r)
    a
    b where
  constructBarlow _ = _Left <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for plus
{-
else instance
  ( ConstructBarlow rest p restA restB a b
  , Wander p
  , Traversable t
  ) =>
  ConstructBarlow
    (TCons Plus rest)
    p
    (t restA)
    (t restB)
    a
    b where
  constructBarlow _ = traversed <<< constructBarlow (Proxy :: Proxy rest)
-}
-- Cons instance for Newtype
else instance
  ( ConstructBarlow rest p restA restA a a
  , Newtype nt restA
  , Profunctor p
  ) =>
  ConstructBarlow
    (TCons ExclamationMark rest)
    p
    nt
    nt
    a
    a where
  constructBarlow _ = _Newtype <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for record selector
else instance
  ( IsSymbol sym
  , ConstructBarlow rest p restA restB a b
  , Row.Cons sym restA thru rx
  , Row.Cons sym restB thru ry
  , Strong p
  ) =>
  ConstructBarlow (TCons (RecordField sym) rest) p { | rx } { | ry } a b where
  constructBarlow _ = prop (Proxy :: Proxy sym) <<< constructBarlow
    (Proxy :: Proxy rest)
