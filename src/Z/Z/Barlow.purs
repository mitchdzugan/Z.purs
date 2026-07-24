module Z.Z.Barlow
  ( __
  , _o
  , _o_
  , module Barlow
  , module BarlowCons
  , module BarlowParse
  , module Lens
  , module MaybeFirst
  , module Proxy
  , module Strong
  , module Symbol
  , o_
  ) where

import Prelude
import Data.Lens (Forget, Optic) as Lens
import Data.Lens.Barlow (class Barlow, barlow) as Barlow
import Data.Lens.Barlow.Construction (class ConstructBarlow) as BarlowCons
import Data.Lens.Barlow.Parser (class ParseSymbol) as BarlowParse
import Data.Maybe.First (First) as MaybeFirst
import Data.Symbol (class IsSymbol) as Symbol
import Data.Profunctor.Strong (class Strong) as Strong
import Type.Proxy (Proxy(..)) as Proxy

__
  :: forall @string lenses p s t a b
   . BarlowParse.ParseSymbol string lenses
  => BarlowCons.ConstructBarlow lenses p s t a b
  => Symbol.IsSymbol string
  => Lens.Optic p s t a b
__ = Barlow.barlow @string

_o
  :: forall @string lenses p s t a b y z
   . BarlowParse.ParseSymbol string lenses
  => BarlowCons.ConstructBarlow lenses p s t a b
  => Symbol.IsSymbol string
  => Lens.Optic p a b y z
  -> Lens.Optic p s t y z
_o i = Barlow.barlow @string <<< i

o_
  :: forall @string lenses p s t a b y z
   . BarlowParse.ParseSymbol string lenses
  => BarlowCons.ConstructBarlow lenses p s t a b
  => Symbol.IsSymbol string
  => Lens.Optic p y z s t
  -> Lens.Optic p y z a b
o_ i = Barlow.barlow @string >>> i

_o_
  :: forall @string1 @string2 lenses1 lenses2 p s1 t1 a1 b1 s2 t2 a2 b2
   . BarlowParse.ParseSymbol string1 lenses1
  => BarlowParse.ParseSymbol string2 lenses2
  => BarlowCons.ConstructBarlow lenses1 p s1 t1 a1 b1
  => BarlowCons.ConstructBarlow lenses2 p s2 t2 a2 b2
  => Symbol.IsSymbol string1
  => Symbol.IsSymbol string2
  => Lens.Optic p a1 b1 s2 t2
  -> Lens.Optic p s1 t1 a2 b2
_o_ i = Barlow.barlow @string1 <<< i <<< Barlow.barlow @string2