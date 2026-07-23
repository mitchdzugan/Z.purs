module Z.Z.Barlow
  ( l
  , module BarlowParse
  , module BarlowCons
  , module Strong
  , module Symbol
  , module Proxy
  , module Barlow
  , module Lens
  , module MaybeFirst
  ) where

import Data.Lens (Forget, Optic) as Lens
import Data.Lens.Barlow (class Barlow, barlow) as Barlow
import Data.Lens.Barlow.Construction (class ConstructBarlow) as BarlowCons
import Data.Lens.Barlow.Parser (class ParseSymbol) as BarlowParse
import Data.Maybe.First (First) as MaybeFirst
import Data.Symbol (class IsSymbol) as Symbol
import Data.Profunctor.Strong (class Strong) as Strong
import Type.Proxy (Proxy(..)) as Proxy

l
  :: forall @string lenses p s t a b
   . BarlowParse.ParseSymbol string lenses
  => BarlowCons.ConstructBarlow lenses p s t a b
  => Symbol.IsSymbol string
  => Lens.Optic p s t a b
l = Barlow.barlow @string