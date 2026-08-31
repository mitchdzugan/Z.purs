module Z.Z.Barlow
  ( class ConstructBarlow'Get
  , class ConstructBarlow'Get'
  , module Barlow
  , module BarlowCons
  , module BarlowParse
  , module Lens
  , module MaybeFirst
  , module Proxy
  , module Strong
  , module Symbol
  ) where

import Data.Lens (Forget, Optic) as Lens
import Data.Maybe.First (First) as MaybeFirst
import Data.Profunctor.Strong (class Strong) as Strong
import Data.Symbol (class IsSymbol) as Symbol
import Type.Proxy (Proxy(..)) as Proxy
import Z.Z.Lens.Barlow (class Barlow, barlow) as Barlow
import Z.Z.Lens.Barlow.Construction (class ConstructBarlow) as BarlowCons
import Z.Z.Lens.Barlow.Parser (class ParseSymbol) as BarlowParse

class
  BarlowCons.ConstructBarlow p (Lens.Forget a) s t a b <=
  ConstructBarlow'Get p s t a b

instance
  ( BarlowCons.ConstructBarlow p (Lens.Forget a) s t a b
  ) =>
  ConstructBarlow'Get p s t a b

class ConstructBarlow'Get p s s a a <= ConstructBarlow'Get' p s a

instance (ConstructBarlow'Get p s s a a) => ConstructBarlow'Get' p s a
