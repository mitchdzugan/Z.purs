module Z.Z.Shorthand
  ( __
  , _o
  , _o_
  , g_
  , gmOr'_
  , gmOr_
  , gm_
  , o_
  , or
  ) where

import Data.Lens as Lens
import Data.Maybe as Maybe
import Z.Z.Barlow as ZBl
import Z.Z.Defaultable as ZDefaultable
import Prelude

or :: forall a. a -> Maybe.Maybe a -> a
or = Maybe.fromMaybe

g_
  :: forall @sym lenses s t a b
   . ZBl.ParseSymbol sym lenses
  => ZBl.ConstructBarlow lenses (ZBl.Forget a) s t a b
  => ZBl.IsSymbol sym
  => s
  -> a
g_ = Lens.view (ZBl.__ @sym)

gm_
  :: forall @sym lenses s t a b
   . ZBl.ParseSymbol sym lenses
  => ZBl.ConstructBarlow lenses (ZBl.Forget (ZBl.First a)) s t a b
  => ZBl.IsSymbol sym
  => s
  -> Maybe.Maybe a
gm_ = Lens.preview (ZBl.__ @sym)

gmOr_
  :: forall @sym lenses s t a b
   . ZBl.ParseSymbol sym lenses
  => ZBl.ConstructBarlow lenses (ZBl.Forget (ZBl.First a)) s t a b
  => ZBl.IsSymbol sym
  => a
  -> s
  -> a
gmOr_ a d = Maybe.fromMaybe a $ Lens.preview (ZBl.__ @sym) d

gmOr'_
  :: forall @sym lenses s t a b
   . ZBl.ParseSymbol sym lenses
  => ZBl.ConstructBarlow lenses (ZBl.Forget (ZBl.First a)) s t a b
  => ZBl.IsSymbol sym
  => ZDefaultable.Defaultable a
  => s
  -> a
gmOr'_ d = ZDefaultable.orDefault $ Lens.preview (ZBl.__ @sym) d

__
  :: forall @string lenses p s t a b
   . ZBl.ParseSymbol string lenses
  => ZBl.ConstructBarlow lenses p s t a b
  => ZBl.IsSymbol string
  => Lens.Optic p s t a b
__ = ZBl.barlow @string

_o
  :: forall @string lenses p s t a b y z
   . ZBl.ParseSymbol string lenses
  => ZBl.ConstructBarlow lenses p s t a b
  => ZBl.IsSymbol string
  => ZBl.Optic p a b y z
  -> ZBl.Optic p s t y z
_o i = ZBl.barlow @string <<< i

o_
  :: forall @string lenses p s t a b y z
   . ZBl.ParseSymbol string lenses
  => ZBl.ConstructBarlow lenses p s t a b
  => ZBl.IsSymbol string
  => Lens.Optic p y z s t
  -> Lens.Optic p y z a b
o_ i = ZBl.barlow @string >>> i

_o_
  :: forall @string1 @string2 lenses1 lenses2 p s1 t1 a1 b1 s2 t2 a2 b2
   . ZBl.ParseSymbol string1 lenses1
  => ZBl.ParseSymbol string2 lenses2
  => ZBl.ConstructBarlow lenses1 p s1 t1 a1 b1
  => ZBl.ConstructBarlow lenses2 p s2 t2 a2 b2
  => ZBl.IsSymbol string1
  => ZBl.IsSymbol string2
  => Lens.Optic p a1 b1 s2 t2
  -> Lens.Optic p s1 t1 a2 b2
_o_ i = ZBl.barlow @string1 <<< i <<< ZBl.barlow @string2