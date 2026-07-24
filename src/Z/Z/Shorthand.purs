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

import Prelude

import Z.Z.Barlow (class ConstructBarlow, class ParseSymbol, Forget, barlow) as Z
import Z.Z.Defaultable (class Defaultable, orDefault) as Z
import Z.Z.Ext (class IsSymbol, First, Maybe, Optic, fromMaybe, preview, view) as Z

or :: forall a. a -> Z.Maybe a -> a
or = Z.fromMaybe

g_
  :: forall @sym lenses s t a b
   . Z.ParseSymbol sym lenses
  => Z.ConstructBarlow lenses (Z.Forget a) s t a b
  => Z.IsSymbol sym
  => s
  -> a
g_ = Z.view (Z.barlow @sym)

gm_
  :: forall @sym lenses s t a b
   . Z.ParseSymbol sym lenses
  => Z.ConstructBarlow lenses (Z.Forget (Z.First a)) s t a b
  => Z.IsSymbol sym
  => s
  -> Z.Maybe a
gm_ = Z.preview (Z.barlow @sym)

gmOr_
  :: forall @sym lenses s t a b
   . Z.ParseSymbol sym lenses
  => Z.ConstructBarlow lenses (Z.Forget (Z.First a)) s t a b
  => Z.IsSymbol sym
  => a
  -> s
  -> a
gmOr_ a d = Z.fromMaybe a $ Z.preview (Z.barlow @sym) d

gmOr'_
  :: forall @sym lenses s t a b
   . Z.ParseSymbol sym lenses
  => Z.ConstructBarlow lenses (Z.Forget (Z.First a)) s t a b
  => Z.IsSymbol sym
  => Z.Defaultable a
  => s
  -> a
gmOr'_ d = Z.orDefault $ Z.preview (Z.barlow @sym) d

__
  :: forall @string lenses p s t a b
   . Z.ParseSymbol string lenses
  => Z.ConstructBarlow lenses p s t a b
  => Z.IsSymbol string
  => Z.Optic p s t a b
__ = Z.barlow @string

_o
  :: forall @string lenses p s t a b y z
   . Z.ParseSymbol string lenses
  => Z.ConstructBarlow lenses p s t a b
  => Z.IsSymbol string
  => Z.Optic p a b y z
  -> Z.Optic p s t y z
_o i = Z.barlow @string <<< i

o_
  :: forall @string lenses p s t a b y z
   . Z.ParseSymbol string lenses
  => Z.ConstructBarlow lenses p s t a b
  => Z.IsSymbol string
  => Z.Optic p y z s t
  -> Z.Optic p y z a b
o_ i = Z.barlow @string >>> i

_o_
  :: forall @string1 @string2 lenses1 lenses2 p s1 t1 a1 b1 s2 t2 a2 b2
   . Z.ParseSymbol string1 lenses1
  => Z.ParseSymbol string2 lenses2
  => Z.ConstructBarlow lenses1 p s1 t1 a1 b1
  => Z.ConstructBarlow lenses2 p s2 t2 a2 b2
  => Z.IsSymbol string1
  => Z.IsSymbol string2
  => Z.Optic p a1 b1 s2 t2
  -> Z.Optic p s1 t1 a2 b2
_o_ i = Z.barlow @string1 <<< i <<< Z.barlow @string2