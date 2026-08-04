module Z.Z.Shorthand
  ( (%)
  , (<->)
  , (<:>)
  , (<|<)
  , (>|>)
  , (~)
  , (~.)
  , E'
  , R'
  , TPlus
  , W'
  , Xflipped
  , _'
  , __
  , _o
  , _o_
  , g_
  , gmOr'_
  , gmOr_
  , gm_
  , jOr
  , jOr'
  , jOr0
  , jOr1
  , jOr1n
  , jOrE
  , jOrF
  , jOrT
  , mfirst
  , mlast
  , module ZExp
  , o_
  , over_
  , set_
  , stextConcat
  , stextConcatSp
  , type (#>)
  , type (+)
  , type (<#)
  ) where

import Prelude

import Z.Z.Barlow
  ( class ConstructBarlow
  , class ParseSymbol
  , Forget
  , barlow
  ) as Z
import Z.Z.Defaultable (class Defaultable, orDefault, default) as Z
import Z.Z.Core as ZCore
import Z.Z.Ext
  ( class IsSymbol
  , First
  , Maybe(..)
  , Optic
  , Either(..)
  , fromMaybe
  , preview
  , over
  , view
  , set
  , Reader
  , Writer
  , Except
  ) as Z
import Z.Z.Ext ((/\)) as ZExp
import Z.Z.X as X
import Type.Row (type (+)) as TypeRow

stextConcat
  :: forall t1 t2. ZCore.SText t1 => ZCore.SText t2 => t1 -> t2 -> String
stextConcat t1 t2 = ZCore.stext t1 <> ZCore.stext t2

stextConcatSp
  :: forall t1 t2. ZCore.SText t1 => ZCore.SText t2 => t1 -> t2 -> String
stextConcatSp t1 t2 = ZCore.stext t1 <> " " <> ZCore.stext t2

infixr 5 stextConcat as <:>
infixr 5 stextConcatSp as <->

type R' r = Z.Reader r
type W' w = Z.Writer w

type E' :: forall k. Type -> k -> Type
type E' e = Z.Except e

mfirst
  :: forall r1 r2 a
   . ZCore.Resulting r1
  => ZCore.Resulting r2
  => r1 a
  -> r2 a
  -> r1 a
mfirst r1 r2 = case ZCore.resultVal r1 ZExp./\ ZCore.resultVal r2 of
  Z.Nothing ZExp./\ (Z.Just v2) -> pure v2
  _ -> r1

mlast
  :: forall r1 r2 a
   . ZCore.Resulting r1
  => ZCore.Resulting r2
  => r1 a
  -> r2 a
  -> r2 a
mlast r1 r2 = case ZCore.resultVal r1 ZExp./\ ZCore.resultVal r2 of
  (Z.Just v1) ZExp./\ Z.Nothing -> pure v1
  _ -> r2

infixr 0 mlast as <|<

infixr 0 mfirst as >|>

type Xflipped a x = X.X x a

type TPlus :: forall k. (Row k -> Row k) -> Row k -> Row k
type TPlus a b = a TypeRow.+ b

infixr 0 type X.X as #>

infixr 0 type Xflipped as <#

infixr 1 type TPlus as +

_' :: forall a. Z.Defaultable a => a
_' = Z.default

jOr :: forall a. a -> Z.Maybe a -> a
jOr = Z.fromMaybe

jOr' :: forall a. Z.Defaultable a => Z.Maybe a -> a
jOr' = Z.orDefault

jOr0 :: Z.Maybe Int -> Int
jOr0 = Z.fromMaybe 0

jOr1 :: Z.Maybe Int -> Int
jOr1 = Z.fromMaybe 1

jOr1n :: Z.Maybe Int -> Int
jOr1n = Z.fromMaybe (-1)

jOrT :: Z.Maybe Boolean -> Boolean
jOrT = Z.fromMaybe true

jOrF :: Z.Maybe Boolean -> Boolean
jOrF = Z.fromMaybe false

jOrE :: forall e a. e -> Z.Maybe a -> Z.Either e a
jOrE e m = Z.fromMaybe (Z.Left e) $ m <#> Z.Right

set_
  :: forall s t a b @sym lenses
   . Z.IsSymbol sym
  => Z.ParseSymbol sym lenses
  => Z.ConstructBarlow lenses Function s t a b
  => s
  -> b
  -> t
set_ = flip (Z.set (Z.barlow @sym))

infixr 0 set_ as ~
infixr 0 set_ as ~.

over_
  :: forall s t a b @sym lenses
   . Z.IsSymbol sym
  => Z.ParseSymbol sym lenses
  => Z.ConstructBarlow lenses Function s t a b
  => s
  -> (a -> b)
  -> t
over_ = flip (Z.over (Z.barlow @sym))

infixr 0 over_ as %

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