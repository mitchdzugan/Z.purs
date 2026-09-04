module Z.Z.X5.Core where

import Z.Z.X5.UtilPrelude

import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Prim.RowList as RL
import Run.Reader as RunR
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Z.Z.Core
  ( class C'Relate
  , class ConsSymbol
  , T'Related
  , T'RelatedBy
  , T'apply
  , T'const
  , T'flip
  , T'id
  , T2'0
  , T2'1
  )
import Z.Z.Wraps (class Unwraps, class Wraps, wrapped'from, wrapped'get)

class (Functor res'm) <= X'Consable mf param res'm | mf -> param res'm where
  x'consable'impl
    :: forall p x' x a
     . ConsSymbol p (mf p) x' x
    => Proxy p
    -> param
    -> Run x a
    -> Run x' (res'm a)

class X'RespondsTo mf responds | mf -> responds where
  x'respondsTo'impl
    :: forall p x' x a
     . ConsSymbol p (mf p) x' x
    => Proxy p
    -> responds a
    -> Run x a

class X'Resultable mf result | mf -> result where
  x'resultable'impl
    :: forall p x' x
     . ConsSymbol p (mf p) x' x
    => Proxy p
    -> Run x result

instance
  ( X'RespondsTo mf (VariantF (result :: Resp0Args result | vfr))
  ) =>
  X'Resultable mf result where
  x'resultable'impl p = x'respondsTo'impl @mf p
    (inj (Proxy @"result") resp0Args'id)

data X'Tag :: forall k. k -> Type
data X'Tag tag

data X'Rel''tag''mf

class X'Tags :: forall k1 k2. k1 -> k2 -> Constraint
class X'Tags tag mf | tag -> mf

instance (C'Relate tag X'Rel''tag''mf mf) => X'Tags (X'Tag tag) mf
else instance X'Tags mf mf

type X'mf :: forall k. k -> Type
type X'mf tag = forall mf. X'Tags tag mf => mf

class
  ( X'Tags tag mf
  , X'Consable mf param res'm
  , X'RespondsTo mf (VariantF (result :: Resp0Args result | responds'rest))
  , X'Resultable mf result
  , C'Relate tag X'Rel''tag''mf mf
  , Functor res'm
  ) <=
  X'Runs tag mf param responds'rest res'm result
  | tag -> mf param responds'rest res'm result

instance
  ( X'Tags tag mf
  , X'Consable mf param res'm
  , X'RespondsTo mf (VariantF (result :: Resp0Args result | responds'rest))
  , X'Resultable mf result
  , C'Relate tag X'Rel''tag''mf mf
  , Functor res'm
  ) =>
  X'Runs tag mf param responds'rest res'm result

x'runnable'run
  :: forall @p x' x a @tag mf param res'm
   . X'Consable mf param res'm
  => X'Tags tag mf
  => ConsSymbol p (mf p) x' x
  => param
  -> Run x a
  -> Run x' (res'm a)
x'runnable'run = x'consable'impl @mf (Proxy @p)

x'resultable'run
  :: forall @p x' x @tag mf result
   . X'Resultable mf result
  => X'Tags tag mf
  => ConsSymbol p (mf p) x' x
  => Run x result
x'resultable'run = x'resultable'impl @mf (Proxy @p)

----------------------------------------------------------------------

type TR'h
  :: forall k1 k2 k3 k4
   . k2
  -> Symbol
  -> (k3 -> Type -> Type)
  -> (k3 -> k1 -> Type)
  -> (k4 -> Type)
  -> (k4 -> Type -> k1)
  -> Type
type TR'h tag p res'm'sel res'sel i'res wrap'result =
  forall mf param responds'rest res'm result x' x a
   . X'Runs tag mf param responds'rest (res'm'sel res'm) result
  => ConsSymbol p (mf p) x' x
  => param
  -> Run x (i'res a)
  -> Run x' (res'sel res'm (wrap'result a result))

x'run :: forall @tag @p. TR'h tag p (T'const Identity) T2'1 T'id (/\)
x'run param m = unwrap <$> x'runnable'run @p @tag param do
  a <- m
  result <- x'resultable'run @p @tag
  pure $ a /\ result

x'exec :: forall @tag @p. TR'h tag p (T'const Identity) T2'1 (T'const Unit) T2'1
x'exec r m = x'run @tag @p r m <#> snd

x'eval :: forall @tag @p. TR'h tag p (T'const Identity) T2'1 T'id T2'0
x'eval r m = x'run @tag @p r m <#> fst

xm'run :: forall @tag @p. TR'h tag p T'id T'apply T'id (/\)
xm'run param m = x'runnable'run @p @tag param do
  a <- m
  result <- x'resultable'run @p @tag
  pure $ a /\ result

xm'exec :: forall @tag @p. TR'h tag p T'id T'apply (T'const Unit) T2'1
xm'exec param m = xm'run @tag @p param m <#> map snd

xm'eval :: forall @tag @p. TR'h tag p T'id T'apply T'id T2'0
xm'eval param m = xm'run @tag @p param m <#> map fst

-----------------------------------------------------------------------

type T'tag'param :: forall k. k -> Type

type T'tag'param tag =
  forall mf param res'm. X'Tags tag mf => X'Consable mf param res'm => param

data X'Runnable :: forall k. k -> Type
data X'Runnable tag = X'Runnable (Proxy $ X'mf tag) (T'tag'param tag)

----------------------------------------------------------------------
----------------------------------------------------------------------

-- TODO
-- TODO
-- TODO CHANGE `r` to `rf p`
-- TODO
-- TODO ^^ MAKE THIS THE VERY NEXT THING ^^
-- TODO

newtype R'Tagged :: forall k1. Type -> k1 -> Type -> Type
newtype R'Tagged r p a = R'Tagged'F (r /\ Proxy r -> a)

derive instance Functor (R'Tagged r p)

handle'R'Tagged :: forall p @r a. Proxy p -> r -> R'Tagged r p ~> Run a
handle'R'Tagged _ t (R'Tagged'F f) = pure $ f $ t /\ Proxy

r'tagged'run
  :: forall p @r x' x a
   . ConsSymbol p (R'Tagged r p) x' x
  => Proxy p
  -> r
  -> Run x a
  -> Run x' a
r'tagged'run p r = run (on p (handle'R'Tagged p r) send)

r'tagged'askAt
  :: forall @p r x' x
   . ConsSymbol p (R'Tagged r p) x' x
  => Proxy p
  -> Run x r
r'tagged'askAt p = lift p (R'Tagged'F fst)

----------------------------------------------------------------------

data X'Tag'r :: forall k. k -> Type
data X'Tag'r tag

data X'Rel''tag''r

class X'Tags'r :: forall k1 k2. k1 -> k2 -> Constraint
class X'Tags'r tag r | tag -> r

instance (C'Relate tag X'Rel''tag''r r) => X'Tags'r (X'Tag'r tag) r
else instance X'Tags'r r r

type X'r :: forall k. k -> Type
type X'r tag = forall r. X'Tags'r tag r => r

----------------------------------------------------------------------

class X'Consable'R r param | r -> param where
  x'consable'R'impl :: param -> r

instance
  ( X'Consable'R r param
  ) =>
  X'Consable (R'Tagged r) param Identity where
  x'consable'impl p param m =
    Identity <$> r'tagged'run @r p (x'consable'R'impl @r param) m

class X'RespondsTo'R r responds | r -> responds where
  x'respondsTo'R'impl
    :: forall p x' x a
     . ConsSymbol p (R'Tagged r p) x' x
    => Proxy p
    -> responds a
    -> Run x a

instance
  ( X'RespondsTo'R r responds
  ) =>
  X'RespondsTo (R'Tagged r) responds where
  x'respondsTo'impl = x'respondsTo'R'impl @r

----------------------------------------------------------------------
----------------------------------------------------------------------

instance X'Consable'R (Identity t) t where
  x'consable'R'impl = pure

type R'Identity'reponds t = VariantF
  ( get :: Resp0Args t
  , result :: Resp0Args Unit
  )

instance X'RespondsTo'R (Identity t) (R'Identity'reponds t) where
  x'respondsTo'R'impl p = match
    { get: \(Resp0Args f) -> f <$> unwrap <$> r'tagged'askAt p
    , result: \(Resp0Args f) -> pure $ f unit
    }

----------------------------------------------------------------------

newtype X'R'Adapted r = X'R'Adapted r

derive instance Newtype (X'R'Adapted r) _

class X'Consable'R'Adapted r param | r -> param where
  x'consable'R'adapted'impl :: param -> Effect r

instance
  ( X'Consable'R'Adapted r param
  ) =>
  X'Consable'R (X'R'Adapted r) param where
  x'consable'R'impl =
    X'R'Adapted <<< unsafePerformEffect <<< x'consable'R'adapted'impl @r

r'doAsked
  :: forall @p @r a x' x
   . ConsSymbol p (R'Tagged (X'R'Adapted r) p) x' x
  => Proxy p
  -> (r -> Eff'At p a)
  -> Run x a
r'doAsked p getEff =
  r'tagged'askAt p <#> unwrap <#> eff'useTag ($) <<< getEff

----------------------------------------------------------------------------

data Tag'Eff = Tag'Eff

instance X'Consable'R Tag'Eff Unit where
  x'consable'R'impl _ = Tag'Eff

type R'Tag'Eff'reponds = VariantF (result :: Resp0Args Unit)

instance X'RespondsTo'R Tag'Eff R'Tag'Eff'reponds where
  x'respondsTo'R'impl _ = match { result: \(Resp0Args f) -> pure $ f unit }

----------------------------------------------------------------------------

type X'Eff'Permit :: forall k. k -> Type -> Type
type X'Eff'Permit = R'Tagged Tag'Eff

data Eff'At :: forall k. k -> Type -> Type
data Eff'At t a = Eff'At (Effect a)

derive instance Functor (Eff'At t)

instance Wraps (Eff'At t a) (Effect a) where
  wrapped'get (Eff'At e) = e

instance Unwraps (Eff'At t a) Unit (Effect a) where
  wrapped'mk e _ = (Eff'At e)
  wrapped'rest _ = unit

eff'evalTagged :: forall @t a. Eff'At t a -> a
eff'evalTagged = unsafePerformEffect <<< wrapped'get

eff'useTag :: forall @t v a. ((Eff'At t a -> a) -> v) -> v
eff'useTag runTagged = runTagged $ eff'evalTagged @t

eff'tag :: forall @tag a. Effect a -> Eff'At tag a
eff'tag = wrapped'from

eff'untag :: forall @tag a. forall t. TypeEquals t tag => Eff'At t a -> Effect a
eff'untag = wrapped'get

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
newtype Resp0Args res a = Resp0Args (res -> a)

derive instance Functor (Resp0Args res)

resp0Args :: forall res a. (res -> a) -> Resp0Args res a
resp0Args = Resp0Args

resp0Args'id :: forall res. Resp0Args res res
resp0Args'id = resp0Args identity

-----------------------------------------------------------------------
data Resp1Arg a1 res a = Resp1Arg a1 (res -> a)

derive instance Functor (Resp1Arg p1 res)

resp1Arg :: forall p1 res a. p1 -> (res -> a) -> Resp1Arg p1 res a
resp1Arg p1 f = Resp1Arg p1 f

resp1Arg'id :: forall p1 res. p1 -> Resp1Arg p1 res res
resp1Arg'id p1 = resp1Arg p1 identity

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------

x'get
  :: forall @p x x' mf responds'rest t
   . ConsSymbol p (mf p) x' x
  => X'RespondsTo mf (VariantF (get :: Resp0Args t | responds'rest))
  => Run x t
x'get = x'respondsTo'impl @mf (Proxy @p) (inj (Proxy @"get") resp0Args'id)

x'set
  :: forall @p x x' mf responds'rest t
   . ConsSymbol p (mf p) x' x
  => X'RespondsTo mf (VariantF (set :: Resp1Arg t Unit | responds'rest))
  => t
  -> Run x Unit
x'set t = x'respondsTo'impl @mf (Proxy @p) (inj (Proxy @"set") (resp1Arg'id t))
