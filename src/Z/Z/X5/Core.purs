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
import Z.Z.Core (class ConsSymbol)
import Z.Z.Wraps (class Unwraps, class Wraps, wrapped'from, wrapped'get)

class X'Runs'mf tag mf | tag -> mf

class X'Runs'mf tag mf <= X'Runs'mf'via tag mf | mf -> tag

instance X'Runs'mf tag mf => X'Runs'mf'via tag mf

class X'Runs'param tag param | tag -> param
class X'Runs'res'm tag res'm | tag -> res'm

class X'Runs'responds tag responds | tag -> responds where
  x'runs'responds'respond
    :: forall p mf x' x a
     . X'Runs'mf tag mf
    => ConsSymbol p (mf p) x' x
    => Proxy p
    -> responds a
    -> Run x a

class X'Runs'result tag result | tag -> result where
  x'runs'result'result
    :: forall p mf x' x
     . X'Runs'mf tag mf
    => ConsSymbol p (mf p) x' x
    => Proxy p
    -> Run x result

class
  ( X'Runs'mf tag mf
  , X'Runs'param tag param
  , X'Runs'res'm tag res'm
  , X'Runs'result tag result
  ) <=
  X'Runs tag mf param res'm result
  | tag -> mf param res'm result where
  x'runs'run
    :: forall p mf x' x a
     . X'Runs'mf tag mf
    => ConsSymbol p (mf p) x' x
    => Proxy p
    -> param
    -> Run x a
    -> Run x' (res'm a)

class
  ( X'Runs'mf'via tag mf
  , X'Runs tag mf param res'm result
  ) <=
  X'Runs'via tag mf param res'm result
  | mf -> tag param result

instance
  ( X'Runs'mf'via tag mf
  , X'Runs tag mf param res'm result
  ) =>
  X'Runs'via tag mf param res'm result

class
  ( X'Runs'mf'via tag mf
  , X'Runs'mf tag mf
  , ConsSymbol p (mf p) x' x
  ) <=
  X'Cons p tag mf x' x
  | p mf x' -> tag x

instance
  ( X'Runs'mf'via tag mf
  , X'Runs'mf tag mf
  , ConsSymbol p (mf p) x' x
  ) =>
  X'Cons p tag mf x' x

----------------------------------------------------------------------

type T2'0 t0 t1 = t0

type T2'1 t0 t1 = t1

type T'id t = t
type T'const t'use t'ignore = T2'0 t'use t'ignore

type T'apply tf t1 = tf t1

type TR'h tag p res'm'sel res'sel i'res wrap'result =
  forall mf param res'm result x' x a
   . X'Runs tag mf param (res'm'sel res'm) result
  => X'Cons p tag mf x' x
  => param
  -> Run x (i'res a)
  -> Run x' (res'sel res'm (wrap'result a result))

x'run :: forall @tag @p. TR'h tag p (T'const Identity) T2'1 T'id (/\)
x'run init m = unwrap <$> x'runs'run @tag (Proxy @p) init do
  a <- m
  result <- x'runs'result'result @tag (Proxy @p)
  pure $ a /\ result

x'exec :: forall @tag @p. TR'h tag p (T'const Identity) T2'1 (T'const Unit) T2'1
x'exec r m = x'run @tag @p r m <#> snd

x'eval :: forall @tag @p. TR'h tag p (T'const Identity) T2'1 T'id T2'0
x'eval r m = x'run @tag @p r m <#> fst

xm'run :: forall @tag @p. TR'h tag p T'id T'apply T'id (/\)
xm'run init m = x'runs'run @tag (Proxy @p) init do
  a <- m
  result <- x'runs'result'result @tag (Proxy @p)

  ----------------------------------------------------------------------
  pure $ a /\ result

x'get
  :: forall @p x x' mf tag vfr t
   . X'Cons p tag mf x' x
  => X'Runs'responds tag (VariantF (get :: (->) t | vfr))
  => Run x t
x'get = x'runs'responds'respond @tag (Proxy @p) (inj (Proxy @"get") identity)

----------------------------------------------------------------------

type X'Runnable'param tag = forall param. X'Runs'param tag param => param

type X'Runnable'res'm tag a = forall res'm. X'Runs'res'm tag res'm => res'm a

data X'Runnable tag = X'Runnable (Proxy tag) (X'Runnable'param tag)

newtype R'Tagged tag r p a = R'Tagged'F (r /\ Proxy tag -> a)

derive instance Functor (R'Tagged tag r p)

handle'R'Tagged :: forall p @tag r a. Proxy p -> r -> R'Tagged tag r p ~> Run a
handle'R'Tagged _ t (R'Tagged'F f) = pure $ f $ t /\ Proxy

r'tagged'run
  :: forall p @tag r x' x a
   . ConsSymbol p (R'Tagged tag r p) x' x
  => Proxy p
  -> r
  -> Run x a
  -> Run x' a
r'tagged'run p r = run (on p (handle'R'Tagged p r) send)

r'tagged'askAt
  :: forall @p tag r x' x
   . ConsSymbol p (R'Tagged tag r p) x' x
  => Proxy p
  -> Run x r
r'tagged'askAt p = lift p (R'Tagged'F fst)

class X'Runs'R tag param r responds v | tag -> param r responds v where
  -- x'runs'r'init :: param -> r
  x'runs'r'complete
    :: forall p x' x
     . ConsSymbol p (R'Tagged tag r p) x' x
    => Proxy p
    -> Run x v
  x'runs'r'respond
    :: forall a p x' x
     . ConsSymbol p (R'Tagged tag r p) x' x
    => Proxy p
    -> responds a
    -> Run x a

data X'Reads :: forall k. k -> Type
data X'Reads tag

class X'Runs'R'r tag r | tag -> r
class X'Runs'R'param tag param | tag -> param
class
  X'Runs'R'result tag result
  | tag -> result where
  x'runs'r'result
    :: forall p mf x' x r
     . ConsSymbol p (mf p) x' x
    => X'Runs'mf (X'Reads tag) mf
    => X'Runs'R'r tag r
    => TypeEquals mf (R'Tagged tag r)
    => Proxy p
    -> Run x result

{-

    :: forall p mf x' x
     . X'Runs'mf tag mf
    => ConsSymbol p (mf p) x' x
    => Proxy p
    -> Run x result
-}

instance X'Runs'R'r tag r => X'Runs'mf (X'Reads tag) (R'Tagged tag r)
instance X'Runs'R'param tag param => X'Runs'param (X'Reads tag) param
instance X'Runs'res'm (X'Reads tag) Identity
instance
  ( X'Runs'R'result tag result
  , X'Runs'mf (X'Reads tag) mf
  , X'Runs'R'r tag r
  ) =>
  X'Runs'result (X'Reads tag) result where
  x'runs'result'result = x'runs'r'result @tag

data X'R t

instance X'Runs'R'r (X'R t) t
instance X'Runs'R'param (X'R t) t
instance X'Runs'R'result (X'R t) t where
  x'runs'r'result p = r'tagged'askAt p

{-
  , X'Runs'result tag result
-}

{-
instance
  ( X'Runs'R tag param r responds v
  , X'Runs'mf tag (X'Reads tag)
  ) =>
  X'Runs'run (X'Reads tag) param Identity where
  x'runs'run p init m = 
    Identity <$> r'tagged'run @(X'Reads tag) p (x'runs'r'init @tag init) m
-}

{-

----------------------------------------------------------------------

instance X'Runs'R tag param r responds v => X'Runs'param (X'Reads tag) param
instance
  X'Runs'R tag param r responds v =>
  X'Runs'm (X'Reads tag) (R'Tagged tag r)

instance
  X'Runs'R tag param r responds v =>
  X'Runs'responds (X'Reads tag) responds

instance X'Runs'R tag param r responds v => X'Runs'result (X'Reads tag) v

instance
  X'Runs'R tag param r responds v =>
  X'Runs (X'Reads tag) param (R'Tagged tag r) responds v where
  x'runs'run p param m = do
    let r = x'runs'r'init @tag param
    run'R'Tagged @tag p r $ (/\) <$> m <*> x'runs'r'complete @tag p
  x'runs'respond = x'runs'r'respond @tag

----------------------------------------------------------------------

data R'Id t

instance X'Runs'R (R'Id t) t t (VariantF (get :: (->) t)) t where
  x'runs'r'init = identity
  x'runs'r'complete = r'tagged'askAt
  x'runs'r'respond p = match
    { get: \f -> r'tagged'askAt p <#> f
    }

-- instance X'respondsTo'get (X'Reads (R'Id t)) t _ where
--  x'respondsTo'get = R'Id'responds identity

----------------------------------------------------------------------

class
  X'Runs'responds tag responds <=
  X'respondsTo'get tag g responds
  | tag -> g responds where
  x'respondsTo'get :: responds g

x'get
  :: forall @p x x' m tag vfr t
   . ConsSymbol p (m p) x' x
  => X'Runs'm tag m
  => X'Runs'responds tag (VariantF (get :: (->) t | vfr))
  => Run x t
x'get = x'runs'respond @tag (Proxy @p) (inj (Proxy @"get") identity)

----------------------------------------------------------------------


r'doAsked
  :: forall @p tag r a x' x
   . ConsSymbol p (R'Tagged tag r p) x' x
  => Proxy p
  -> (r -> Eff'At p a)
  -> Run x a
r'doAsked p getEff = r'tagged'askAt p <#> eff'useTag ($) <<< getEff

----------------------------------------------------------------------

type X'R :: forall k. k -> Type
type X'R tag =
  forall p r param v. X'Runs'R p tag r param v => X'Runnable (X'Reads tag) r

x'R
  :: forall @p @tag r param v
   . X'Runs'R p tag r param v
  => r
  -> X'Runnable (X'Reads tag) r
x'R = X'Runnable (Proxy @(X'Reads tag))

type R'Tagged tag r = RunR.Reader (r /\ Proxy tag)

instance X'Runs'R p tag param r v => X'Runs'param p (X'Reads tag) param
instance X'Runs'R p tag param r v => X'Runs'm p (X'Reads tag) (R'Tagged tag r)
instance X'Runs'R p tag param r v => X'Runs'result p (X'Reads tag) v

instance
  X'Runs'R p tag param r v =>
  X'Runs p (X'Reads tag) param (R'Tagged tag r) v where
  x'run'impl param m = do
    let r = x'runs'r'init @p @tag param /\ Proxy @tag
    RunR.runReaderAt (Proxy @p) r $ (/\) <$> m <*> x'runs'r'complete @p @tag

class
  ( X'Runs'R p tag param r v
  , X'Runs p (X'Reads tag) param (R'Tagged tag r) v
  ) <=
  X'Runs'R'Tagged p tag param r v
  | p tag -> param r v

instance
  ( X'Runs'R p tag param r v
  , X'Runs p (X'Reads tag) param (R'Tagged tag r) v
  ) =>
  X'Runs'R'Tagged p tag param r v

r'doAsked
  :: forall @p tag r a x' x
   . IsSymbol p
  => Cons p (R'Tagged tag r) x' x
  => (r -> Eff'At p a)
  -> Run x a
r'doAsked getEff = RunR.askAt (Proxy @p) <#> eff'useTag ($) <<< getEff <<< fst

----------------------------------------------------------------------------

data X'Reads'Adapted :: forall k. k -> Type
data X'Reads'Adapted tag

type X'R'Adapted tag r = X'Runnable (X'Reads (X'Reads'Adapted tag)) r
type R'Tagged'Adapted tag r = R'Tagged (X'Reads'Adapted tag) r

x'Runnable'mk :: forall @tag r. r -> X'Runnable tag r
x'Runnable'mk = X'Runnable Proxy

x'R'Adapted
  :: forall @p @tag r param v
   . X'Runs'R'Adapted p tag r param v
  => r
  -> X'Runnable (X'Reads (X'Reads'Adapted tag)) r
x'R'Adapted = x'Runnable'mk

class X'Runs'R'Adapted p tag param r v | p tag -> param r v where
  x'runs'r'adapted'init :: param -> Effect r
  x'runs'r'adapted'complete
    :: forall x' x
     . IsSymbol p
    => Cons p (R'Tagged'Adapted tag r) x' x
    => Run x v

instance
  ( X'Runs'R'Adapted p tag param r v
  ) =>
  X'Runs'R p (X'Reads'Adapted tag) param r v where
  x'runs'r'init = unsafePerformEffect <<< x'runs'r'adapted'init @p @tag
  x'runs'r'complete = x'runs'r'adapted'complete @p @tag

----------------------------------------------------------------------------

class
  X'method'R'get p tag r v
  | p tag -> r v where
  x'method'R'get
    :: forall x' x
     . IsSymbol p
    => Cons p (R'Tagged'Adapted tag r) x' x
    => Run x v

x'get
  :: forall @p tag v x' x r
   . X'method'R'get p tag r v
  => IsSymbol p
  => Cons p (R'Tagged'Adapted tag r) x' x
  => Run x v
x'get = x'method'R'get @p @tag

----------------------------------------------------------------------------

type X'Eff'Permit = X'R Tag'Eff

----------------------------------------------------------------------------

-}

data Tag'Eff = Tag'Eff

instance Generable Tag'Eff gdesc Tag'Eff where
  mkGenerable = Tag'Eff

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