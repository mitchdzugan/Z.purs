module Z.Z.X5.Core where

import Z.Z.X5.UtilPrelude

import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Prim.RowList as RL
import Run.Reader as RunR
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Z.Z.Core (class ConsSymbol)
import Z.Z.Defaultable (class Generable, mkGenerable)
import Z.Z.Wraps (class Unwraps, class Wraps, wrapped'from, wrapped'get)

class X'Runs'param tag param | tag -> param
class X'Runs'm tag m | m -> tag
class X'Runs'responds tag responds | tag -> responds
class X'Runs'result tag result | tag -> result

class
  ( X'Runs'param tag param
  , X'Runs'm tag m
  , X'Runs'responds tag responds
  , X'Runs'result tag result
  ) <=
  X'Runs tag param m responds result
  | tag -> param m responds result where
  x'runs'run
    :: forall p x' x a
     . ConsSymbol p (m p) x' x
    => Proxy p
    -> param
    -> Run x a
    -> Run x' (a /\ result)
  x'runs'respond
    :: forall p x' x a
     . ConsSymbol p (m p) x' x
    => Proxy p
    -> responds a
    -> Run x a

x'run
  :: forall @tag param m responds result @p x' x a
   . X'Runs tag param m responds result
  => ConsSymbol p (m p) x' x
  => param
  -> Run x a
  -> Run x' (a /\ result)
x'run = x'runs'run @tag $ Proxy @p

x'exec
  :: forall @tag param m responds result @p x' x
   . X'Runs tag param m responds result
  => ConsSymbol p (m p) x' x
  => param
  -> Run x Unit
  -> Run x' result
x'exec r m = x'run @tag @p r m <#> snd

x'eval
  :: forall @tag param m responds result @p x' x a
   . IsSymbol p
  => X'Runs tag param m responds result
  => ConsSymbol p (m p) x' x
  => param
  -> Run x a
  -> Run x' a
x'eval r m = x'run @tag @p r m <#> fst

----------------------------------------------------------------------

type X'Runnable'Param tag = forall param. X'Runs'param tag param => param
type X'Runnable'm tag = forall m. X'Runs'm tag m => m
type X'Runnable'responds tag =
  forall responds. X'Runs'responds tag responds => responds

type X'Runnable'result tag = forall result. X'Runs'result tag result => result

data X'Runnable tag = X'Runnable (Proxy tag) (X'Runnable'Param tag)

----------------------------------------------------------------------

newtype R'Tagged tag r p a = R'Tagged'F (r /\ Proxy tag -> a)

handle'R'Tagged :: forall p @tag r a. Proxy p -> r -> R'Tagged tag r p ~> Run a
handle'R'Tagged _ t (R'Tagged'F f) = pure $ f $ t /\ Proxy

run'R'Tagged
  :: forall p @tag r x' x a
   . ConsSymbol p (R'Tagged tag r p) x' x
  => Proxy p
  -> r
  -> Run x a
  -> Run x' a
run'R'Tagged p r = run (on p (handle'R'Tagged p r) send)

derive instance Functor (R'Tagged tag r p)

class X'Runs'R tag param r responds v | tag -> param r responds v where
  x'runs'r'init :: param -> r
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

{-

x'get
  :: forall @p x x' m tag vfr t
   . ConsSymbol p (m p) x' x
  => X'Runs'm tag m
  => X'Runs'responds tag (VariantF (get :: (->) t | vfr))
  => Run x t
x'get = x'runs'respond @tag (Proxy @p) (inj (Proxy @"get") identity)

-}

----------------------------------------------------------------------

r'doAsked
  :: forall @p tag r a x' x
   . ConsSymbol p (R'Tagged tag r p) x' x
  => Proxy p
  -> (r -> Eff'At p a)
  -> Run x a
r'doAsked p getEff = r'tagged'askAt p <#> eff'useTag ($) <<< getEff

r'tagged'askAt
  :: forall @p tag r x' x
   . ConsSymbol p (R'Tagged tag r p) x' x
  => Proxy p
  -> Run x r
r'tagged'askAt p = lift p (R'Tagged'F fst)

{-

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