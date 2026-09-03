module Z.Z.X5.Core where

import Z.Z.X5.UtilPrelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Prim.RowList as RL
import Run.Reader as RunR
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Z.Z.Defaultable (class Generable, mkGenerable)
import Z.Z.Wraps (class Unwraps, class Wraps, wrapped'from, wrapped'get)

class X'Runs'param p tag param | p tag -> param
class X'Runs'm p tag m | p tag -> m
class X'Runs'result p tag result | p tag -> result

class
  ( X'Runs'param p tag param
  , X'Runs'm p tag m
  , X'Runs'result p tag result
  ) <=
  X'Runs p tag param m result
  | p tag -> param m result where
  x'run'impl
    :: forall x' x a
     . IsSymbol p
    => Cons p m x' x
    => param
    -> Run x a
    -> Run x' (a /\ result)

class X'Using'RL spec'rl spec'row x' x | spec'rl x' -> spec'row x

instance X'Using'RL RL.Nil x x ()

instance
  ( IsSymbol p
  , X'Runs'm p foc'spec foc'm
  , Cons k foc'spec spec'row'tail spec'row
  , Cons p foc'm x' x
  ) =>
  X'Using'RL (RL.Cons p foc'spec spec'rl'tail) spec'row x' x

class X'Using spec'row x' x | spec'row x' -> x

instance
  ( RL.RowToList spec'row spec'rl
  , X'Using'RL spec'rl spec'row x' x
  ) =>
  X'Using (Record spec'row) x' x

x'run
  :: forall @tag param m result @p x' x a
   . IsSymbol p
  => X'Runs p tag param m result
  => Cons p m x' x
  => param
  -> Run x a
  -> Run x' (a /\ result)
x'run = x'run'impl @p @tag

x'exec
  :: forall @tag param m result @p x' x
   . IsSymbol p
  => X'Runs p tag param m result
  => Cons p m x' x
  => param
  -> Run x Unit
  -> Run x' result
x'exec r m = x'run @tag @p r m <#> snd

x'eval
  :: forall @tag param m result @p x' x a
   . IsSymbol p
  => X'Runs p tag param m result
  => Cons p m x' x
  => param
  -> Run x a
  -> Run x' a
x'eval r m = x'run @tag @p r m <#> fst

data X'Runnable tag param = X'Runnable (Proxy tag) param

----------------------------------------------------------------------

class X'Runs'R p tag param r v | p tag -> param r v where
  x'runs'r'init :: param -> r
  x'runs'r'complete
    :: forall x' x
     . IsSymbol p
    => Cons p (R'Tagged tag r) x' x
    => Run x v

class X'Runs'R'r p tag r | p tag -> r

instance X'Runs'R p (X'Reads tag) param r v => X'Runs'R'r p tag r

data X'Reads tag

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

data Tag'Eff = Tag'Eff

instance Generable Tag'Eff gdesc Tag'Eff where
  mkGenerable = Tag'Eff

type X'Eff'Permit = X'R Tag'Eff

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