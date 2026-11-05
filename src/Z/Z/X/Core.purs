module Z.Z.X.Core
  ( Def'Sel'Evaluable
  , Def'Sel'm
  , Def'Sel'result
  , Eff'Reader(..)
  , R'Tagged
  , RW'Tag'R
  , RW'Tag'RW
  , RW'Tag'W
  , RW'Tagged
  , Reader(..)
  , Run
  , T'Consable
  , T'Evaluable
  , T'use'_'AsSym
  , W'Tagged
  , WithRWTag(..)
  , X'Evaluable(..)
  , _'eval
  , _'eval_
  , _'exec
  , _'exec_
  , _'run
  , _'run_
  , class RW'Tag'CanRead
  , class RW'Tag'CanWrite
  , class X'Buildable
  , class X'Buildable'RL
  , class X'Consable
  , class X'Readable
  , class X'Readable'rw
  , class X'Results
  , class X'Results'R
  , class X'Results'R'Eff
  , class X'Results'R'rw
  , evaluable'run
  , x_build
  , x_buildable'eval
  , x_buildable'rl'run
  , x_buildable'run
  , x'consable'impl
  , x_eval
  , x_eval_
  , x_evaluable
  , x_evaluable_
  , x_exec
  , x_exec_
  , x'readable'mk
  , x'readable'rw'mk
  , x'results'impl
  , x'results'r'eff'impl
  , x'results'r'impl
  , x'results'r'rw'impl
  , x'run
  , x'run_
  ) where

import Z.Z.X.UtilPrelude

import Prim.RowList as RL
import Unsafe.Coerce (unsafeCoerce)
import Z.Z.Core (T'useAsSym, rec'get, rec'insert)
import Z.Z.Run (RunS, ctxR'askAt, ctxR'runAt)

type Run x = RunS x

newtype Reader r = Reader r

derive instance Newtype (Reader r) _

newtype Eff'Reader r = Eff'Reader r

derive instance Newtype (Eff'Reader r) _

type T'Consable p m param res'm =
  forall x' x a
   . ConsSymbol p m x' x
  => param
  -> Run x a
  -> Run x' (res'm a)

class X'Consable m param res'm | m -> param res'm where
  x'consable'impl :: forall p. Proxy p -> T'Consable p m param res'm

------------------------------------------------------------------------------

class X'Results m result | m -> result where
  x'results'impl
    :: forall p x' x. ConsSymbol p m x' x => Proxy p -> Run x result

class X'Results'R r result where
  x'results'r'impl :: r -> result

class X'Results'R'rw r result | r -> result where
  x'results'r'rw'impl :: r -> Effect result

class X'Results'R'Eff r result | r -> result where
  x'results'r'eff'impl :: r -> Effect result

instance
  X'Results'R'rw r result =>
  X'Results'R'Eff (WithRWTag tag r) result where
  x'results'r'eff'impl (WithRWTag r) = x'results'r'rw'impl r

instance X'Results'R r result => X'Results (Reader r) result where
  x'results'impl
    :: forall p x' x. ConsSymbol p (Reader r) x' x => Proxy p -> Run x result
  x'results'impl _ = ctxR'askAt @p <#> unwrap <#> x'results'r'impl @r
else instance X'Results'R'Eff r result => X'Results r result where
  x'results'impl
    :: forall p x' x. ConsSymbol p r x' x => Proxy p -> Run x result
  x'results'impl _ =
    ctxR'askAt @p <#> x'results'r'eff'impl <#> unsafePerformEffect

------------------------------------------------------------------------------

type T'Evaluable m =
  forall p x' x a
   . ConsSymbol p m x' x
  => Proxy p
  -> Run x a
  -> Run x' a

newtype X'Evaluable m = X'Evaluable (T'Evaluable m)

instance
  ( X'Consable m param Identity
  , Generable param GDefault param
  ) =>
  Generable (X'Evaluable m) GDefault (X'Evaluable m) where
  mkGenerable = x_evaluable_ @m @param

x_evaluable
  :: forall @m param
   . X'Consable m param Identity
  => param
  -> X'Evaluable m
x_evaluable param =
  X'Evaluable \p m -> x'consable'impl @m p param m <#> \(Identity v) -> v

x_evaluable_
  :: forall @m @param
   . X'Consable m param Identity
  => Generable param GDefault param
  => X'Evaluable m
x_evaluable_ = x_evaluable @m $ g @param

evaluable'run
  :: forall @p @m x' x @param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => X'Evaluable m
  -> Run x a
  -> Run x' a
evaluable'run (X'Evaluable mf) = mf (Proxy @p)

type T'x_eval m p =
  forall x' x param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => param
  -> Run x a
  -> Run x' a

x_eval :: forall @p @m. T'x_eval m p
x_eval param m = pure unit >>= \_ ->
  x'consable'impl @m (Proxy @p) param m <#> \(Identity v) -> v

type T'x'run m p =
  forall x' x param a result
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => X'Results m result
  => param
  -> Run x a
  -> Run x' (a /\ result)

x'run :: forall @p @m. T'x'run m p
x'run param m = x_eval @p param do
  a <- m
  result <- x'results'impl @m (Proxy @p)
  pure $ a /\ result

type T'x_exec m p =
  forall x' x param result
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => X'Results m result
  => param
  -> Run x Unit
  -> Run x' result

x_exec :: forall @p @m. T'x_exec m p
x_exec param m = x_eval @p param $ m *> x'results'impl @m (Proxy @p)

type T'x_eval_ m p =
  forall x' x param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => Generable param GDefault param
  => Run x a
  -> Run x' a

x_eval_ :: forall @p @m. T'x_eval_ m p
x_eval_ m = x_eval @p @m default m

type T'x'run_ m p =
  forall x' x param result a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => X'Results m result
  => Generable param GDefault param
  => Run x a
  -> Run x' (a /\ result)

x'run_ :: forall @p @m. T'x'run_ m p
x'run_ m = x'run @p @m default m

type T'x_exec_ m p =
  forall param x' x result
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => X'Results m result
  => Generable param GDefault param
  => Run x Unit
  -> Run x' result

x_exec_ :: forall @p @m. T'x_exec_ m p
x_exec_ m = x_exec @p @m default m

---------------------------------------------------------------------

type T'use'_'AsSym p f = T'useAsSym "_" p f

_'run :: forall @m p. T'use'_'AsSym p (T'x'run m)
_'run = x'run @p

_'exec :: forall @m p. T'use'_'AsSym p (T'x_exec m)
_'exec = x_exec @p

_'eval :: forall @m p. T'use'_'AsSym p (T'x_eval m)
_'eval = x_eval @p

_'run_ :: forall @m p. T'use'_'AsSym p (T'x'run_ m)
_'run_ = x'run_ @p

_'exec_ :: forall @m p. T'use'_'AsSym p (T'x_exec_ m)
_'exec_ = x_exec_ @p

_'eval_ :: forall @m p. T'use'_'AsSym p (T'x_eval_ m)
_'eval_ = x_eval_ @p

------------------------------------------------------------------------------

instance
  X'Readable r param =>
  X'Consable r param Identity where
  x'consable'impl = impl
    where
    impl :: forall p. Proxy p -> T'Consable p r param Identity
    impl p param m =
      wrap <$> ctxR'runAt @p
        (unsafePerformEffect $ x'readable'mk @r param)
        m

class X'Readable r param | r -> param where
  x'readable'mk :: param -> Effect r

------------------------------------------------------------------------------

instance X'Readable (Reader r) r where
  x'readable'mk = pure <<< wrap

instance X'Results'R'Eff (Eff'Reader r) r where
  x'results'r'eff'impl = pure <<< unwrap

------------------------------------------------------------------------------

instance X'Readable Unit Unit where
  x'readable'mk = pure

instance X'Results'R'Eff Unit Unit where
  x'results'r'eff'impl _ = pure unit

---------------------------------------------------------------------

foreign import data RW'Tag'R :: Type
foreign import data RW'Tag'W :: Type
foreign import data RW'Tag'RW :: Type

class RW'Tag'CanRead t

instance RW'Tag'CanRead RW'Tag'R
instance RW'Tag'CanRead RW'Tag'RW

class RW'Tag'CanWrite w

instance RW'Tag'CanWrite RW'Tag'W
instance RW'Tag'CanWrite RW'Tag'RW

newtype WithRWTag tag t = WithRWTag t

type R'Tagged r = WithRWTag RW'Tag'R r
type W'Tagged r = WithRWTag RW'Tag'W r
type RW'Tagged r = WithRWTag RW'Tag'RW r

---------------------------------------------------------------------

class X'Readable'rw r param | r -> param where
  x'readable'rw'mk :: param -> Effect r

instance X'Readable'rw r param => X'Readable (WithRWTag tag r) param where
  x'readable'mk param = WithRWTag <$> x'readable'rw'mk @r param

---------------------------------------------------------------------

class X'Buildable spec x' x result | spec x' -> x result where
  x_buildable'run :: forall a. spec -> Run x a -> Run x' (a /\ result)

x_buildable'eval
  :: forall a spec x' x result
   . X'Buildable spec x' x result
  => spec
  -> Run x a
  -> Run x' a
x_buildable'eval spec m = x_buildable'run @spec @x' spec m <#> fst

x_build
  :: forall spec x' x result
   . X'Buildable spec x' x result
  => spec
  -> Run x Unit
  -> Run x' result
x_build spec m = x_buildable'run @spec @x' spec m <#> snd

instance
  ( RL.RowToList spec'row spec'rl
  , X'Buildable'RL spec'row spec'rl x' x result'row
  ) =>
  X'Buildable (Record spec'row) x' x (Record result'row) where
  x_buildable'run = x_buildable'rl'run @spec'row @spec'rl

---------------------------------------------------------------------

class X'Buildable'RL
  :: forall k1
   . Row Type
  -> k1
  -> Row Type
  -> Row Type
  -> Row Type
  -> Constraint
class
  X'Buildable'RL spec'row spec'rl x' x result'row
  | spec'rl -> spec'row x' x result'row where
  x_buildable'rl'run
    :: forall a. Record spec'row -> Run x a -> Run x' (a /\ Record result'row)

instance X'Buildable'RL () RL.Nil x x () where
  x_buildable'rl'run _ m = m <#> flip (/\) {}

instance
  ( IsSymbol k
  , X'Buildable'RL spec'row'tail spec'rl'tail x'' x' result'row'tail
  , Cons k (X'Evaluable m) spec'row'tail spec'row
  , Cons k m x' x
  , Lacks k result'row'tail
  , Cons k result result'row'tail result'row
  , X'Consable m param Identity
  , X'Results m result
  ) =>
  X'Buildable'RL spec'row
    (RL.Cons k (X'Evaluable m) spec'rl'tail)
    x''
    x
    result'row where
  x_buildable'rl'run r m = do
    (a /\ nxt) /\ prt <-
      x_buildable'rl'run @spec'row'tail @spec'rl'tail (unsafeCoerce r)
        $ evaluable'run @k @m @param (rec'get @k r) do
            a <- m
            nxt <- x'results'impl @m $ Proxy @k
            pure $ a /\ nxt
    pure $ a /\ rec'insert @k nxt prt

---------------------------------------------------------------------

type Def'Sel'Evaluable :: forall k. Type -> k -> Type
type Def'Sel'Evaluable m _result = X'Evaluable m

type Def'Sel'm :: forall k. Type -> k -> Type
type Def'Sel'm m _result = m

type Def'Sel'result :: forall k. Type -> k -> k
type Def'Sel'result _m result = result
