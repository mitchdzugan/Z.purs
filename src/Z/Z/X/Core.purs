module Z.Z.X.Core
  ( Def'Sel'Evaluable
  , Def'Sel'm
  , Def'Sel'result
  , R'Tagged
  , RW'Tag'R
  , RW'Tag'RW
  , RW'Tag'W
  , RW'Tagged
  , T'Consable
  , T'Evaluable
  , T'use'_'AsSym
  , W'Tagged
  , WithRWTag(..)
  , X'Evaluable(..)
  , X'EvaluableAt(..)
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
  , x'build
  , x'buildable'eval
  , x'buildable'rl'run
  , x'buildable'run
  , x'consable'impl
  , x'eval
  , x'eval_
  , x'evaluable
  , x'evaluableAt_
  , x'evaluable_
  , x'exec
  , x'exec_
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

instance X'Results'R r result => X'Results (Reader r) result where
  x'results'impl p = askAt p <#> x'results'r'impl @r

class X'Results'R'rw r result | r -> result where
  x'results'r'rw'impl :: r -> Effect result

class X'Results'R'Eff r result | r -> result where
  x'results'r'eff'impl :: r -> Effect result

instance X'Results'R'rw r result => X'Results'R (WithRWTag tag r) result where
  x'results'r'impl (WithRWTag r) = unsafePerformEffect $ x'results'r'rw'impl r
else instance X'Results'R'Eff r result => X'Results'R r result where
  x'results'r'impl r = unsafePerformEffect $ x'results'r'eff'impl r

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
  mkGenerable = x'evaluable_ @m @param

x'evaluable
  :: forall @m param
   . X'Consable m param Identity
  => param
  -> X'Evaluable m
x'evaluable param =
  X'Evaluable \p m -> x'consable'impl @m p param m <#> \(Identity v) -> v

x'evaluable_
  :: forall @m @param
   . X'Consable m param Identity
  => Generable param GDefault param
  => X'Evaluable m
x'evaluable_ = x'evaluable @m $ g @param

evaluable'run
  :: forall @p @m x' x @param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => X'Evaluable m
  -> Run x a
  -> Run x' a
evaluable'run (X'Evaluable mf) = mf (Proxy @p)

type T'x'eval m p =
  forall x' x param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => param
  -> Run x a
  -> Run x' a

x'eval :: forall @p @m. T'x'eval m p
x'eval param m = pure unit >>= \_ ->
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
x'run param m = x'eval @p param do
  a <- m
  result <- x'results'impl @m (Proxy @p)
  pure $ a /\ result

type T'x'exec m p =
  forall x' x param result
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => X'Results m result
  => param
  -> Run x Unit
  -> Run x' result

x'exec :: forall @p @m. T'x'exec m p
x'exec param m = x'eval @p param $ m *> x'results'impl @m (Proxy @p)

type T'x'eval_ m p =
  forall x' x param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => Generable param GDefault param
  => Run x a
  -> Run x' a

x'eval_ :: forall @p @m. T'x'eval_ m p
x'eval_ m = x'eval @p @m default m

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

type T'x'exec_ m p =
  forall param x' x result
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => X'Results m result
  => Generable param GDefault param
  => Run x Unit
  -> Run x' result

x'exec_ :: forall @p @m. T'x'exec_ m p
x'exec_ m = x'exec @p @m default m

---------------------------------------------------------------------

type T'use'_'AsSym p f = T'useAsSym "_" p f

_'run :: forall @m p. T'use'_'AsSym p (T'x'run m)
_'run = x'run @p

_'exec :: forall @m p. T'use'_'AsSym p (T'x'exec m)
_'exec = x'exec @p

_'eval :: forall @m p. T'use'_'AsSym p (T'x'eval m)
_'eval = x'eval @p

_'run_ :: forall @m p. T'use'_'AsSym p (T'x'run_ m)
_'run_ = x'run_ @p

_'exec_ :: forall @m p. T'use'_'AsSym p (T'x'exec_ m)
_'exec_ = x'exec_ @p

_'eval_ :: forall @m p. T'use'_'AsSym p (T'x'eval_ m)
_'eval_ = x'eval_ @p

------------------------------------------------------------------------------

instance
  X'Readable r param =>
  X'Consable (Reader r) param Identity where
  x'consable'impl = impl
    where
    impl :: forall p. Proxy p -> T'Consable p (Reader r) param Identity
    impl p param m =
      wrap <$> runReaderAt p (unsafePerformEffect $ x'readable'mk @r param) m

class X'Readable r param | r -> param where
  x'readable'mk :: param -> Effect r

------------------------------------------------------------------------------

instance X'Readable (Identity r) r where
  x'readable'mk = pure <<< Identity

instance X'Results'R'Eff (Identity r) r where
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
  x'buildable'run :: forall a. spec -> Run x a -> Run x' (a /\ result)

x'buildable'eval
  :: forall a spec x' x result
   . X'Buildable spec x' x result
  => spec
  -> Run x a
  -> Run x' a
x'buildable'eval spec m = x'buildable'run @spec @x' spec m <#> fst

x'build
  :: forall spec x' x result
   . X'Buildable spec x' x result
  => spec
  -> Run x Unit
  -> Run x' result
x'build spec m = x'buildable'run @spec @x' spec m <#> snd

instance
  ( ConsSymbol p m x' x
  , X'Consable m param Identity
  , X'Results m result
  ) =>
  X'Buildable (X'EvaluableAt p m) x' x result where
  x'buildable'run (X'EvaluableAt e) m = evaluable'run @p @m @param e do
    a <- m
    result <- x'results'impl @m $ Proxy @p
    pure $ a /\ result

instance
  ( RL.RowToList spec'row spec'rl
  , X'Buildable'RL spec'row spec'rl x' x result'row
  ) =>
  X'Buildable (Record spec'row) x' x (Record result'row) where
  x'buildable'run = x'buildable'rl'run @spec'row @spec'rl

---------------------------------------------------------------------

class X'Buildable'RL
  :: forall k1
   . Row Type
  -> k1
  -> Row (Type -> Type)
  -> Row (Type -> Type)
  -> Row Type
  -> Constraint
class
  X'Buildable'RL spec'row spec'rl x' x result'row
  | spec'rl -> spec'row x' x result'row where
  x'buildable'rl'run
    :: forall a. Record spec'row -> Run x a -> Run x' (a /\ Record result'row)

instance X'Buildable'RL () RL.Nil x x () where
  x'buildable'rl'run _ m = m <#> flip (/\) {}

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
  x'buildable'rl'run r m = do
    (a /\ nxt) /\ prt <-
      x'buildable'rl'run @spec'row'tail @spec'rl'tail (unsafeCoerce r)
        $ evaluable'run @k @m @param (rec'get @k r) do
            a <- m
            nxt <- x'results'impl @m $ Proxy @k
            pure $ a /\ nxt
    pure $ a /\ rec'insert @k nxt prt

---------------------------------------------------------------------

newtype X'EvaluableAt :: Symbol -> (Type -> Type) -> Type
newtype X'EvaluableAt p mf = X'EvaluableAt (X'Evaluable mf)

x'evaluableAt_
  :: forall @p @mf param
   . X'Consable mf param Identity
  => Generable param GDefault param
  => X'EvaluableAt p mf
x'evaluableAt_ = X'EvaluableAt $ x'evaluable_ @mf @param

---------------------------------------------------------------------

type Def'Sel'Evaluable :: forall k. (Type -> Type) -> k -> Type
type Def'Sel'Evaluable m _result = X'Evaluable m

type Def'Sel'm :: forall k. (Type -> Type) -> k -> (Type -> Type)
type Def'Sel'm m _result = m

type Def'Sel'result :: forall k. (Type -> Type) -> k -> k
type Def'Sel'result _m result = result
