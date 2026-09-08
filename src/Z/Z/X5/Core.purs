module Z.Z.X5.Core where

import Z.Z.X5.UtilPrelude

import Data.Exists (Exists, mkExists, runExists)
import Prim.RowList as RL
import Z.Z.Eff.Tagged (Eff'Tagged, eff'useTag)
import Z.Z.X5.Responds
  ( Responds0
  , Responds1
  , responds0'id
  , responds0'run
  , responds1'id
  )

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
  ( X'RespondsTo mf (VariantF (result :: Responds0 result | vfr))
  ) =>
  X'Resultable mf result where
  x'resultable'impl p = x'respondsTo'impl @mf p
    (inj (Proxy @"result") responds0'id)

data X'Tag :: forall k. k -> Type
data X'Tag tag

data X'Rel''tag''mf

type X'mf :: forall k. k -> Type
type X'mf tag = forall mf. C'Relate tag X'Rel''tag''mf mf => mf

class
  ( C'Relate tag X'Rel''tag''mf mf
  , X'Consable mf param res'm
  , X'RespondsTo mf (VariantF (result :: Responds0 result | responds'rest))
  , X'Resultable mf result
  , Functor res'm
  ) <=
  X'Runs tag mf param responds'rest res'm result
  | tag -> mf param responds'rest res'm result

instance
  ( C'Relate tag X'Rel''tag''mf mf
  , X'Consable mf param res'm
  , X'RespondsTo mf (VariantF (result :: Responds0 result | responds'rest))
  , X'Resultable mf result
  , Functor res'm
  ) =>
  X'Runs tag mf param responds'rest res'm result

x'consable'run
  :: forall @p x' x a @tag mf param res'm
   . X'Consable mf param res'm
  => C'Relate tag X'Rel''tag''mf mf
  => ConsSymbol p (mf p) x' x
  => param
  -> Run x a
  -> Run x' (res'm a)
x'consable'run = x'consable'impl @mf (Proxy @p)

x'resultable'run
  :: forall @p x' x @tag mf result
   . X'Resultable mf result
  => C'Relate tag X'Rel''tag''mf mf
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

xm'run :: forall @tag @p. TR'h tag p T'id T'apply T'id (/\)
xm'run param m = x'consable'run @p @tag param do
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
  forall mf param res'm
   . C'Relate tag X'Rel''tag''mf mf
  => X'Consable mf param res'm
  => param

type T'Runnable mf =
  forall p x' x a
   . ConsSymbol p (mf p) x' x
  => Proxy p
  -> Run x a
  -> Run x' a

newtype X'Runnable mf = X'Runnable (T'Runnable mf)

instance C'Relate (X'Runnable mf) X'Rel''tag''mf mf

x'runnable
  :: forall @tag mf param
   . C'Relate tag X'Rel''tag''mf mf
  => X'Consable mf param Identity
  => param
  -> X'Runnable mf
x'runnable param = X'Runnable \p m -> unwrap <$> x'consable'impl p param m

x'runnable'run'impl
  :: forall @p mf x' x a
   . ConsSymbol p (mf p) x' x
  => X'Runnable mf
  -> Run x a
  -> Run x' a
x'runnable'run'impl (X'Runnable f) m = f (Proxy @p) m

x'run
  :: forall @p mf result x' x a
   . X'Resultable mf result
  => ConsSymbol p (mf p) x' x
  => X'Runnable mf
  -> Run x a
  -> Run x' (a /\ result)
x'run (X'Runnable f) m = f (Proxy @p) $ do
  a <- m
  result <- x'resultable'impl @mf (Proxy @p)
  pure $ a /\ result

x'eval
  :: forall @p mf result x' x a
   . X'Resultable mf result
  => ConsSymbol p (mf p) x' x
  => X'Runnable mf
  -> Run x a
  -> Run x' a
x'eval rn m = fst <$> x'run @p rn m

x'exec
  :: forall @p mf result x' x
   . X'Resultable mf result
  => ConsSymbol p (mf p) x' x
  => X'Runnable mf
  -> Run x Unit
  -> Run x' result
x'exec rn m = snd <$> x'run @p rn m

----------------------------------------------------------------------
----------------------------------------------------------------------

newtype R'Tagged rf p a = R'Tagged'F (rf p /\ Proxy rf -> a)

derive instance Functor (R'Tagged r p)

handle'R'Tagged :: forall p @rf a. Proxy p -> rf p -> R'Tagged rf p ~> Run a
handle'R'Tagged _ t (R'Tagged'F f) = pure $ f $ t /\ Proxy

r'tagged'run
  :: forall p @rf x' x a
   . ConsSymbol p (R'Tagged rf p) x' x
  => Proxy p
  -> rf p
  -> Run x a
  -> Run x' a
r'tagged'run p r = run (on p (handle'R'Tagged p r) send)

r'tagged'askAt
  :: forall @p rf r x' x
   . ConsSymbol p (R'Tagged rf p) x' x
  => Newtype (rf p) r
  => Proxy p
  -> Run x r
r'tagged'askAt p = lift p (R'Tagged'F fst) <#> unwrap

----------------------------------------------------------------------

data X'Tag'rf :: forall k. k -> Type
data X'Tag'rf tag

data X'Rel''tag''rf

class X'Tags'rf :: forall k1 k2. k1 -> k2 -> Constraint
class X'Tags'rf tag rf | tag -> rf

instance (C'Relate tag X'Rel''tag''rf rf) => X'Tags'rf (X'Tag'rf tag) rf
else instance X'Tags'rf rf rf

type X'rf :: forall k. k -> Type
type X'rf tag = forall rf. X'Tags'rf tag rf => rf

----------------------------------------------------------------------

class X'Consable'R :: forall k. (k -> Type) -> Type -> Constraint
class X'Consable'R rf param | rf -> param where
  x'consable'R'impl :: forall p. Proxy p -> param -> rf p

instance
  ( X'Consable'R rf param
  ) =>
  X'Consable (R'Tagged rf) param Identity where
  x'consable'impl p param m =
    Identity <$> r'tagged'run @rf p (x'consable'R'impl @rf Proxy param) m

class X'RespondsTo'R rf responds | rf -> responds where
  x'respondsTo'R'impl
    :: forall p x' x a
     . ConsSymbol p (R'Tagged rf p) x' x
    => Proxy p
    -> responds a
    -> Run x a

instance
  ( X'RespondsTo'R rf responds
  ) =>
  X'RespondsTo (R'Tagged rf) responds where
  x'respondsTo'impl = x'respondsTo'R'impl @rf

----------------------------------------------------------------------
----------------------------------------------------------------------

newtype R'Identity t p = R'Identity (t /\ Proxy p)

derive instance Newtype (R'Identity t p) _

instance X'Consable'R (R'Identity t) t where
  x'consable'R'impl p v = wrap $ wrapped'mk v p

type R'Identity'reponds t = VariantF
  ( get :: Responds0 t
  , result :: Responds0 Unit
  )

instance X'RespondsTo'R (R'Identity t) (R'Identity'reponds t) where
  x'respondsTo'R'impl p = match
    { get: responds0'run $ wrapped'get <$> r'tagged'askAt p
    , result: responds0'run $ pure unit
    }

----------------------------------------------------------------------

newtype X'R'Adapted rf p = X'R'Adapted (rf p)

derive instance Newtype (X'R'Adapted rf p) _

type R'Adapted rf = R'Tagged (X'R'Adapted rf)

class X'Consable'R'Adapted rf param | rf -> param where
  x'consable'R'adapted'impl :: forall p. Proxy p -> param -> Effect (rf p)

instance
  ( X'Consable'R'Adapted rf param
  ) =>
  X'Consable'R (X'R'Adapted rf) param where
  x'consable'R'impl p =
    X'R'Adapted <<< unsafePerformEffect <<< x'consable'R'adapted'impl @rf p

class X'RespondsTo'R'Adapted rf responds | rf -> responds where
  x'respondsTo'R'adapted'impl
    :: forall p x' x a
     . ConsSymbol p (R'Tagged (X'R'Adapted rf) p) x' x
    => Proxy p
    -> responds a
    -> Run x a

instance
  ( X'RespondsTo'R'Adapted rf responds
  ) =>
  X'RespondsTo'R (X'R'Adapted rf) responds where
  x'respondsTo'R'impl = x'respondsTo'R'adapted'impl @rf

r'doAsked
  :: forall @p @rf r a x' x
   . ConsSymbol p (R'Tagged (X'R'Adapted rf) p) x' x
  => Newtype (rf p) r
  => Proxy p
  -> (r -> Eff'Tagged p a)
  -> Run x a
r'doAsked p getEff =
  r'tagged'askAt p <#> unwrap <#> eff'useTag ($) <<< getEff

----------------------------------------------------------------------------

data Tag'Eff p = Tag'Eff

instance X'Consable'R Tag'Eff Unit where
  x'consable'R'impl _ _ = Tag'Eff

type R'Tag'Eff'reponds = VariantF (result :: Responds0 Unit)

instance X'RespondsTo'R Tag'Eff R'Tag'Eff'reponds where
  x'respondsTo'R'impl _ = match { result: responds0'run $ pure unit }

----------------------------------------------------------------------------

type X'Permit :: forall k. k -> Type -> Type
type X'Permit p = R'Tagged Tag'Eff p

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------

x'get
  :: forall @p x x' mf responds'rest t
   . ConsSymbol p (mf p) x' x
  => X'RespondsTo mf (VariantF (get :: Responds0 t | responds'rest))
  => Run x t
x'get = x'respondsTo'impl @mf (Proxy @p) (inj (Proxy @"get") responds0'id)

x'set
  :: forall @p x x' mf responds'rest t
   . ConsSymbol p (mf p) x' x
  => X'RespondsTo mf (VariantF (set :: Responds1 t Unit | responds'rest))
  => t
  -> Run x Unit
x'set t = x'respondsTo'impl @mf (Proxy @p) (inj (Proxy @"set") (responds1'id t))

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------

class X'Cons consable x' x | consable x' -> x

{-
instance
  ConsSymbol p (mf p) x' x =>
  X'Cons p (X'Runnable mf) x' x (mf p)
-}

------------------------------------------------------------------------

class X'Cons'RL m'rl p m'row x'rl x'row | m'rl p -> m'row x'rl x'row

instance X'Cons'RL RL.Nil p () RL.Nil ()

type RLCons'XR p mf tail = RL.Cons p (X'Runnable mf) tail

type RLCons'mf
  :: forall k. Symbol -> (Symbol -> k) -> RL.RowList k -> RL.RowList k
type RLCons'mf p mf tail = RL.Cons p (mf p) tail

instance
  ( IsSymbol p
  , Cons p (X'Runnable mf) m'row'tail m'row
  , Cons p (mf p) x'row'tail x'row
  , X'Cons'RL m'rl'tail p m'row'tail x'rl'tail x'row'tail
  ) =>
  X'Cons'RL (RLCons'XR p mf m'rl'tail) p m'row (RLCons'mf p mf x'rl'tail) x'row

data X'Cons'Tag p mf

instance (IsSymbol p, Cons p (mf p) x' x) => X'Cons (X'Cons'Tag p mf) x' x