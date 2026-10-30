module Z.Z.X.Method
  ( (@#)
  , (@.)
  , (@<=>)
  , (@=)
  , T'x'add
  , T'x'addAt
  , T'x'alter
  , T'x'assign
  , T'x'assignAt
  , T'x'assignPure
  , T'x'clear
  , T'x'clearAt
  , T'x'cons
  , T'x'consAt
  , T'x'consLast
  , T'x'd1'cons
  , T'x'd1'entries
  , T'x'd1'keys
  , T'x'effCall
  , T'x'effGet
  , T'x'entries
  , T'x'entriesAt
  , T'x'extract
  , T'x'extractAt
  , T'x'has
  , T'x'insert
  , T'x'keys
  , T'x'keysAt
  , T'x'lookup
  , T'x'modify
  , T'x'over
  , T'x'over'b
  , T'x'pop
  , T'x'preview
  , T'x'preview'b
  , T'x'push
  , T'x'remove
  , T'x'replace
  , T'x'reset
  , T'x'resetAt
  , T'x'set
  , T'x'set'b
  , T'x'setPure
  , T'x'setPure'b
  , T'x'size
  , T'x'sizeAt
  , T'x'toArrayOf
  , T'x'toArrayOf'b
  , T'x'uncons
  , T'x'update
  , T'x'vals
  , T'x'valsAt
  , T'x'view
  , T'x'view'b
  , XG'clear
  , XG'd1'entries
  , XG'd1'keys
  , XG'entries
  , XG'extract
  , XG'keys
  , XG'pop
  , XG'reset
  , XG'size
  , XG'uncons
  , XG'vals
  , XM'add
  , XM'addAt
  , XM'assign
  , XM'assignAt
  , XM'clearAt
  , XM'cons
  , XM'consAt
  , XM'consLast
  , XM'd1'cons
  , XM'entriesAt
  , XM'extractAt
  , XM'insert
  , XM'keysAt
  , XM'lookup
  , XM'push
  , XM'remove
  , XM'resetAt
  , XM'sizeAt
  , XM'valsAt
  , XMt'd1'key
  , XMt'd1'value
  , XMt'd2'key
  , XMt'key
  , XMt'self
  , XMt'value
  , _'alter
  , _'cons
  , _'consAt
  , _'insert
  , _'lookup
  , _'modify
  , class Eff'Getter
  , class Eff'Method
  , class Eff'RW'Getter
  , class Eff'RW'Method
  , class X'Getter
  , class X'Method
  , class XC'assign
  , class XC'extract
  , class XM'type
  , eff'call
  , eff'get
  , eff'rw'call
  , eff'rw'get
  , x'call
  , x'get
  , x'add
  , x'addAt
  , x'alter
  , x'assign
  , x'assignAt
  , x'assignPure
  , x'clear
  , x'clearAt
  , x'cons
  , x'consAt
  , x'consLast
  , x'd1'cons
  , x'd1'entries
  , x'd1'keys
  , x'effCall
  , x'effGet
  , x'entries
  , x'entriesAt
  , x'extract
  , x'extractAt
  , x'has
  , x'insert
  , x'keys
  , x'keysAt
  , x'lookup
  , x'modify
  , x'over
  , x'over'b
  , x'pop
  , x'preview
  , x'preview'b
  , x'push
  , x'remove
  , x'replace
  , x'reset
  , x'resetAt
  , x'set
  , x'set'b
  , x'setPure
  , x'setPure'b
  , x'size
  , x'sizeAt
  , x'toArrayOf
  , x'toArrayOf'b
  , x'uncons
  , x'update
  , x'vals
  , x'valsAt
  , x'view
  , x'view'b
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Lens (Forget, Optic, over, preview, set, toArrayOf, view)
import Data.List (List)
import Data.Maybe (Maybe, isJust)
import Data.Maybe.First (First)
import Data.Monoid.Endo (Endo)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Record as Rec
import Type.Proxy (Proxy(..))
import Z.EXTERNAL.RUN.Run (Run)
import Z.EXTERNAL.RUN.Run.Reader (Reader, asksAt)
import Z.Z.Barlow (class C'Barlow, barlow)
import Z.Z.Core (class ConsSymbol, T'id, may'__, rec'get)
import Z.Z.Method (class Method, call)
import Z.Z.X.Core (T'use'_'AsSym, WithRWTag(..))

class X'Method :: forall k. k -> (Type -> Type) -> Type -> Type -> Constraint
class X'Method name m param res | name m -> param res where
  x'call :: forall p x' x. ConsSymbol p m x' x => Proxy p -> param -> Run x res

xAt'call
  :: forall @p @name x' x m param res
   . X'Method name m param res
  => ConsSymbol p m x' x
  => param
  -> Run x res
xAt'call = x'call @name (Proxy @p)

class Eff'Method :: forall k. k -> Type -> Type -> Type -> Constraint
class Eff'Method name t param res | name t -> param res where
  eff'call :: t -> param -> Effect res

class Eff'RW'Method
  :: forall k1 k2. k1 -> Type -> k2 -> Type -> Type -> Constraint
class Eff'RW'Method name t rwTag param res | name t -> rwTag param res where
  eff'rw'call :: t -> param -> Effect res

class X'Getter :: forall k. k -> (Type -> Type) -> Type -> Constraint
class X'Getter name m res | name m -> res where
  x'get :: forall p x' x. ConsSymbol p m x' x => Proxy p -> Run x res

xAt'get
  :: forall @p @name x' x m res
   . X'Getter name m res
  => ConsSymbol p m x' x
  => Run x res
xAt'get = x'get @name (Proxy @p)

class Eff'Getter :: forall k. k -> Type -> Type -> Constraint
class Eff'Getter name t res | name t -> res where
  eff'get :: t -> Effect res

class Eff'RW'Getter
  :: forall k1 k2. k1 -> Type -> k2 -> Type -> Constraint
class Eff'RW'Getter name t rwTag res | name t -> rwTag res where
  eff'rw'get :: t -> Effect res

instance
  ( Method name r param res
  ) =>
  X'Method name (Reader (Identity r)) param res where
  x'call p param = asksAt p \(Identity r) -> call @name r param
else instance
  ( Eff'RW'Method name r rwTag param res
  ) =>
  X'Method name (Reader (WithRWTag rwTag r)) param res where
  x'call p param =
    asksAt p \(WithRWTag r) -> unsafePerformEffect $ eff'rw'call @name r param
else instance
  ( Eff'Method name r param res
  ) =>
  X'Method name (Reader r) param res where
  x'call p param = asksAt p \r -> unsafePerformEffect $ eff'call @name r param

instance
  X'Getter XG'extract (Reader (Identity r)) r where
  x'get p = asksAt p \(Identity r) -> r
else instance
  ( Eff'RW'Getter name r rwTag res
  ) =>
  X'Getter name (Reader (WithRWTag rwTag r)) res where
  x'get p =
    asksAt p \(WithRWTag r) -> unsafePerformEffect $ eff'rw'get @name r
else instance
  X'Getter XG'extract (Reader r) r where
  x'get p = asksAt p \r -> r
else instance
  ( Eff'Getter name r res
  ) =>
  X'Getter name (Reader r) res where
  x'get p = asksAt p \r -> unsafePerformEffect $ eff'get @name r

class XM'type :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class XM'type m tag t | m tag -> t

instance XM'type r tag t => XM'type (Reader (WithRWTag rwTag r)) tag t

else instance XM'type (Reader (Identity r)) XMt'self r

else instance XM'type r tag t => XM'type (Reader (Identity r)) tag t

else instance XM'type (Reader r) XMt'self r

-----------------------------------------

foreign import data XMt'self :: Type

foreign import data XMt'value :: Type

foreign import data XMt'key :: Type

foreign import data XMt'd1'key :: Type

foreign import data XMt'd1'value :: Type

foreign import data XMt'd2'key :: Type

-----------------------------------------

foreign import data XM'assign :: Type

class (X'Method XM'assign m t Unit, XM'type m XMt'self t) <= XC'assign m t

instance (X'Method XM'assign m t Unit, XM'type m XMt'self t) => XC'assign m t

type T'x'assign use p =
  forall x' x m t
   . XC'assign m t
  => ConsSymbol p m x' x
  => use (t -> Run x Unit)

x'assign :: forall @p. T'x'assign T'id p
x'assign = xAt'call @p @XM'assign

infixl 6 x'assign as @=

-----------------------------------------

foreign import data XM'assignAt :: Type

type T'x'assignAt use p =
  forall x' x m k v
   . X'Method XM'assignAt m (k /\ v) Unit
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

x'assignAt :: forall @p. T'x'assignAt T'id p
x'assignAt = curry (xAt'call @p @XM'assignAt)

-----------------------------------------

type T'x'assignPure use p =
  forall x' x m t a
   . XC'assign m (a t)
  => ConsSymbol p m x' x
  => Applicative a
  => use (t -> Run x Unit)

x'assignPure :: forall @p. T'x'assignPure T'id p
x'assignPure v = xAt'call @p @XM'assign (pure v)

infixl 6 x'assignPure as @<=>

-----------------------------------------

foreign import data XG'reset :: Type

type T'x'reset use p =
  forall x' x m
   . X'Getter XG'reset m Unit
  => ConsSymbol p m x' x
  => use (Run x Unit)

x'reset :: forall @p. T'x'reset T'id p
x'reset = xAt'get @p @XG'reset

-----------------------------------------

foreign import data XM'resetAt :: Type

type T'x'resetAt use p =
  forall x' x m k
   . X'Method XM'resetAt m k Unit
  => XM'type m XMt'd1'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Unit)

x'resetAt :: forall @p. T'x'resetAt T'id p
x'resetAt = xAt'call @p @XM'resetAt

-----------------------------------------

foreign import data XM'sizeAt :: Type

type T'x'sizeAt use p =
  forall x' x m k
   . X'Method XM'sizeAt m k Int
  => XM'type m XMt'd1'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Int)

x'sizeAt :: forall @p. T'x'sizeAt T'id p
x'sizeAt = xAt'call @p @XM'sizeAt

-----------------------------------------

foreign import data XG'size :: Type

type T'x'size use p =
  forall x' x m
   . X'Getter XG'size m Int
  => ConsSymbol p m x' x
  => use (Run x Int)

x'size :: forall @p. T'x'size T'id p
x'size = xAt'get @p @XG'size

-----------------------------------------

foreign import data XM'valsAt :: Type

type T'x'valsAt use p =
  forall x' x m k v
   . X'Method XM'valsAt m k (Array v)
  => XM'type m XMt'd1'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> Run x (Array v))

x'valsAt :: forall @p. T'x'valsAt T'id p
x'valsAt = xAt'call @p @XM'valsAt

-----------------------------------------

foreign import data XG'vals :: Type

type T'x'vals use p =
  forall x' x m t
   . X'Getter XG'vals m (Array t)
  => XM'type m XMt'value t
  => ConsSymbol p m x' x
  => use (Run x (Array t))

x'vals :: forall @p. T'x'vals T'id p
x'vals = xAt'get @p @XG'vals

-----------------------------------------

foreign import data XM'keysAt :: Type

type T'x'keysAt use p =
  forall x' x m k1 k2
   . X'Method XM'keysAt m k1 (Array k2)
  => XM'type m XMt'd1'key k1
  => XM'type m XMt'd2'key k2
  => ConsSymbol p m x' x
  => use (k1 -> Run x (Array k2))

x'keysAt :: forall @p. T'x'keysAt T'id p
x'keysAt = xAt'call @p @XM'keysAt

-----------------------------------------

foreign import data XG'keys :: Type

type T'x'keys use p =
  forall x' x m t
   . X'Getter XG'keys m (Array t)
  => XM'type m XMt'key t
  => ConsSymbol p m x' x
  => use (Run x (Array t))

x'keys :: forall @p. T'x'keys T'id p
x'keys = xAt'get @p @XG'keys

-----------------------------------------

foreign import data XM'entriesAt :: Type

type T'x'entriesAt use p =
  forall x' x m k1 k2 v
   . X'Method XM'entriesAt m k1 (Array (k2 /\ v))
  => XM'type m XMt'd1'key k1
  => XM'type m XMt'd2'key k2
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k1 -> Run x (Array (k2 /\ v)))

x'entriesAt :: forall @p. T'x'entriesAt T'id p
x'entriesAt = xAt'call @p @XM'entriesAt

-----------------------------------------

foreign import data XG'entries :: Type

type T'x'entries use p =
  forall x' x m k v
   . X'Getter XG'entries m (Array (k /\ v))
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (Run x (Array (k /\ v)))

x'entries :: forall @p. T'x'entries T'id p
x'entries = xAt'get @p @XG'entries

-----------------------------------------

foreign import data XM'lookup :: Type

type T'x'lookup use p =
  forall x' x m k v
   . X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> Run x (Maybe v))

x'lookup :: forall @p. T'x'lookup T'id p
x'lookup = xAt'call @p @XM'lookup

-----------------------------------------

type T'x'has use p =
  forall x' x m k v
   . X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> Run x Boolean)

x'has :: forall @p. T'x'has T'id p
x'has k = x'lookup @p k <#> isJust

-----------------------------------------

foreign import data XG'd1'keys :: Type

type T'x'd1'keys use p =
  forall x' x m k
   . X'Getter XG'd1'keys m (Array k)
  => XM'type m XMt'd1'key k
  => ConsSymbol p m x' x
  => use (Run x (Array k))

x'd1'keys :: forall @p. T'x'd1'keys T'id p
x'd1'keys = xAt'get @p @XG'd1'keys

-----------------------------------------

foreign import data XG'd1'entries :: Type

type T'x'd1'entries use p =
  forall x' x m k v
   . X'Getter XG'd1'entries m (Array (k /\ v))
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (Run x (Array (k /\ v)))

x'd1'entries :: forall @p. T'x'd1'entries T'id p
x'd1'entries = xAt'get @p @XG'd1'entries

-----------------------------------------

foreign import data XG'clear :: Type

type T'x'clear use p =
  forall x' x m
   . X'Getter XG'clear m Unit
  => ConsSymbol p m x' x
  => use (Run x Unit)

x'clear :: forall @p. T'x'clear T'id p
x'clear = xAt'get @p @XG'clear

-----------------------------------------

foreign import data XM'clearAt :: Type

type T'x'clearAt use p =
  forall x' x m k
   . X'Method XM'clearAt m k Unit
  => XM'type m XMt'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Unit)

x'clearAt :: forall @p. T'x'clearAt T'id p
x'clearAt = xAt'call @p @XM'clearAt

-----------------------------------------

foreign import data XM'add :: Type

type T'x'add use p =
  forall x' x m t
   . X'Method XM'add m t Unit
  => XM'type m XMt'self t
  => ConsSymbol p m x' x
  => use (t -> Run x Unit)

x'add :: forall @p. T'x'add T'id p
x'add = xAt'call @p @XM'add

-----------------------------------------

foreign import data XM'addAt :: Type

type T'x'addAt use p =
  forall x' x m k v
   . X'Method XM'addAt m (k /\ v) Unit
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

x'addAt :: forall @p. T'x'addAt T'id p
x'addAt = curry (xAt'call @p @XM'addAt)

-----------------------------------------

foreign import data XM'remove :: Type

type T'x'remove use p =
  forall x' x m k
   . X'Method XM'remove m k Unit
  => XM'type m XMt'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Unit)

x'remove :: forall @p. T'x'remove T'id p
x'remove = xAt'call @p @XM'remove

-----------------------------------------

foreign import data XM'insert :: Type

type T'x'insert use p =
  forall x' x m k v
   . X'Method XM'insert m (k /\ v) Unit
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

x'insert :: forall @p. T'x'insert T'id p
x'insert = curry (xAt'call @p @XM'insert)

-----------------------------------------

foreign import data XM'cons :: Type

type T'x'cons use p =
  forall x' x m v
   . X'Method XM'cons m v Unit
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

x'cons :: forall @p. T'x'cons T'id p
x'cons = xAt'call @p @XM'cons

-----------------------------------------

foreign import data XG'uncons :: Type

type T'x'uncons use p =
  forall x' x m v
   . X'Getter XG'uncons m (Maybe v)
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (Run x (Maybe v))

x'uncons :: forall @p. T'x'uncons T'id p
x'uncons = xAt'get @p @XG'uncons

-----------------------------------------

foreign import data XM'push :: Type

type T'x'push use p =
  forall x' x m v
   . X'Method XM'push m v Unit
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

x'push :: forall @p. T'x'push T'id p
x'push = xAt'call @p @XM'push

-----------------------------------------

foreign import data XG'pop :: Type

type T'x'pop use p =
  forall x' x m v
   . X'Getter XG'pop m (Maybe v)
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (Run x (Maybe v))

x'pop :: forall @p. T'x'pop T'id p
x'pop = xAt'get @p @XG'pop

-----------------------------------------

foreign import data XM'consAt :: Type

type T'x'consAt use p =
  forall x' x m k v
   . X'Method XM'consAt m (k /\ v) Unit
  => XM'type m XMt'd1'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

x'consAt :: forall @p. T'x'consAt T'id p
x'consAt = curry (xAt'call @p @XM'consAt)

-----------------------------------------

foreign import data XM'consLast :: Type

type T'x'consLast use p =
  forall x' x m v
   . X'Method XM'consLast m v Unit
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

x'consLast :: forall @p. T'x'consLast T'id p
x'consLast = xAt'call @p @XM'consLast

-----------------------------------------

foreign import data XM'd1'cons :: Type

type T'x'd1'cons use p =
  forall x' x m v
   . X'Method XM'd1'cons m v Unit
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

x'd1'cons :: forall @p. T'x'd1'cons T'id p
x'd1'cons = xAt'call @p @XM'd1'cons

-----------------------------------------

type T'x'replace use p =
  forall x' x m k v
   . X'Method XM'insert m (k /\ v) Unit
  => X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

x'replace :: forall @p. T'x'replace T'id p
x'replace k v = x'has @p k >>= flip when (x'insert @p k v)

-----------------------------------------

type T'x'alter use p =
  forall x' x m k v
   . X'Method XM'insert m (k /\ v) Unit
  => X'Method XM'remove m k Unit
  => X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> (Maybe v -> Maybe v) -> Run x Unit)

x'alter :: forall @p. T'x'alter T'id p
x'alter k f = x'lookup @p k <#> f >>= may'__
  (\_ -> x'remove @p k)
  (\v -> x'insert @p k v)

-----------------------------------------

type T'x'modify use p =
  forall x' x m k v
   . X'Method XM'insert m (k /\ v) Unit
  => X'Method XM'remove m k Unit
  => X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> (v -> v) -> Run x Unit)

x'modify :: forall @p. T'x'modify T'id p
x'modify k f = x'lookup @p k <#> map f >>= may'__
  (\_ -> x'remove @p k)
  (\v -> x'insert @p k v)

-----------------------------------------

foreign import data XM'extractAt :: Type

type T'x'extractAt use p =
  forall x' x m k v
   . X'Method XM'extractAt m k v
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (k -> Run x v)

x'extractAt :: forall @p. T'x'extractAt T'id p
x'extractAt = xAt'call @p @XM'extractAt

-----------------------------------------

foreign import data XG'extract :: Type

class (X'Getter XG'extract m t, XM'type m XMt'self t) <= XC'extract m t

instance (X'Getter XG'extract m t, XM'type m XMt'self t) => XC'extract m t

type T'x'extract use p =
  forall x' x m t
   . XC'extract m t
  => ConsSymbol p m x' x
  => use (Run x t)

x'extract :: forall @p. T'x'extract T'id p
x'extract = xAt'get @p @XG'extract

infixl 6 x'extract as @.

-------------------------------------------------------------------------

type T'x'effCall px pm =
  forall m x' x nt r' r param a
   . XC'extract m nt
  => Newtype nt { | r }
  => ConsSymbol px m x' x
  => ConsSymbol pm (param -> Effect a) r' r
  => param
  -> Run x a

x'effCall :: forall @px @pm. T'x'effCall px pm
x'effCall param = x'extract @px <#> unwrap <#> rec'get @pm <#> (#) param <#>
  unsafePerformEffect

-------------------------------------------------------------------------

type T'x'effGet px pm =
  forall m x' x nt r' r a
   . XC'extract m nt
  => Newtype nt { | r }
  => ConsSymbol px m x' x
  => ConsSymbol pm (Effect a) r' r
  => Run x a

x'effGet :: forall @px @pm. T'x'effGet px pm
x'effGet = x'extract @px <#> unwrap <#> rec'get @pm <#> unsafePerformEffect

-----------------------------------------

type T'x'update use p =
  forall x' x m t
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use ((t -> t) -> Run x Unit)

x'update :: forall @p. T'x'update T'id p
x'update f = f <$> x'extract @p >>= x'assign @p

infixl 6 x'update as @#

-----------------------------------------

type T'x'view use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic (Forget a) t t a b -> Run x a)

x'view :: forall @p. T'x'view T'id p
x'view l = x'extract @p <#> view l

-----------------------------------------

type T'x'view'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses (Forget a) t t a b
  => use (Run x a)

x'view'b :: forall @p @sym. T'x'view'b T'id sym p
x'view'b = x'extract @p <#> view (barlow @sym)

-----------------------------------------

type T'x'preview use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic (Forget (First a)) t t a b -> Run x (Maybe a))

x'preview :: forall @p. T'x'preview T'id p
x'preview l = x'extract @p <#> preview l

-----------------------------------------

type T'x'preview'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses (Forget (First a)) t t a b
  => use (Run x (Maybe a))

x'preview'b :: forall @p @sym. T'x'preview'b T'id sym p
x'preview'b = x'extract @p <#> preview (barlow @sym)

-----------------------------------------

type T'x'toArrayOf use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic (Forget (Endo Function (List a))) t t a b -> Run x (Array a))

x'toArrayOf :: forall @p. T'x'toArrayOf T'id p
x'toArrayOf l = x'extract @p <#> toArrayOf l

-----------------------------------------

type T'x'toArrayOf'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses (Forget (Endo Function (List a))) t t a b
  => use (Run x (Array a))

x'toArrayOf'b :: forall @p @sym. T'x'toArrayOf'b T'id sym p
x'toArrayOf'b = x'extract @p <#> toArrayOf (barlow @sym)

-----------------------------------------

type T'x'set use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic Function t t a b -> b -> Run x Unit)

x'set :: forall @p. T'x'set T'id p
x'set l v = x'update @p (set l v)

-----------------------------------------

type T'x'set'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses Function t t a b
  => use (b -> Run x Unit)

x'set'b :: forall @p @sym. T'x'set'b T'id sym p
x'set'b v = x'update @p (set (barlow @sym) v)

-----------------------------------------

type T'x'setPure use p =
  forall x' x m t a b app
   . XC'extract m t
  => XC'assign m t
  => Applicative app
  => ConsSymbol p m x' x
  => use (Optic Function t t a (app b) -> b -> Run x Unit)

x'setPure :: forall @p. T'x'setPure T'id p
x'setPure l v = x'update @p (set l (pure v))

-----------------------------------------

type T'x'setPure'b use sym p =
  forall lenses x' x m t a b app
   . XC'extract m t
  => XC'assign m t
  => Applicative app
  => ConsSymbol p m x' x
  => C'Barlow sym lenses Function t t a (app b)
  => use (b -> Run x Unit)

x'setPure'b :: forall @p @sym. T'x'setPure'b T'id sym p
x'setPure'b v = x'update @p (set (barlow @sym) (pure v))

-----------------------------------------

type T'x'over use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic Function t t a b -> (a -> b) -> Run x Unit)

x'over :: forall @p. T'x'over T'id p
x'over l v = x'update @p (over l v)

-----------------------------------------

type T'x'over'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses Function t t a b
  => use ((a -> b) -> Run x Unit)

x'over'b :: forall @p @sym. T'x'over'b T'id sym p
x'over'b v = x'update @p (over (barlow @sym) v)

--------------------------------------------------------------

_'insert :: forall p. T'use'_'AsSym p (T'x'insert T'id)
_'insert = x'insert @p

_'lookup :: forall p. T'use'_'AsSym p (T'x'lookup T'id)
_'lookup = x'lookup @p

_'cons :: forall p. T'use'_'AsSym p (T'x'cons T'id)
_'cons = x'cons @p

_'consAt :: forall p. T'use'_'AsSym p (T'x'consAt T'id)
_'consAt = x'consAt @p

_'alter :: forall p. T'use'_'AsSym p (T'x'alter T'id)
_'alter = x'alter @p

_'modify :: forall p. T'use'_'AsSym p (T'x'modify T'id)
_'modify = x'modify @p