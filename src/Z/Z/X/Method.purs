module Z.Z.X.Method
  ( (@#)
  , (@.)
  , (@<=>)
  , (@=)
  , T'xm'add
  , T'xm'addAt
  , T'xm'alter
  , T'xm'assign
  , T'xm'assignAt
  , T'xm'assignPure
  , T'xm'clear
  , T'xm'clearAt
  , T'xm'cons
  , T'xm'consAt
  , T'xm'consLast
  , T'xm'd1'cons
  , T'xm'd1'entries
  , T'xm'd1'keys
  , T'xm'effCall
  , T'xm'effGet
  , T'xm'entries
  , T'xm'entriesAt
  , T'xm'extract
  , T'xm'extractAt
  , T'xm'has
  , T'xm'insert
  , T'xm'keys
  , T'xm'keysAt
  , T'xm'lookup
  , T'xm'modify
  , T'xm'over
  , T'xm'over'b
  , T'xm'pop
  , T'xm'preview
  , T'xm'preview'b
  , T'xm'push
  , T'xm'remove
  , T'xm'replace
  , T'xm'reset
  , T'xm'resetAt
  , T'xm'set
  , T'xm'set'b
  , T'xm'setPure
  , T'xm'setPure'b
  , T'xm'size
  , T'xm'sizeAt
  , T'xm'toArrayOf
  , T'xm'toArrayOf'b
  , T'xm'uncons
  , T'xm'update
  , T'xm'vals
  , T'xm'valsAt
  , T'xm'view
  , T'xm'view'b
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
  , xm'add
  , xm'addAt
  , xm'alter
  , xm'assign
  , xm'assignAt
  , xm'assignPure
  , xm'clear
  , xm'clearAt
  , xm'cons
  , xm'consAt
  , xm'consLast
  , xm'd1'cons
  , xm'd1'entries
  , xm'd1'keys
  , xm'effCall
  , xm'effGet
  , xm'entries
  , xm'entriesAt
  , xm'extract
  , xm'extractAt
  , xm'has
  , xm'insert
  , xm'keys
  , xm'keysAt
  , xm'lookup
  , xm'modify
  , xm'over
  , xm'over'b
  , xm'pop
  , xm'preview
  , xm'preview'b
  , xm'push
  , xm'remove
  , xm'replace
  , xm'reset
  , xm'resetAt
  , xm'set
  , xm'set'b
  , xm'setPure
  , xm'setPure'b
  , xm'size
  , xm'sizeAt
  , xm'toArrayOf
  , xm'toArrayOf'b
  , xm'uncons
  , xm'update
  , xm'vals
  , xm'valsAt
  , xm'view
  , xm'view'b
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

type T'xm'assign use p =
  forall x' x m t
   . XC'assign m t
  => ConsSymbol p m x' x
  => use (t -> Run x Unit)

xm'assign :: forall @p. T'xm'assign T'id p
xm'assign = xAt'call @p @XM'assign

infixl 6 xm'assign as @=

-----------------------------------------

foreign import data XM'assignAt :: Type

type T'xm'assignAt use p =
  forall x' x m k v
   . X'Method XM'assignAt m (k /\ v) Unit
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

xm'assignAt :: forall @p. T'xm'assignAt T'id p
xm'assignAt = curry (xAt'call @p @XM'assignAt)

-----------------------------------------

type T'xm'assignPure use p =
  forall x' x m t a
   . XC'assign m (a t)
  => ConsSymbol p m x' x
  => Applicative a
  => use (t -> Run x Unit)

xm'assignPure :: forall @p. T'xm'assignPure T'id p
xm'assignPure v = xAt'call @p @XM'assign (pure v)

infixl 6 xm'assignPure as @<=>

-----------------------------------------

foreign import data XG'reset :: Type

type T'xm'reset use p =
  forall x' x m
   . X'Getter XG'reset m Unit
  => ConsSymbol p m x' x
  => use (Run x Unit)

xm'reset :: forall @p. T'xm'reset T'id p
xm'reset = xAt'get @p @XG'reset

-----------------------------------------

foreign import data XM'resetAt :: Type

type T'xm'resetAt use p =
  forall x' x m k
   . X'Method XM'resetAt m k Unit
  => XM'type m XMt'd1'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Unit)

xm'resetAt :: forall @p. T'xm'resetAt T'id p
xm'resetAt = xAt'call @p @XM'resetAt

-----------------------------------------

foreign import data XM'sizeAt :: Type

type T'xm'sizeAt use p =
  forall x' x m k
   . X'Method XM'sizeAt m k Int
  => XM'type m XMt'd1'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Int)

xm'sizeAt :: forall @p. T'xm'sizeAt T'id p
xm'sizeAt = xAt'call @p @XM'sizeAt

-----------------------------------------

foreign import data XG'size :: Type

type T'xm'size use p =
  forall x' x m
   . X'Getter XG'size m Int
  => ConsSymbol p m x' x
  => use (Run x Int)

xm'size :: forall @p. T'xm'size T'id p
xm'size = xAt'get @p @XG'size

-----------------------------------------

foreign import data XM'valsAt :: Type

type T'xm'valsAt use p =
  forall x' x m k v
   . X'Method XM'valsAt m k (Array v)
  => XM'type m XMt'd1'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> Run x (Array v))

xm'valsAt :: forall @p. T'xm'valsAt T'id p
xm'valsAt = xAt'call @p @XM'valsAt

-----------------------------------------

foreign import data XG'vals :: Type

type T'xm'vals use p =
  forall x' x m t
   . X'Getter XG'vals m (Array t)
  => XM'type m XMt'value t
  => ConsSymbol p m x' x
  => use (Run x (Array t))

xm'vals :: forall @p. T'xm'vals T'id p
xm'vals = xAt'get @p @XG'vals

-----------------------------------------

foreign import data XM'keysAt :: Type

type T'xm'keysAt use p =
  forall x' x m k1 k2
   . X'Method XM'keysAt m k1 (Array k2)
  => XM'type m XMt'd1'key k1
  => XM'type m XMt'd2'key k2
  => ConsSymbol p m x' x
  => use (k1 -> Run x (Array k2))

xm'keysAt :: forall @p. T'xm'keysAt T'id p
xm'keysAt = xAt'call @p @XM'keysAt

-----------------------------------------

foreign import data XG'keys :: Type

type T'xm'keys use p =
  forall x' x m t
   . X'Getter XG'keys m (Array t)
  => XM'type m XMt'key t
  => ConsSymbol p m x' x
  => use (Run x (Array t))

xm'keys :: forall @p. T'xm'keys T'id p
xm'keys = xAt'get @p @XG'keys

-----------------------------------------

foreign import data XM'entriesAt :: Type

type T'xm'entriesAt use p =
  forall x' x m k1 k2 v
   . X'Method XM'entriesAt m k1 (Array (k2 /\ v))
  => XM'type m XMt'd1'key k1
  => XM'type m XMt'd2'key k2
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k1 -> Run x (Array (k2 /\ v)))

xm'entriesAt :: forall @p. T'xm'entriesAt T'id p
xm'entriesAt = xAt'call @p @XM'entriesAt

-----------------------------------------

foreign import data XG'entries :: Type

type T'xm'entries use p =
  forall x' x m k v
   . X'Getter XG'entries m (Array (k /\ v))
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (Run x (Array (k /\ v)))

xm'entries :: forall @p. T'xm'entries T'id p
xm'entries = xAt'get @p @XG'entries

-----------------------------------------

foreign import data XM'lookup :: Type

type T'xm'lookup use p =
  forall x' x m k v
   . X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> Run x (Maybe v))

xm'lookup :: forall @p. T'xm'lookup T'id p
xm'lookup = xAt'call @p @XM'lookup

-----------------------------------------

type T'xm'has use p =
  forall x' x m k v
   . X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> Run x Boolean)

xm'has :: forall @p. T'xm'has T'id p
xm'has k = xm'lookup @p k <#> isJust

-----------------------------------------

foreign import data XG'd1'keys :: Type

type T'xm'd1'keys use p =
  forall x' x m k
   . X'Getter XG'd1'keys m (Array k)
  => XM'type m XMt'd1'key k
  => ConsSymbol p m x' x
  => use (Run x (Array k))

xm'd1'keys :: forall @p. T'xm'd1'keys T'id p
xm'd1'keys = xAt'get @p @XG'd1'keys

-----------------------------------------

foreign import data XG'd1'entries :: Type

type T'xm'd1'entries use p =
  forall x' x m k v
   . X'Getter XG'd1'entries m (Array (k /\ v))
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (Run x (Array (k /\ v)))

xm'd1'entries :: forall @p. T'xm'd1'entries T'id p
xm'd1'entries = xAt'get @p @XG'd1'entries

-----------------------------------------

foreign import data XG'clear :: Type

type T'xm'clear use p =
  forall x' x m
   . X'Getter XG'clear m Unit
  => ConsSymbol p m x' x
  => use (Run x Unit)

xm'clear :: forall @p. T'xm'clear T'id p
xm'clear = xAt'get @p @XG'clear

-----------------------------------------

foreign import data XM'clearAt :: Type

type T'xm'clearAt use p =
  forall x' x m k
   . X'Method XM'clearAt m k Unit
  => XM'type m XMt'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Unit)

xm'clearAt :: forall @p. T'xm'clearAt T'id p
xm'clearAt = xAt'call @p @XM'clearAt

-----------------------------------------

foreign import data XM'add :: Type

type T'xm'add use p =
  forall x' x m t
   . X'Method XM'add m t Unit
  => XM'type m XMt'self t
  => ConsSymbol p m x' x
  => use (t -> Run x Unit)

xm'add :: forall @p. T'xm'add T'id p
xm'add = xAt'call @p @XM'add

-----------------------------------------

foreign import data XM'addAt :: Type

type T'xm'addAt use p =
  forall x' x m k v
   . X'Method XM'addAt m (k /\ v) Unit
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

xm'addAt :: forall @p. T'xm'addAt T'id p
xm'addAt = curry (xAt'call @p @XM'addAt)

-----------------------------------------

foreign import data XM'remove :: Type

type T'xm'remove use p =
  forall x' x m k
   . X'Method XM'remove m k Unit
  => XM'type m XMt'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Unit)

xm'remove :: forall @p. T'xm'remove T'id p
xm'remove = xAt'call @p @XM'remove

-----------------------------------------

foreign import data XM'insert :: Type

type T'xm'insert use p =
  forall x' x m k v
   . X'Method XM'insert m (k /\ v) Unit
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

xm'insert :: forall @p. T'xm'insert T'id p
xm'insert = curry (xAt'call @p @XM'insert)

-----------------------------------------

foreign import data XM'cons :: Type

type T'xm'cons use p =
  forall x' x m v
   . X'Method XM'cons m v Unit
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

xm'cons :: forall @p. T'xm'cons T'id p
xm'cons = xAt'call @p @XM'cons

-----------------------------------------

foreign import data XG'uncons :: Type

type T'xm'uncons use p =
  forall x' x m v
   . X'Getter XG'uncons m (Maybe v)
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (Run x (Maybe v))

xm'uncons :: forall @p. T'xm'uncons T'id p
xm'uncons = xAt'get @p @XG'uncons

-----------------------------------------

foreign import data XM'push :: Type

type T'xm'push use p =
  forall x' x m v
   . X'Method XM'push m v Unit
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

xm'push :: forall @p. T'xm'push T'id p
xm'push = xAt'call @p @XM'push

-----------------------------------------

foreign import data XG'pop :: Type

type T'xm'pop use p =
  forall x' x m v
   . X'Getter XG'pop m (Maybe v)
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (Run x (Maybe v))

xm'pop :: forall @p. T'xm'pop T'id p
xm'pop = xAt'get @p @XG'pop

-----------------------------------------

foreign import data XM'consAt :: Type

type T'xm'consAt use p =
  forall x' x m k v
   . X'Method XM'consAt m (k /\ v) Unit
  => XM'type m XMt'd1'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

xm'consAt :: forall @p. T'xm'consAt T'id p
xm'consAt = curry (xAt'call @p @XM'consAt)

-----------------------------------------

foreign import data XM'consLast :: Type

type T'xm'consLast use p =
  forall x' x m v
   . X'Method XM'consLast m v Unit
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

xm'consLast :: forall @p. T'xm'consLast T'id p
xm'consLast = xAt'call @p @XM'consLast

-----------------------------------------

foreign import data XM'd1'cons :: Type

type T'xm'd1'cons use p =
  forall x' x m v
   . X'Method XM'd1'cons m v Unit
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

xm'd1'cons :: forall @p. T'xm'd1'cons T'id p
xm'd1'cons = xAt'call @p @XM'd1'cons

-----------------------------------------

type T'xm'replace use p =
  forall x' x m k v
   . X'Method XM'insert m (k /\ v) Unit
  => X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

xm'replace :: forall @p. T'xm'replace T'id p
xm'replace k v = xm'has @p k >>= flip when (xm'insert @p k v)

-----------------------------------------

type T'xm'alter use p =
  forall x' x m k v
   . X'Method XM'insert m (k /\ v) Unit
  => X'Method XM'remove m k Unit
  => X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> (Maybe v -> Maybe v) -> Run x Unit)

xm'alter :: forall @p. T'xm'alter T'id p
xm'alter k f = xm'lookup @p k <#> f >>= may'__
  (\_ -> xm'remove @p k)
  (\v -> xm'insert @p k v)

-----------------------------------------

type T'xm'modify use p =
  forall x' x m k v
   . X'Method XM'insert m (k /\ v) Unit
  => X'Method XM'remove m k Unit
  => X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> (v -> v) -> Run x Unit)

xm'modify :: forall @p. T'xm'modify T'id p
xm'modify k f = xm'lookup @p k <#> map f >>= may'__
  (\_ -> xm'remove @p k)
  (\v -> xm'insert @p k v)

-----------------------------------------

foreign import data XM'extractAt :: Type

type T'xm'extractAt use p =
  forall x' x m k v
   . X'Method XM'extractAt m k v
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (k -> Run x v)

xm'extractAt :: forall @p. T'xm'extractAt T'id p
xm'extractAt = xAt'call @p @XM'extractAt

-----------------------------------------

foreign import data XG'extract :: Type

class (X'Getter XG'extract m t, XM'type m XMt'self t) <= XC'extract m t

instance (X'Getter XG'extract m t, XM'type m XMt'self t) => XC'extract m t

type T'xm'extract use p =
  forall x' x m t
   . XC'extract m t
  => ConsSymbol p m x' x
  => use (Run x t)

xm'extract :: forall @p. T'xm'extract T'id p
xm'extract = xAt'get @p @XG'extract

infixl 6 xm'extract as @.

-------------------------------------------------------------------------

type T'xm'effCall px pm =
  forall m x' x nt r' r param a
   . XC'extract m nt
  => Newtype nt { | r }
  => ConsSymbol px m x' x
  => ConsSymbol pm (param -> Effect a) r' r
  => param
  -> Run x a

xm'effCall :: forall @px @pm. T'xm'effCall px pm
xm'effCall param = xm'extract @px <#> unwrap <#> rec'get @pm <#> (#) param <#>
  unsafePerformEffect

-------------------------------------------------------------------------

type T'xm'effGet px pm =
  forall m x' x nt r' r a
   . XC'extract m nt
  => Newtype nt { | r }
  => ConsSymbol px m x' x
  => ConsSymbol pm (Effect a) r' r
  => Run x a

xm'effGet :: forall @px @pm. T'xm'effGet px pm
xm'effGet = xm'extract @px <#> unwrap <#> rec'get @pm <#> unsafePerformEffect

-----------------------------------------

type T'xm'update use p =
  forall x' x m t
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use ((t -> t) -> Run x Unit)

xm'update :: forall @p. T'xm'update T'id p
xm'update f = f <$> xm'extract @p >>= xm'assign @p

infixl 6 xm'update as @#

-----------------------------------------

type T'xm'view use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic (Forget a) t t a b -> Run x a)

xm'view :: forall @p. T'xm'view T'id p
xm'view l = xm'extract @p <#> view l

-----------------------------------------

type T'xm'view'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses (Forget a) t t a b
  => use (Run x a)

xm'view'b :: forall @p @sym. T'xm'view'b T'id sym p
xm'view'b = xm'extract @p <#> view (barlow @sym)

-----------------------------------------

type T'xm'preview use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic (Forget (First a)) t t a b -> Run x (Maybe a))

xm'preview :: forall @p. T'xm'preview T'id p
xm'preview l = xm'extract @p <#> preview l

-----------------------------------------

type T'xm'preview'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses (Forget (First a)) t t a b
  => use (Run x (Maybe a))

xm'preview'b :: forall @p @sym. T'xm'preview'b T'id sym p
xm'preview'b = xm'extract @p <#> preview (barlow @sym)

-----------------------------------------

type T'xm'toArrayOf use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic (Forget (Endo Function (List a))) t t a b -> Run x (Array a))

xm'toArrayOf :: forall @p. T'xm'toArrayOf T'id p
xm'toArrayOf l = xm'extract @p <#> toArrayOf l

-----------------------------------------

type T'xm'toArrayOf'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses (Forget (Endo Function (List a))) t t a b
  => use (Run x (Array a))

xm'toArrayOf'b :: forall @p @sym. T'xm'toArrayOf'b T'id sym p
xm'toArrayOf'b = xm'extract @p <#> toArrayOf (barlow @sym)

-----------------------------------------

type T'xm'set use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic Function t t a b -> b -> Run x Unit)

xm'set :: forall @p. T'xm'set T'id p
xm'set l v = xm'update @p (set l v)

-----------------------------------------

type T'xm'set'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses Function t t a b
  => use (b -> Run x Unit)

xm'set'b :: forall @p @sym. T'xm'set'b T'id sym p
xm'set'b v = xm'update @p (set (barlow @sym) v)

-----------------------------------------

type T'xm'setPure use p =
  forall x' x m t a b app
   . XC'extract m t
  => XC'assign m t
  => Applicative app
  => ConsSymbol p m x' x
  => use (Optic Function t t a (app b) -> b -> Run x Unit)

xm'setPure :: forall @p. T'xm'setPure T'id p
xm'setPure l v = xm'update @p (set l (pure v))

-----------------------------------------

type T'xm'setPure'b use sym p =
  forall lenses x' x m t a b app
   . XC'extract m t
  => XC'assign m t
  => Applicative app
  => ConsSymbol p m x' x
  => C'Barlow sym lenses Function t t a (app b)
  => use (b -> Run x Unit)

xm'setPure'b :: forall @p @sym. T'xm'setPure'b T'id sym p
xm'setPure'b v = xm'update @p (set (barlow @sym) (pure v))

-----------------------------------------

type T'xm'over use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic Function t t a b -> (a -> b) -> Run x Unit)

xm'over :: forall @p. T'xm'over T'id p
xm'over l v = xm'update @p (over l v)

-----------------------------------------

type T'xm'over'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses Function t t a b
  => use ((a -> b) -> Run x Unit)

xm'over'b :: forall @p @sym. T'xm'over'b T'id sym p
xm'over'b v = xm'update @p (over (barlow @sym) v)

--------------------------------------------------------------

_'insert :: forall p. T'use'_'AsSym p (T'xm'insert T'id)
_'insert = xm'insert @p

_'lookup :: forall p. T'use'_'AsSym p (T'xm'lookup T'id)
_'lookup = xm'lookup @p

_'cons :: forall p. T'use'_'AsSym p (T'xm'cons T'id)
_'cons = xm'cons @p

_'consAt :: forall p. T'use'_'AsSym p (T'xm'consAt T'id)
_'consAt = xm'consAt @p

_'alter :: forall p. T'use'_'AsSym p (T'xm'alter T'id)
_'alter = xm'alter @p

_'modify :: forall p. T'use'_'AsSym p (T'xm'modify T'id)
_'modify = xm'modify @p