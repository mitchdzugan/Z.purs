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
import Type.Proxy (Proxy)
import Web.Storage.Event.StorageEvent (key)
import Z.EXTERNAL.RUN.Run (Run)
import Z.EXTERNAL.RUN.Run.Reader (Reader, asksAt)
import Z.Z.Barlow (class C'Barlow, barlow)
import Z.Z.Core (class ConsSymbol, Eff'At, may'__, rec'get)
import Z.Z.Method (class Method, call)
import Z.Z.X.Core (WithRWTag(..))

class X'Method :: forall k. k -> (Type -> Type) -> Type -> Type -> Constraint
class X'Method name m param res | name m -> param res where
  x'call :: forall p x' x. ConsSymbol p m x' x => Proxy p -> param -> Run x res

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

xm'assign :: forall p. T'xm'assign ((->) (Proxy p)) p
xm'assign = x'call @XM'assign

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

xm'assignAt :: forall p. T'xm'assignAt ((->) (Proxy p)) p
xm'assignAt p = curry (x'call @XM'assignAt p)

-----------------------------------------

type T'xm'assignPure use p =
  forall x' x m t a
   . XC'assign m (a t)
  => ConsSymbol p m x' x
  => Applicative a
  => use (t -> Run x Unit)

xm'assignPure :: forall p. T'xm'assignPure ((->) (Proxy p)) p
xm'assignPure p v = x'call @XM'assign p (pure v)

infixl 6 xm'assignPure as @<=>

-----------------------------------------

foreign import data XG'reset :: Type

type T'xm'reset use p =
  forall x' x m
   . X'Getter XG'reset m Unit
  => ConsSymbol p m x' x
  => use (Run x Unit)

xm'reset :: forall p. T'xm'reset ((->) (Proxy p)) p
xm'reset = x'get @XG'reset

-----------------------------------------

foreign import data XM'resetAt :: Type

type T'xm'resetAt use p =
  forall x' x m k
   . X'Method XM'resetAt m k Unit
  => XM'type m XMt'd1'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Unit)

xm'resetAt :: forall p. T'xm'resetAt ((->) (Proxy p)) p
xm'resetAt = x'call @XM'resetAt

-----------------------------------------

foreign import data XM'sizeAt :: Type

type T'xm'sizeAt use p =
  forall x' x m k
   . X'Method XM'sizeAt m k Int
  => XM'type m XMt'd1'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Int)

xm'sizeAt :: forall p. T'xm'sizeAt ((->) (Proxy p)) p
xm'sizeAt = x'call @XM'sizeAt

-----------------------------------------

foreign import data XG'size :: Type

type T'xm'size use p =
  forall x' x m
   . X'Getter XG'size m Int
  => ConsSymbol p m x' x
  => use (Run x Int)

xm'size :: forall p. T'xm'size ((->) (Proxy p)) p
xm'size = x'get @XG'size

-----------------------------------------

foreign import data XM'valsAt :: Type

type T'xm'valsAt use p =
  forall x' x m k v
   . X'Method XM'valsAt m k (Array v)
  => XM'type m XMt'd1'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> Run x (Array v))

xm'valsAt :: forall p. T'xm'valsAt ((->) (Proxy p)) p
xm'valsAt = x'call @XM'valsAt

-----------------------------------------

foreign import data XG'vals :: Type

type T'xm'vals use p =
  forall x' x m t
   . X'Getter XG'vals m (Array t)
  => XM'type m XMt'value t
  => ConsSymbol p m x' x
  => use (Run x (Array t))

xm'vals :: forall p. T'xm'vals ((->) (Proxy p)) p
xm'vals = x'get @XG'vals

-----------------------------------------

foreign import data XM'keysAt :: Type

type T'xm'keysAt use p =
  forall x' x m k1 k2
   . X'Method XM'keysAt m k1 (Array k2)
  => XM'type m XMt'd1'key k1
  => XM'type m XMt'd2'key k2
  => ConsSymbol p m x' x
  => use (k1 -> Run x (Array k2))

xm'keysAt :: forall p. T'xm'keysAt ((->) (Proxy p)) p
xm'keysAt = x'call @XM'keysAt

-----------------------------------------

foreign import data XG'keys :: Type

type T'xm'keys use p =
  forall x' x m t
   . X'Getter XG'keys m (Array t)
  => XM'type m XMt'key t
  => ConsSymbol p m x' x
  => use (Run x (Array t))

xm'keys :: forall p. T'xm'keys ((->) (Proxy p)) p
xm'keys = x'get @XG'keys

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

xm'entriesAt :: forall p. T'xm'entriesAt ((->) (Proxy p)) p
xm'entriesAt = x'call @XM'entriesAt

-----------------------------------------

foreign import data XG'entries :: Type

type T'xm'entries use p =
  forall x' x m k v
   . X'Getter XG'entries m (Array (k /\ v))
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (Run x (Array (k /\ v)))

xm'entries :: forall p. T'xm'entries ((->) (Proxy p)) p
xm'entries = x'get @XG'entries

-----------------------------------------

foreign import data XM'lookup :: Type

type T'xm'lookup use p =
  forall x' x m k v
   . X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> Run x (Maybe v))

xm'lookup :: forall p. T'xm'lookup ((->) (Proxy p)) p
xm'lookup = x'call @XM'lookup

-----------------------------------------

type T'xm'has use p =
  forall x' x m k v
   . X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> Run x Boolean)

xm'has :: forall p. T'xm'has ((->) (Proxy p)) p
xm'has p k = xm'lookup p k <#> isJust

-----------------------------------------

foreign import data XG'd1'keys :: Type

type T'xm'd1'keys use p =
  forall x' x m k
   . X'Getter XG'd1'keys m (Array k)
  => XM'type m XMt'd1'key k
  => ConsSymbol p m x' x
  => use (Run x (Array k))

xm'd1'keys :: forall p. T'xm'd1'keys ((->) (Proxy p)) p
xm'd1'keys = x'get @XG'd1'keys

-----------------------------------------

foreign import data XG'd1'entries :: Type

type T'xm'd1'entries use p =
  forall x' x m k v
   . X'Getter XG'd1'entries m (Array (k /\ v))
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (Run x (Array (k /\ v)))

xm'd1'entries :: forall p. T'xm'd1'entries ((->) (Proxy p)) p
xm'd1'entries = x'get @XG'd1'entries

-----------------------------------------

foreign import data XG'clear :: Type

type T'xm'clear use p =
  forall x' x m
   . X'Getter XG'clear m Unit
  => ConsSymbol p m x' x
  => use (Run x Unit)

xm'clear :: forall p. T'xm'clear ((->) (Proxy p)) p
xm'clear = x'get @XG'clear

-----------------------------------------

foreign import data XM'clearAt :: Type

type T'xm'clearAt use p =
  forall x' x m k
   . X'Method XM'clearAt m k Unit
  => XM'type m XMt'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Unit)

xm'clearAt :: forall p. T'xm'clearAt ((->) (Proxy p)) p
xm'clearAt = x'call @XM'clearAt

-----------------------------------------

foreign import data XM'add :: Type

type T'xm'add use p =
  forall x' x m t
   . X'Method XM'add m t Unit
  => XM'type m XMt'self t
  => ConsSymbol p m x' x
  => use (t -> Run x Unit)

xm'add :: forall p. T'xm'add ((->) (Proxy p)) p
xm'add = x'call @XM'add

-----------------------------------------

foreign import data XM'addAt :: Type

type T'xm'addAt use p =
  forall x' x m k v
   . X'Method XM'addAt m (k /\ v) Unit
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

xm'addAt :: forall p. T'xm'addAt ((->) (Proxy p)) p
xm'addAt p = curry (x'call @XM'addAt p)

-----------------------------------------

foreign import data XM'remove :: Type

type T'xm'remove use p =
  forall x' x m k
   . X'Method XM'remove m k Unit
  => XM'type m XMt'key k
  => ConsSymbol p m x' x
  => use (k -> Run x Unit)

xm'remove :: forall p. T'xm'remove ((->) (Proxy p)) p
xm'remove = x'call @XM'remove

-----------------------------------------

foreign import data XM'insert :: Type

type T'xm'insert use p =
  forall x' x m k v
   . X'Method XM'insert m (k /\ v) Unit
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

xm'insert :: forall p. T'xm'insert ((->) (Proxy p)) p
xm'insert p = curry (x'call @XM'insert p)

-----------------------------------------

foreign import data XM'cons :: Type

type T'xm'cons use p =
  forall x' x m v
   . X'Method XM'cons m v Unit
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

xm'cons :: forall p. T'xm'cons ((->) (Proxy p)) p
xm'cons p = x'call @XM'cons p

-----------------------------------------

foreign import data XG'uncons :: Type

type T'xm'uncons use p =
  forall x' x m v
   . X'Getter XG'uncons m (Maybe v)
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (Run x (Maybe v))

xm'uncons :: forall p. T'xm'uncons ((->) (Proxy p)) p
xm'uncons p = x'get @XG'uncons p

-----------------------------------------

foreign import data XM'push :: Type

type T'xm'push use p =
  forall x' x m v
   . X'Method XM'push m v Unit
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

xm'push :: forall p. T'xm'push ((->) (Proxy p)) p
xm'push p = x'call @XM'push p

-----------------------------------------

foreign import data XG'pop :: Type

type T'xm'pop use p =
  forall x' x m v
   . X'Getter XG'pop m (Maybe v)
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (Run x (Maybe v))

xm'pop :: forall p. T'xm'pop ((->) (Proxy p)) p
xm'pop p = x'get @XG'pop p

-----------------------------------------

foreign import data XM'consAt :: Type

type T'xm'consAt use p =
  forall x' x m k v
   . X'Method XM'consAt m (k /\ v) Unit
  => XM'type m XMt'd1'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

xm'consAt :: forall p. T'xm'consAt ((->) (Proxy p)) p
xm'consAt p = curry (x'call @XM'consAt p)

-----------------------------------------

foreign import data XM'consLast :: Type

type T'xm'consLast use p =
  forall x' x m v
   . X'Method XM'consLast m v Unit
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

xm'consLast :: forall p. T'xm'consLast ((->) (Proxy p)) p
xm'consLast = x'call @XM'consLast

-----------------------------------------

foreign import data XM'd1'cons :: Type

type T'xm'd1'cons use p =
  forall x' x m v
   . X'Method XM'd1'cons m v Unit
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (v -> Run x Unit)

xm'd1'cons :: forall p. T'xm'd1'cons ((->) (Proxy p)) p
xm'd1'cons p = x'call @XM'd1'cons p

-----------------------------------------

type T'xm'replace use p =
  forall x' x m k v
   . X'Method XM'insert m (k /\ v) Unit
  => X'Method XM'lookup m k (Maybe v)
  => XM'type m XMt'key k
  => XM'type m XMt'value v
  => ConsSymbol p m x' x
  => use (k -> v -> Run x Unit)

xm'replace :: forall p. T'xm'replace ((->) (Proxy p)) p
xm'replace p k v = xm'has p k >>= flip when (xm'insert p k v)

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

xm'alter :: forall p. T'xm'alter ((->) (Proxy p)) p
xm'alter p k f = xm'lookup p k <#> f >>= may'__
  (\_ -> xm'remove p k)
  (\v -> xm'insert p k v)

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

xm'modify :: forall p. T'xm'modify ((->) (Proxy p)) p
xm'modify p k f = xm'lookup p k <#> map f >>= may'__
  (\_ -> xm'remove p k)
  (\v -> xm'insert p k v)

-----------------------------------------

foreign import data XM'extractAt :: Type

type T'xm'extractAt use p =
  forall x' x m k v
   . X'Method XM'extractAt m k v
  => XM'type m XMt'd1'key k
  => XM'type m XMt'd1'value v
  => ConsSymbol p m x' x
  => use (k -> Run x v)

xm'extractAt :: forall p. T'xm'extractAt ((->) (Proxy p)) p
xm'extractAt = x'call @XM'extractAt

-----------------------------------------

foreign import data XG'extract :: Type

class (X'Getter XG'extract m t, XM'type m XMt'self t) <= XC'extract m t

instance (X'Getter XG'extract m t, XM'type m XMt'self t) => XC'extract m t

type T'xm'extract use p =
  forall x' x m t
   . XC'extract m t
  => ConsSymbol p m x' x
  => use (Run x t)

xm'extract :: forall p. T'xm'extract ((->) (Proxy p)) p
xm'extract = x'get @XG'extract

infixl 6 xm'extract as @.

-------------------------------------------------------------------------

type T'xm'effCall p =
  forall m x' x nt r' r method param a
   . XC'extract m nt
  => Newtype nt { | r }
  => ConsSymbol p m x' x
  => ConsSymbol method (param -> Effect a) r' r
  => Proxy p
  -> Proxy method
  -> param
  -> Run x a

xm'effCall :: forall @p. T'xm'effCall p
xm'effCall px pm param =
  xm'extract px <#> unwrap <#> Rec.get pm <#> (#) param <#> unsafePerformEffect

-------------------------------------------------------------------------

type T'xm'effGet p =
  forall m x' x nt r' r method a
   . XC'extract m nt
  => Newtype nt { | r }
  => ConsSymbol p m x' x
  => ConsSymbol method (Effect a) r' r
  => Proxy p
  -> Proxy method
  -> Run x a

xm'effGet :: forall @p. T'xm'effGet p
xm'effGet px pm =
  xm'extract px <#> unwrap <#> Rec.get pm <#> unsafePerformEffect

-----------------------------------------

type T'xm'update use p =
  forall x' x m t
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use ((t -> t) -> Run x Unit)

xm'update :: forall p. T'xm'update ((->) (Proxy p)) p
xm'update p f = f <$> xm'extract p >>= xm'assign p

infixl 6 xm'update as @#

-----------------------------------------

type T'xm'view use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic (Forget a) t t a b -> Run x a)

xm'view :: forall p. T'xm'view ((->) (Proxy p)) p
xm'view p l = xm'extract p <#> view l

-----------------------------------------

type T'xm'view'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses (Forget a) t t a b
  => use (Run x a)

xm'view'b :: forall @sym p. T'xm'view'b ((->) (Proxy p)) sym p
xm'view'b p = xm'extract p <#> view (barlow @sym)

-----------------------------------------

type T'xm'preview use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic (Forget (First a)) t t a b -> Run x (Maybe a))

xm'preview :: forall p. T'xm'preview ((->) (Proxy p)) p
xm'preview p l = xm'extract p <#> preview l

-----------------------------------------

type T'xm'preview'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses (Forget (First a)) t t a b
  => use (Run x (Maybe a))

xm'preview'b :: forall @sym p. T'xm'preview'b ((->) (Proxy p)) sym p
xm'preview'b p = xm'extract p <#> preview (barlow @sym)

-----------------------------------------

type T'xm'toArrayOf use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic (Forget (Endo Function (List a))) t t a b -> Run x (Array a))

xm'toArrayOf :: forall p. T'xm'toArrayOf ((->) (Proxy p)) p
xm'toArrayOf p l = xm'extract p <#> toArrayOf l

-----------------------------------------

type T'xm'toArrayOf'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses (Forget (Endo Function (List a))) t t a b
  => use (Run x (Array a))

xm'toArrayOf'b :: forall @sym p. T'xm'toArrayOf'b ((->) (Proxy p)) sym p
xm'toArrayOf'b p = xm'extract p <#> toArrayOf (barlow @sym)

-----------------------------------------

type T'xm'set use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic Function t t a b -> b -> Run x Unit)

xm'set :: forall p. T'xm'set ((->) (Proxy p)) p
xm'set p l v = xm'update p (set l v)

-----------------------------------------

type T'xm'set'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses Function t t a b
  => use (b -> Run x Unit)

xm'set'b :: forall @sym p. T'xm'set'b ((->) (Proxy p)) sym p
xm'set'b p v = xm'update p (set (barlow @sym) v)

-----------------------------------------

type T'xm'setPure use p =
  forall x' x m t a b app
   . XC'extract m t
  => XC'assign m t
  => Applicative app
  => ConsSymbol p m x' x
  => use (Optic Function t t a (app b) -> b -> Run x Unit)

xm'setPure :: forall p. T'xm'setPure ((->) (Proxy p)) p
xm'setPure p l v = xm'update p (set l (pure v))

-----------------------------------------

type T'xm'setPure'b use sym p =
  forall lenses x' x m t a b app
   . XC'extract m t
  => XC'assign m t
  => Applicative app
  => ConsSymbol p m x' x
  => C'Barlow sym lenses Function t t a (app b)
  => use (b -> Run x Unit)

xm'setPure'b :: forall @sym p. T'xm'setPure'b ((->) (Proxy p)) sym p
xm'setPure'b p v = xm'update p (set (barlow @sym) (pure v))

-----------------------------------------

type T'xm'over use p =
  forall x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => use (Optic Function t t a b -> (a -> b) -> Run x Unit)

xm'over :: forall p. T'xm'over ((->) (Proxy p)) p
xm'over p l v = xm'update p (over l v)

-----------------------------------------

type T'xm'over'b use sym p =
  forall lenses x' x m t a b
   . XC'extract m t
  => XC'assign m t
  => ConsSymbol p m x' x
  => C'Barlow sym lenses Function t t a b
  => use ((a -> b) -> Run x Unit)

xm'over'b :: forall @sym p. T'xm'over'b ((->) (Proxy p)) sym p
xm'over'b p v = xm'update p (over (barlow @sym) v)