module Z.Z.X6.Methods
  ( x'add
  , x'assign
  , x'clear
  , x'cons
  , x'entries
  , x'extract
  , x'insert
  , x'keys
  , x'lookup
  , x'pop
  , x'push
  , x'remove
  , x'replace
  , x'reset
  , x'result
  , x'size
  , x'uncons
  , x'vals
  ) where

import Z.Z.X6.UtilPrelude

import Z.Z.X6.Core
  ( class X'RespondsTo
  , class X'Results
  , x'respondTo
  , x'respondTo_
  , x'results'impl
  )
import Z.Z.X6.Responds (Responds, Responds'Const)

type T'self :: forall k. k -> Row k -> Row k
type T'self t rest = (self :: t | rest)

type T'element :: forall k. k -> Row k -> Row k
type T'element t rest = (element :: t | rest)

type T'key :: forall k. k -> Row k -> Row k
type T'key t rest = (key :: t | rest)

---------------------------------------------------------------------

type T'extract t rest = (extract :: Responds'Const t | rest)

x'extract
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract t resp'rest) (T'self t t'rest)
  => Run x t
x'extract = x'respondTo_ @p @"extract"

---------------------------------------------------------------------

x'result
  :: forall @p m x' x result
   . ConsSymbol p m x' x
  => X'Results m result
  => Run x result
x'result = x'results'impl @m $ Proxy @p

---------------------------------------------------------------------

type T'size rest = (size :: Responds'Const Int | rest)

x'size
  :: forall @p m x' x resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'size resp'rest) t'rest
  => Run x Int
x'size = x'respondTo_ @p @"size"

---------------------------------------------------------------------

type T'vals t rest = (vals :: Responds'Const (Array t) | rest)

x'vals
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'vals t resp'rest) (T'element t t'rest)
  => Run x (Array t)
x'vals = x'respondTo_ @p @"vals"

---------------------------------------------------------------------

type T'keys k rest = (keys :: Responds'Const (Array k) | rest)

x'keys
  :: forall @p m x' x k resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'keys k resp'rest) (T'key k t'rest)
  => Run x (Array k)
x'keys = x'respondTo_ @p @"keys"

---------------------------------------------------------------------

type T'entries k v rest = (entries :: Responds'Const (Array $ k /\ v) | rest)

x'entries
  :: forall @p m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'entries k v resp'rest)
       (T'key k $ T'element v t'rest)
  => Run x (Array $ k /\ v)
x'entries = x'respondTo_ @p @"entries"

---------------------------------------------------------------------

type T'lookup k v rest = (lookup :: Responds k (Maybe v) | rest)

x'lookup
  :: forall @p m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'lookup k v resp'rest)
       (T'key k $ T'element v t'rest)
  => k
  -> Run x (Maybe v)
x'lookup = x'respondTo @p @"lookup"

---------------------------------------------------------------------

type T'assign t rest = (assign :: Responds t Unit | rest)

x'assign
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'assign t resp'rest) (T'self t t'rest)
  => t
  -> Run x Unit
x'assign = x'respondTo @p @"assign"

---------------------------------------------------------------------

type T'reset rest = (reset :: Responds'Const Unit | rest)

x'reset
  :: forall @p m x' x resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'reset resp'rest) t'rest
  => Run x Unit
x'reset = x'respondTo_ @p @"reset"

---------------------------------------------------------------------

type T'clear rest = (clear :: Responds'Const Unit | rest)

x'clear
  :: forall @p m x' x resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'clear resp'rest) t'rest
  => Run x Unit
x'clear = x'respondTo_ @p @"clear"

---------------------------------------------------------------------

type T'add t rest = (add :: Responds t Unit | rest)

x'add
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'add t resp'rest) (T'self t t'rest)
  => t
  -> Run x Unit
x'add = x'respondTo @p @"add"

---------------------------------------------------------------------

type T'cons t rest = (cons :: Responds t Unit | rest)

x'cons
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'cons t resp'rest) (T'element t t'rest)
  => t
  -> Run x Unit
x'cons = x'respondTo @p @"cons"

---------------------------------------------------------------------

type T'uncons t rest = (uncons :: Responds'Const (Maybe t) | rest)

x'uncons
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'uncons t resp'rest) (T'element t t'rest)
  => Run x (Maybe t)
x'uncons = x'respondTo_ @p @"uncons"

---------------------------------------------------------------------

type T'push t rest = (push :: Responds t Unit | rest)

x'push
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'push t resp'rest) (T'element t t'rest)
  => t
  -> Run x Unit
x'push = x'respondTo @p @"push"

---------------------------------------------------------------------

type T'pop t rest = (pop :: Responds'Const (Maybe t) | rest)

x'pop
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'pop t resp'rest) (T'element t t'rest)
  => Run x (Maybe t)
x'pop = x'respondTo_ @p @"pop"

---------------------------------------------------------------------

type T'remove t rest = (remove :: Responds t Unit | rest)

x'remove
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'remove t resp'rest) (T'key t t'rest)
  => t
  -> Run x Unit
x'remove = x'respondTo @p @"remove"

---------------------------------------------------------------------

type T'insert k v rest = (insert :: Responds (k /\ v) Unit | rest)

x'insert
  :: forall @p m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'insert k v resp'rest)
       (T'key k $ T'element v t'rest)
  => k
  -> v
  -> Run x Unit
x'insert k v = x'respondTo @p @"insert" (k /\ v)

---------------------------------------------------------------------

type T'replace k v rest = (replace :: Responds (k /\ v) Unit | rest)

x'replace
  :: forall @p m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'replace k v resp'rest)
       (T'key k $ T'element v t'rest)
  => k
  -> v
  -> Run x Unit
x'replace k v = x'respondTo @p @"replace" (k /\ v)
