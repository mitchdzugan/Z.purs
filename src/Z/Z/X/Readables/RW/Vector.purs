module Z.Z.X.Readables.RW.Vector
  ( Eff'Vector
  , R'Vector(..)
  , R'Writer(..)
  , X'Vector
  , X'Vector'w
  , X'Writer
  , x'vector
  , x'vector'w
  , x'vector_
  , x'vector_'w
  , x'writer
  ) where

import Z.Z.X.UtilPrelude

import Data.Foldable (for_)
import Z.Z.Core (arr'fold, arr'range, arr'withInd, mapM)
import Z.Z.Defaultable (whenJust)
import Z.Z.Eff.Bin
  ( Eff'Bin
  , eff'bin'clear
  , eff'bin'delete
  , eff'bin'insert
  , eff'bin'lookup
  , eff'bin'new
  , eff'bin'size
  )
import Z.Z.Eff.Ref (Eff'Ref, eff'ref'get, eff'ref'new, eff'ref'set)
import Z.Z.X.Core
  ( class RW'Tag'CanRead
  , class RW'Tag'CanWrite
  , class X'Readable'rw
  , class X'Results'R'rw
  , RW'Tagged
  , W'Tagged
  , X'Evaluable
  , x'evaluable
  , x'evaluable_
  )
import Z.Z.X.Method
  ( class Eff'RW'Getter
  , class Eff'RW'Method
  , class XM'type
  , XG'clear
  , XG'entries
  , XG'extract
  , XG'keys
  , XG'pop
  , XG'reset
  , XG'size
  , XG'uncons
  , XG'vals
  , XM'add
  , XM'assign
  , XM'cons
  , XM'insert
  , XM'lookup
  , XM'push
  , XMt'key
  , XMt'self
  , XMt'value
  )

---------------------------------------------------------------------

type Eff'Vector a =
  { b'els :: Eff'Bin { ind :: Int, el :: a }
  , init :: Array a
  , r'start :: Eff'Ref Int
  , r'length :: Eff'Ref Int
  }

---------------------------------------------------------------------

newtype R'Vector a = R'Vector (Eff'Vector a)

instance XM'type (R'Vector a) XMt'self (Array a)

instance XM'type (R'Vector a) XMt'key Int

instance XM'type (R'Vector a) XMt'value a

----------------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'extract (R'Vector a) rwTag (Array a) where
  eff'rw'get (R'Vector st) = eff'vec'freeze st

----------------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'size (R'Vector a) rwTag Int where
  eff'rw'get (R'Vector st) = eff'bin'size st.b'els

----------------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'vals (R'Vector a) rwTag (Array a) where
  eff'rw'get (R'Vector st) = eff'vec'freeze st

----------------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'keys (R'Vector a) rwTag (Array Int) where
  eff'rw'get (R'Vector st) = eff'vec'keys st

----------------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'entries (R'Vector a) rwTag (Array (Int /\ a)) where
  eff'rw'get (R'Vector st) = eff'vec'freeze st <#> arr'withInd

----------------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Method XM'lookup (R'Vector a) rwTag Int (Maybe a) where
  eff'rw'call (R'Vector st) ind = eff'vec'lookup ind st

-------------------------------------------

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'assign (R'Vector a) rwTag (Array a) Unit where
  eff'rw'call (R'Vector st) a = eff'vec'set a st

----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'reset (R'Vector a) rwTag Unit where
  eff'rw'get (R'Vector st) = eff'vec'set st.init st

----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'clear (R'Vector a) rwTag Unit where
  eff'rw'get (R'Vector st) = eff'vec'clear st

----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'add (R'Vector a) rwTag (Array a) Unit where
  eff'rw'call (R'Vector st) a = eff'vec'add a st

----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'cons (R'Vector a) rwTag a Unit where
  eff'rw'call (R'Vector st) el = eff'vec'cons el st

----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'uncons (R'Vector a) rwTag (Maybe a) where
  eff'rw'get (R'Vector st) = eff'vec'uncons st

----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'push (R'Vector a) rwTag a Unit where
  eff'rw'call (R'Vector st) el = eff'vec'push el st

----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'pop (R'Vector a) rwTag (Maybe a) where
  eff'rw'get (R'Vector st) = eff'vec'pop st

----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'insert (R'Vector a) rwTag (Int /\ a) Unit where
  eff'rw'call (R'Vector st) (ind /\ el) = eff'bin'lookup ind st.b'els >>=
    \curr -> whenJust curr $ const $ eff'bin'insert ind { ind, el } st.b'els

---------------------------------------------------------------------

instance X'Readable'rw (R'Vector a) (Array a) where
  x'readable'rw'mk = map R'Vector <<< eff'vec'init

instance X'Results'R'rw (R'Vector t) (Array t) where
  x'results'r'rw'impl (R'Vector st) = eff'vec'freeze st

---------------------------------------------------------------------

type X'Vector a = Reader (RW'Tagged (R'Vector a))
type X'Vector'w a = Reader (W'Tagged (R'Vector a))

x'vector :: forall @a. Array a -> X'Evaluable (X'Vector a)
x'vector t = x'evaluable t

x'vector'w :: forall @a. Array a -> X'Evaluable (X'Vector'w a)
x'vector'w t = x'evaluable t

x'vector_ :: forall @a. X'Evaluable (X'Vector a)
x'vector_ = x'evaluable_

x'vector_'w :: forall @a. X'Evaluable (X'Vector'w a)
x'vector_'w = x'evaluable_

---------------------------------------------------------------------

newtype R'Writer monoid = R'Writer (Eff'Vector monoid)

instance Monoid monoid => X'Readable'rw (R'Writer monoid) Unit where
  x'readable'rw'mk _ = R'Writer <$> eff'vec'init []

instance Monoid monoid => X'Results'R'rw (R'Writer monoid) monoid where
  x'results'r'rw'impl (R'Writer st) = eff'vec'freeze st <#> arr'fold

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'cons (R'Writer a) rwTag a Unit where
  eff'rw'call (R'Writer st) el = eff'vec'cons el st

instance XM'type (R'Writer a) XMt'self (Array a)

instance XM'type (R'Writer a) XMt'key Int

instance XM'type (R'Writer a) XMt'value a

---------------------------------------------------------------------

type X'Writer a = Reader (RW'Tagged (R'Writer a))

x'writer :: forall @monoid. Monoid monoid => X'Evaluable (X'Writer monoid)
x'writer = x'evaluable_

---------------------------------------------------------------------

eff'vec'init :: forall a. Array a -> Effect (Eff'Vector a)
eff'vec'init arr'initial = do
  b'els <- eff'bin'new
  r'start <- eff'ref'new 0
  r'length <- eff'ref'new 0
  let st = { b'els, r'start, r'length, init: arr'initial }
  eff'vec'set arr'initial st
  pure st

eff'vec'clear :: forall a. Eff'Vector a -> Effect Unit
eff'vec'clear { b'els, r'start, r'length } = do
  eff'bin'clear b'els
  eff'ref'set 0 r'start
  eff'ref'set 0 r'length

eff'vec'keys :: forall a. Eff'Vector a -> Effect (Array Int)
eff'vec'keys { r'length } = eff'ref'get r'length <#> arr'range 0

eff'vec'lookup :: forall a. Int -> Eff'Vector a -> Effect (Maybe a)
eff'vec'lookup ind { b'els, r'start } = do
  start <- eff'ref'get r'start
  eff'bin'lookup (start + ind) b'els <#> map _.el

eff'vec'cons :: forall a. a -> Eff'Vector a -> Effect Unit
eff'vec'cons el { b'els, r'start, r'length } = do
  start <- eff'ref'get r'start
  length <- eff'ref'get r'length
  eff'ref'set (length + 1) r'length
  let ind = start + length
  eff'bin'insert ind { ind, el } b'els

eff'vec'uncons :: forall a. Eff'Vector a -> Effect (Maybe a)
eff'vec'uncons st@{ b'els, r'length } = do
  length <- eff'ref'get r'length
  if (length <= 0) then pure Nothing
  else do
    res <- eff'vec'lookup (length - 1) st
    eff'ref'set (length - 1) r'length
    eff'bin'delete (length - 1) b'els
    pure res

eff'vec'push :: forall a. a -> Eff'Vector a -> Effect Unit
eff'vec'push el { b'els, r'start, r'length } = do
  start <- eff'ref'get r'start
  length <- eff'ref'get r'length
  let ind = start - 1
  eff'ref'set ind r'start
  eff'ref'set (length + 1) r'length
  eff'bin'insert ind { ind, el } b'els

eff'vec'pop :: forall a. Eff'Vector a -> Effect (Maybe a)
eff'vec'pop st@{ b'els, r'start, r'length } = do
  start <- eff'ref'get r'start
  length <- eff'ref'get r'length
  if (length <= 0) then pure Nothing
  else do
    res <- eff'vec'lookup start st
    eff'ref'set (start + 1) r'length
    eff'bin'delete (length - 1) b'els
    pure res

eff'vec'add :: forall a. Array a -> Eff'Vector a -> Effect Unit
eff'vec'add a st = for_ a (flip eff'vec'cons st)

eff'vec'set :: forall a. Array a -> Eff'Vector a -> Effect Unit
eff'vec'set a st = eff'vec'clear st *> eff'vec'add a st

eff'vec'freeze :: forall a. Eff'Vector a -> Effect (Array a)
eff'vec'freeze st = eff'vec'keys st >>= mapM \ind -> eff'vec'lookup ind st >>=
  case _ of
    Just v -> pure v
    Nothing -> throw "vector invariant violated"