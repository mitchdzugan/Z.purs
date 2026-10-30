module Z.Z.X.Readables.RW.Vector2D
  ( Array2D
  , Eff'Vector2D
  , R'Vector2D(..)
  , X'Vector2D
  , X'Vector2D'w
  , x'vector2D
  , x'vector2D'w
  , x'vector2D_
  , x'vector2D_'w
  ) where

import Z.Z.X.UtilPrelude

import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Z.Z.Core (arr'concat, arr'range, mapM)
import Z.Z.Eff.Bin
  ( Eff'Bin
  , eff'bin'clear
  , eff'bin'insert
  , eff'bin'lookup
  , eff'bin'new
  )
import Z.Z.Eff.Bin2D
  ( Eff'Bin2D
  , eff'bin2D'clear
  , eff'bin2D'insert
  , eff'bin2D'lookup
  , eff'bin2D'new
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
  , XG'extract
  , XG'reset
  , XG'size
  , XG'vals
  , XM'add
  , XM'assign
  , XM'consAt
  , XM'consLast
  , XM'd1'cons
  , XM'sizeAt
  , XM'valsAt
  , XMt'd1'key
  , XMt'd1'value
  , XMt'd2'key
  , XMt'key
  , XMt'self
  , XMt'value
  )

---------------------------------------------------------------------

type Eff'Vector2D a =
  { b'els :: Eff'Bin2D Int { ind :: Int, el :: a }
  , init :: Array (Array a)
  , r'start :: Eff'Ref Int
  , r'length :: Eff'Ref Int
  , b'starts :: Eff'Bin Int
  , b'lengths :: Eff'Bin Int
  }

type Array2D a = Array (Array a)

---------------------------------------------------------------------

newtype R'Vector2D a = R'Vector2D (Eff'Vector2D a)

instance XM'type (R'Vector2D a) XMt'self (Array2D a)

instance XM'type (R'Vector2D a) XMt'key (Int /\ Int)

instance XM'type (R'Vector2D a) XMt'value a

instance XM'type (R'Vector2D a) XMt'd1'key Int

instance XM'type (R'Vector2D a) XMt'd1'value (Array a)

instance XM'type (R'Vector2D a) XMt'd2'key Int

---------------------------------------------------------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'extract (R'Vector2D a) rwTag (Array2D a) where
  eff'rw'get (R'Vector2D st) = eff'vec2D'freeze st

--------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'size (R'Vector2D a) rwTag Int where
  eff'rw'get (R'Vector2D st) = eff'ref'get st.r'length

--------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Method XM'sizeAt (R'Vector2D a) rwTag Int Int where
  eff'rw'call (R'Vector2D st) ind =
    eff'bin'lookup ind st.b'lengths <#> fromMaybe 0

--------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'vals (R'Vector2D a) rwTag (Array a) where
  eff'rw'get (R'Vector2D st) = eff'vec2D'freeze st <#> arr'concat

--------------------

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Method XM'valsAt (R'Vector2D a) rwTag Int (Array a) where
  eff'rw'call (R'Vector2D st) ind = eff'vec2D'freezeAt ind st

-----------------------------------

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'assign (R'Vector2D a) rwTag (Array2D a) Unit where
  eff'rw'call (R'Vector2D st) a2d = eff'vec2D'set a2d st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'reset (R'Vector2D a) rwTag Unit where
  eff'rw'get (R'Vector2D st) = eff'vec2D'set st.init st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'clear (R'Vector2D a) rwTag Unit where
  eff'rw'get (R'Vector2D st) = eff'vec2D'clear st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'add (R'Vector2D a) rwTag (Array2D a) Unit where
  eff'rw'call (R'Vector2D st) a2d = eff'vec2D'add a2d st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'd1'cons (R'Vector2D a) rwTag (Array a) Unit where
  eff'rw'call (R'Vector2D st) a = eff'vec2D'd1cons a st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'consAt (R'Vector2D a) rwTag (Int /\ a) Unit where
  eff'rw'call (R'Vector2D st) (ind /\ el) = eff'vec2D'consAt ind el st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'consLast (R'Vector2D a) rwTag a Unit where
  eff'rw'call (R'Vector2D st) el = eff'vec2D'consLast el st

---------------------------------------------------------------------

instance X'Readable'rw (R'Vector2D a) (Array2D a) where
  x'readable'rw'mk = map R'Vector2D <<< eff'vec2D'init

instance X'Results'R'rw (R'Vector2D t) (Array2D t) where
  x'results'r'rw'impl (R'Vector2D st) = eff'vec2D'freeze st

---------------------------------------------------------------------

type X'Vector2D a = Reader (RW'Tagged (R'Vector2D a))
type X'Vector2D'w a = Reader (W'Tagged (R'Vector2D a))

x'vector2D :: forall @a. Array2D a -> X'Evaluable (X'Vector2D a)
x'vector2D t = x'evaluable t

x'vector2D'w :: forall @a. Array2D a -> X'Evaluable (X'Vector2D'w a)
x'vector2D'w t = x'evaluable t

x'vector2D_ :: forall @a. X'Evaluable (X'Vector2D a)
x'vector2D_ = x'evaluable_

x'vector2D_'w :: forall @a. X'Evaluable (X'Vector2D'w a)
x'vector2D_'w = x'evaluable_

---------------------------------------------------------------------

eff'vec2D'clear :: forall a. Eff'Vector2D a -> Effect Unit
eff'vec2D'clear st = do
  eff'bin2D'clear st.b'els
  eff'bin'clear st.b'starts
  eff'bin'clear st.b'lengths
  eff'ref'set 0 st.r'start
  eff'ref'set 0 st.r'length

eff'vec2D'add :: forall a. Array (Array a) -> Eff'Vector2D a -> Effect Unit
eff'vec2D'add nexts st = for_ nexts \next -> eff'vec2D'd1cons next st

eff'vec2D'set :: forall a. Array (Array a) -> Eff'Vector2D a -> Effect Unit
eff'vec2D'set nexts st = eff'vec2D'clear st *> eff'vec2D'add nexts st

eff'vec2D'init :: forall a. Array (Array a) -> Effect (Eff'Vector2D a)
eff'vec2D'init init = do
  b'els <- eff'bin2D'new
  r'start <- eff'ref'new 0
  r'length <- eff'ref'new 0
  b'starts <- eff'bin'new
  b'lengths <- eff'bin'new
  let st = { b'els, r'start, r'length, b'starts, b'lengths, init }
  eff'vec2D'set init st
  pure st

eff'vec2D'd1cons :: forall a. Array a -> Eff'Vector2D a -> Effect Unit
eff'vec2D'd1cons next st = do
  start <- eff'ref'get st.r'start
  length <- eff'ref'get st.r'length
  eff'ref'set (length + 1) st.r'length
  let ind1 = start + length
  for_ next \el -> eff'vec2D'consAt ind1 el st
  pure unit

eff'vec2D'consAt :: forall a. Int -> a -> Eff'Vector2D a -> Effect Unit
eff'vec2D'consAt ind1 el st = do
  o'length <- eff'ref'get st.r'length
  if (ind1 < 0 || ind1 >= o'length) then pure unit
  else do
    start <- eff'bin'lookup ind1 st.b'starts <#> fromMaybe 0
    length <- eff'bin'lookup ind1 st.b'lengths <#> fromMaybe 0
    eff'bin'insert ind1 (length + 1) st.b'lengths
    let ind2 = start + length
    eff'bin2D'insert ind1 ind2 { ind: ind2, el } st.b'els
    pure unit

eff'vec2D'consLast :: forall a. a -> Eff'Vector2D a -> Effect Unit
eff'vec2D'consLast el st = do
  o'length <- eff'ref'get st.r'length
  eff'vec2D'consAt (o'length - 1) el st

eff'vec2D'keys :: forall a. Eff'Vector2D a -> Effect (Array Int)
eff'vec2D'keys { r'length } = eff'ref'get r'length <#> arr'range 0

eff'vec2D'keysAt :: forall a. Int -> Eff'Vector2D a -> Effect (Array Int)
eff'vec2D'keysAt ind1 { b'lengths } =
  eff'bin'lookup ind1 b'lengths <#> fromMaybe 0 <#> arr'range 0

eff'vec2D'lookup :: forall a. Int -> Int -> Eff'Vector2D a -> Effect (Maybe a)
eff'vec2D'lookup ind1 ind2 { b'els, r'start, b'starts } = do
  o'start <- eff'ref'get r'start
  let key1 = o'start + ind1
  start <- eff'bin'lookup key1 b'starts <#> fromMaybe 0
  let key2 = start + ind2
  eff'bin2D'lookup key1 key2 b'els <#> map _.el

eff'vec2D'freezeAt :: forall a. Int -> Eff'Vector2D a -> Effect (Array a)
eff'vec2D'freezeAt ind1 st = eff'vec2D'keysAt ind1 st >>= mapM \ind2 ->
  eff'vec2D'lookup ind1 ind2 st >>= case _ of
    Just v -> pure v
    Nothing -> throw "vector2d inner invariant violated"

eff'vec2D'freeze :: forall a. Eff'Vector2D a -> Effect (Array (Array a))
eff'vec2D'freeze st = eff'vec2D'keys st >>= mapM (flip eff'vec2D'freezeAt st)
