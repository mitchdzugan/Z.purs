module Z.Z.X6.Readables.RW.Vector where

import Z.Z.X6.UtilPrelude

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
import Z.Z.X6.Core
  ( class X'R'RespondsTo'rw
  , class X'Readable'rw
  , class X'Results'R'rw
  , RW'Tagged
  , W'Tagged
  , X'Evaluable
  , x'evaluable
  , x'evaluable_
  )
import Z.Z.X6.Responds
  ( Responds
  , Responds'Const
  , responds'const'eff
  , responds'run'eff
  )

type Eff'Vector a =
  { b'els :: Eff'Bin { ind :: Int, el :: a }
  , init :: Array a
  , r'start :: Eff'Ref Int
  , r'length :: Eff'Ref Int
  }

---------------------------------------------------------------------

newtype R'Vector a = R'Vector (Eff'Vector a)

instance X'Readable'rw (R'Vector a) (Array a) where
  x'readable'rw'mk = map R'Vector <<< eff'vec'init

instance X'Results'R'rw (R'Vector t) (Array t) where
  x'results'r'rw'impl (R'Vector st) = eff'vec'freeze st

instance
  X'R'RespondsTo'rw (R'Vector a)
    ( VariantF
        ( extract :: Responds'Const (Array a)
        , size :: Responds'Const Int
        , vals :: Responds'Const (Array a)
        , keys :: Responds'Const (Array Int)
        , entries :: Responds'Const (Array $ Int /\ a)
        , lookup :: Responds Int (Maybe a)
        )
    )
    ( VariantF
        ( assign :: Responds (Array a) Unit
        , reset :: Responds'Const Unit
        , clear :: Responds'Const Unit
        , add :: Responds (Array a) Unit
        , cons :: Responds a Unit
        , uncons :: Responds'Const (Maybe a)
        , push :: Responds a Unit
        , pop :: Responds'Const (Maybe a)
        , replace :: Responds (Int /\ a) Unit
        )
    )
    ( self :: (Array a)
    , element :: a
    , key :: Int
    ) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r (R'Vector st) = match
    { extract: responds'const'eff $ eff'vec'freeze st
    , size: responds'const'eff $ eff'bin'size st.b'els
    , vals: responds'const'eff $ eff'vec'freeze st
    , keys: responds'const'eff $ eff'vec'keys st
    , entries: responds'const'eff $ eff'vec'freeze st <#> arr'withInd
    , lookup: responds'run'eff \ind -> eff'vec'lookup ind st
    }
  x'r'mkResponds'w (R'Vector st) = match
    { assign: responds'run'eff \a -> eff'vec'set a st
    , reset: responds'const'eff $ eff'vec'set st.init st
    , clear: responds'const'eff $ eff'vec'clear st
    , add: responds'run'eff \a -> eff'vec'add a st
    , cons: responds'run'eff \el -> eff'vec'cons el st
    , uncons: responds'const'eff $ eff'vec'uncons st
    , push: responds'run'eff \el -> eff'vec'push el st
    , pop: responds'const'eff $ eff'vec'pop st
    , replace: responds'run'eff \(ind /\ el) -> eff'bin'lookup ind st.b'els >>=
        \curr -> whenJust curr $ const $ eff'bin'insert ind { ind, el } st.b'els
    }

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
  Monoid monoid =>
  X'R'RespondsTo'rw (R'Writer monoid)
    (VariantF ())
    (VariantF (cons :: Responds monoid Unit))
    (element :: monoid) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r _ = match {}
  x'r'mkResponds'w (R'Writer st) = match
    { cons: responds'run'eff \el -> eff'vec'cons el st }

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
    Nothing -> throw ""