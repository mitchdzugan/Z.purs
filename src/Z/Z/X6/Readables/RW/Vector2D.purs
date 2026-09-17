module Z.Z.X6.Readables.RW.Vector2D where

import Z.Z.X6.UtilPrelude

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
  , eff'bin2D'addAt
  , eff'bin2D'clear
  , eff'bin2D'insert
  , eff'bin2D'lookup
  , eff'bin2D'new
  )
import Z.Z.Eff.Ref (Eff'Ref, eff'ref'get, eff'ref'new, eff'ref'set)
import Z.Z.X6.Core
  ( class X'R'RespondsTo'rw
  , class X'Readable'rw
  , class X'Results'R'rw
  )
import Z.Z.X6.Responds
  ( Responds
  , Responds'Const
  , responds'const'eff
  , responds'run'eff
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

instance X'Readable'rw (R'Vector2D a) (Array2D a) where
  x'readable'rw'mk = map R'Vector2D <<< eff'vec2D'init

instance X'Results'R'rw (R'Vector2D t) (Array2D t) where
  x'results'r'rw'impl (R'Vector2D st) = eff'vec2D'freeze st

instance
  X'R'RespondsTo'rw (R'Vector2D a)
    ( VariantF
        ( extract :: Responds'Const (Array2D a)
        , size :: Responds'Const Int
        , sizeAt :: Responds Int Int
        , vals :: Responds'Const (Array a)
        , valsAt :: Responds Int (Array a)
        --, keys :: Responds'Const (Array Int)
        --, entries :: Responds'Const (Array $ Int /\ a)
        --, lookup :: Responds Int (Maybe a)
        )
    )
    ( VariantF
        ( assign :: Responds (Array2D a) Unit
        --, reset :: Responds'Const Unit
        --, clear :: Responds'Const Unit
        --, add :: Responds (Array a) Unit
        --, cons :: Responds a Unit
        --, uncons :: Responds'Const (Maybe a)
        --, push :: Responds a Unit
        --, pop :: Responds'Const (Maybe a)
        --, replace :: Responds (Int /\ a) Unit
        )
    )
    ( self :: (Array2D a)
    , element :: a
    , key :: Int /\ Int
    , d1key :: Int
    , d2key :: Int
    , d1element :: Array a
    ) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r (R'Vector2D st) = match
    { extract: responds'const'eff $ eff'vec2D'freeze st
    , size: responds'const'eff $ eff'ref'get st.r'length
    , sizeAt: responds'run'eff \ind ->
        eff'bin'lookup ind st.b'lengths <#> fromMaybe 0
    , vals: responds'const'eff $ eff'vec2D'freeze st <#> arr'concat
    , valsAt: responds'run'eff \ind -> eff'vec2D'freezeAt ind st
    --, keys: responds'const'eff $ eff'vec'keys st
    --, entries: responds'const'eff $ eff'vec'freeze st <#> arr'withInd
    --, lookup: responds'run'eff \ind -> eff'vec'lookup ind st
    }
  x'r'mkResponds'w (R'Vector2D st) = match
    { assign: responds'run'eff \a -> eff'vec2D'set a st
    --, reset: responds'const'eff $ eff'vec'set st.init st
    --, clear: responds'const'eff $ eff'vec'clear st
    --, add: responds'run'eff \a -> eff'vec'add a st
    --, cons: responds'run'eff \el -> eff'vec'cons el st
    --, uncons: responds'const'eff $ eff'vec'uncons st
    --, push: responds'run'eff \el -> eff'vec'push el st
    --, pop: responds'const'eff $ eff'vec'pop st
    --, replace: responds'run'eff \(ind /\ el) -> eff'bin'lookup ind st.b'els >>=
    --    \curr -> whenJust curr $ const $ eff'bin'insert ind { ind, el } st.b'els
    }

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
  o'start <- eff'ref'get st.r'start
  o'length <- eff'ref'get st.r'length
  if (ind1 < o'start || ind1 >= o'start + o'length) then pure unit
  else do
    start <- eff'bin'lookup ind1 st.b'starts <#> fromMaybe 0
    length <- eff'bin'lookup ind1 st.b'lengths <#> fromMaybe 0
    eff'bin'insert ind1 (length + 1) st.b'lengths
    let ind2 = start + length
    eff'bin2D'insert ind1 ind2 { ind: ind2, el } st.b'els
    pure unit

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
