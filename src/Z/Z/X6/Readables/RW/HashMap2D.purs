module Z.Z.X6.Readables.RW.HashMap2D where

import Z.Z.X6.UtilPrelude

import Data.Foldable (for_)
import Data.Traversable (for)
import Z.Z.Core (arr'concat)
import Z.Z.Defaultable (whenJust)
import Z.Z.Eff.Bin2D
  ( Eff'Bin2D
  , eff'bin2D'addAt
  , eff'bin2D'clear
  , eff'bin2D'clearAt
  , eff'bin2D'd1keys
  , eff'bin2D'delete
  , eff'bin2D'freezeAt
  , eff'bin2D'insert
  , eff'bin2D'lookup
  , eff'bin2D'new
  , eff'bin2D'size
  , eff'bin2D'sizeAt
  , eff'bin2D'valsAt
  )
import Z.Z.HashMap (HashMap, hm'entries, hm'fromFoldable, hm'lookup, hm'vals)
import Z.Z.Id (class Identable)
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

type HashMap2D k1 k2 v = HashMap k1 (HashMap k2 v)
type Eff'HashMap2D k1 k2 v = Eff'Bin2D k1 { k :: k2, v :: v }

newtype R'HashMap2D k1 k2 v =
  R'HashMap2D (Eff'HashMap2D k1 k2 v /\ HashMap2D k1 k2 v)

---------------------------------------------------------------------

instance
  ( Identable k1
  , Identable k2
  ) =>
  X'Readable'rw (R'HashMap2D k1 k2 v) (HashMap2D k1 k2 v) where
  x'readable'rw'mk hs'initial = do
    st <- eff'bin2D'new
    eff'hm2D'set hs'initial st
    pure $ R'HashMap2D $ st /\ hs'initial

instance
  ( Identable k1
  , Identable k2
  ) =>
  X'Results'R'rw (R'HashMap2D k1 k2 v) (HashMap2D k1 k2 v) where
  x'results'r'rw'impl (R'HashMap2D (st /\ _)) = eff'hm2D'freeze st

instance
  ( Identable k1
  , Identable k2
  ) =>
  X'R'RespondsTo'rw (R'HashMap2D k1 k2 v)
    ( VariantF
        ( extract :: Responds'Const (HashMap2D k1 k2 v)
        , extractAt :: Responds k1 (HashMap k2 v)
        , size :: Responds'Const Int
        , sizeAt :: Responds k1 Int
        , vals :: Responds'Const (Array v)
        , valsAt :: Responds k1 (Array v)
        , entries :: Responds'Const (Array $ (k1 /\ k2) /\ v)
        , entriesAt :: Responds k1 (Array $ k2 /\ v)
        , keys :: Responds'Const (Array $ k1 /\ k2)
        , keysAt :: Responds k1 (Array k2)
        , lookup :: Responds (k1 /\ k2) (Maybe v)
        , d1keys :: Responds'Const (Array k1)
        )
    )
    ( VariantF
        ( assign :: Responds (HashMap2D k1 k2 v) Unit
        , assignAt :: Responds (k1 /\ HashMap k2 v) Unit
        , reset :: Responds'Const Unit
        , resetAt :: Responds k1 Unit
        , clear :: Responds'Const Unit
        , clearAt :: Responds k1 Unit
        , add :: Responds (HashMap2D k1 k2 v) Unit
        , addAt :: Responds (k1 /\ HashMap k2 v) Unit
        , remove :: Responds (k1 /\ k2) Unit
        , insert :: Responds ((k1 /\ k2) /\ v) Unit
        , replace :: Responds ((k1 /\ k2) /\ v) Unit
        )
    )
    ( self :: (HashMap2D k1 k2 v)
    , element :: v
    , key :: k1 /\ k2
    , d1key :: k1
    , d2key :: k2
    , d1element :: HashMap k2 v
    ) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r (R'HashMap2D (st /\ _)) = match
    { extract: responds'const'eff $ eff'hm2D'freeze st
    , extractAt: responds'run'eff \k1 -> eff'hm2D'freezeAt k1 st
    , size: responds'const'eff $ eff'bin2D'size st
    , sizeAt: responds'run'eff \k1 -> eff'bin2D'sizeAt k1 st
    , vals: responds'const'eff $ eff'hm2D'entries st <#> map \e -> e.v
    , valsAt: responds'run'eff \k1 -> eff'bin2D'valsAt k1 st <#> map _.v
    , entries: responds'const'eff $ eff'hm2D'entries st
        <#> map \e -> (e.k1 /\ e.k2) /\ e.v
    , entriesAt: responds'run'eff \k1 ->
        eff'bin2D'valsAt k1 st <#> map \e -> e.k /\ e.v
    , keys: responds'const'eff $ eff'hm2D'entries st <#> map \e -> e.k1 /\ e.k2
    , keysAt: responds'run'eff \k1 -> eff'bin2D'valsAt k1 st <#> map _.k
    , lookup: responds'run'eff
        \(k1 /\ k2) -> eff'bin2D'lookup k1 k2 st <#> map _.v
    , d1keys: responds'const'eff $ eff'bin2D'd1keys st
    }
  x'r'mkResponds'w (R'HashMap2D (st /\ init)) = match
    { assign: responds'run'eff \hm2d -> eff'hm2D'set hm2d st
    , assignAt: responds'run'eff \(k1 /\ hm2d) -> eff'hm2D'setAt k1 hm2d st
    , reset: responds'const'eff $ eff'hm2D'set init st
    , resetAt: responds'run'eff \k -> case hm'lookup k init of
        Just hm -> eff'hm2D'setAt k hm st
        Nothing -> eff'bin2D'clearAt k st
    , clear: responds'const'eff $ eff'bin2D'clear st
    , clearAt: responds'run'eff \k1 -> eff'bin2D'clearAt k1 st
    , add: responds'run'eff \hm -> eff'hm2D'add hm st
    , addAt: responds'run'eff \(k1 /\ hm) -> eff'hm2D'addAt k1 hm st
    , remove: responds'run'eff \(k1 /\ k2) -> eff'bin2D'delete k1 k2 st
    , insert: responds'run'eff \((k1 /\ k2) /\ v) ->
        eff'bin2D'insert k1 k2 { k: k2, v } st
    , replace: responds'run'eff \((k1 /\ k2) /\ v) ->
        eff'bin2D'lookup k1 k2 st >>= \curr ->
          whenJust curr $ const $ eff'bin2D'insert k1 k2 { k: k2, v } st
    }

---------------------------------------------------------------------

type X'HashMap2D k1 k2 v = Reader (RW'Tagged (R'HashMap2D k1 k2 v))
type X'HashMap2D'w k1 k2 v = Reader (W'Tagged (R'HashMap2D k1 k2 v))

x'hashmap2D
  :: forall @k1 @k2 @v
   . Identable k1
  => Identable k2
  => HashMap k1 (HashMap k2 v)
  -> X'Evaluable (X'HashMap2D k1 k2 v)
x'hashmap2D t = x'evaluable t

x'hashmap2D'w
  :: forall @k1 @k2 @v
   . Identable k1
  => Identable k2
  => HashMap k1 (HashMap k2 v)
  -> X'Evaluable (X'HashMap2D'w k1 k2 v)
x'hashmap2D'w t = x'evaluable t

x'hashmap2D_
  :: forall @k1 @k2 @v
   . Identable k1
  => Identable k2
  => X'Evaluable (X'HashMap2D k1 k2 v)
x'hashmap2D_ = x'evaluable_

x'hashmap2D_'w
  :: forall @k1 @k2 @v
   . Identable k1
  => Identable k2
  => X'Evaluable (X'HashMap2D k1 k2 v)
x'hashmap2D_'w = x'evaluable_

---------------------------------------------------------------------

eff'hm2D'addAt
  :: forall k1 k2 v
   . Identable k1
  => Identable k2
  => k1
  -> HashMap k2 v
  -> Eff'HashMap2D k1 k2 v
  -> Effect Unit
eff'hm2D'addAt k hm st = eff'bin2D'addAt k (unwrap hm) st

eff'hm2D'add
  :: forall k1 k2 v
   . Identable k1
  => Identable k2
  => HashMap2D k1 k2 v
  -> Eff'HashMap2D k1 k2 v
  -> Effect Unit
eff'hm2D'add hm2d st =
  for_ (hm'entries hm2d) \(k /\ hm) -> eff'hm2D'addAt k hm st

eff'hm2D'setAt
  :: forall k1 k2 v
   . Identable k1
  => Identable k2
  => k1
  -> HashMap k2 v
  -> Eff'HashMap2D k1 k2 v
  -> Effect Unit
eff'hm2D'setAt k hm st = eff'bin2D'clearAt k st *> eff'hm2D'addAt k hm st

eff'hm2D'set
  :: forall k1 k2 v
   . Identable k1
  => Identable k2
  => HashMap2D k1 k2 v
  -> Eff'HashMap2D k1 k2 v
  -> Effect Unit
eff'hm2D'set hm2d st = eff'bin2D'clear st *> eff'hm2D'add hm2d st

eff'hm2D'freezeAt
  :: forall k1 k2 v
   . Identable k1
  => Identable k2
  => k1
  -> Eff'HashMap2D k1 k2 v
  -> Effect (HashMap k2 v)
eff'hm2D'freezeAt k st = wrap <$> eff'bin2D'freezeAt k st

eff'hm2D'freeze
  :: forall k1 k2 v
   . Identable k1
  => Identable k2
  => Eff'HashMap2D k1 k2 v
  -> Effect (HashMap2D k1 k2 v)
eff'hm2D'freeze st = do
  d1keys <- eff'bin2D'd1keys st
  entries <- for d1keys \k1 -> eff'hm2D'freezeAt k1 st <#> (/\) k1
  pure $ hm'fromFoldable entries

eff'hm2D'entries
  :: forall k1 k2 v
   . Identable k1
  => Identable k2
  => Eff'HashMap2D k1 k2 v
  -> Effect (Array { k1 :: k1, k2 :: k2, v :: v })
eff'hm2D'entries st = do
  hm2d <- eff'hm2D'freeze st
  pure $ arr'concat $ hm'entries hm2d <#> \(k1 /\ hm) -> hm'entries hm <#>
    \(k2 /\ v) -> { k1, k2, v }