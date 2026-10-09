module Z.Z.Foldable
  ( SorterF
  , arr'fromTraversable
  , arr'idented
  , average
  , averageBy
  , groupBy
  , hm'mapSomeKeys
  , keyBy
  , multisort
  , sorter
  , sorter'rev
  , stats
  , stats'percentile
  , stats'zScore
  , statsBy
  , stddev
  , stddevBy
  ) where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Number (pow, sqrt)
import Data.Traversable (class Traversable, sum)
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested ((/\))
import Z.Z.Core
  ( class Numberable
  , arr'concat
  , arr'size
  , arr'withInd
  , ffmap
  , forM_
  , numberable'toNumber
  , zScoreToPercentile
  )
import Z.Z.HashMap (HashMap, hm'entries)
import Z.Z.HashSet (hs'vals)
import Z.Z.Id (class Identable, IdV, Idented, ident'get, idented'mk, idented'v)
import Z.Z.Passable (pass)
import Z.Z.Util (arr'sortBy)
import Z.Z.X.Core (_'exec_)
import Z.Z.X.Index (sync'x)
import Z.Z.X.Methods (_'cons, _'consAt, _'insert)
import Z.Z.X.Readables.RW.HashMap (X'HashMap)
import Z.Z.X.Readables.RW.HashSet2D (X'HashSet2D)
import Z.Z.X.Readables.RW.Vector (X'Vector)

keyBy
  :: forall f k v
   . Identable k
  => Traversable f
  => (v -> k)
  -> f v
  -> HashMap k v
keyBy toKey coll = sync'x $ _'exec_ @(X'HashMap k v) do
  forM_ coll \v -> _'insert (toKey v) v

arr'fromTraversable :: forall f v. Traversable f => f v -> Array v
arr'fromTraversable coll = sync'x $ _'exec_ @(X'Vector v) do
  forM_ coll \v -> _'cons v

arr'idented :: forall f v. Traversable f => f v -> Array (Idented Int v)
arr'idented a = uncurry idented'mk <$> (arr'withInd $ arr'fromTraversable a)

groupBy
  :: forall f k v
   . Identable k
  => Traversable f
  => (v -> k)
  -> f v
  -> HashMap k (Array v)
groupBy toKey coll =
  ffmap idented'v
    <$> map hs'vals
    <$> sync'x $ _'exec_ @(X'HashSet2D k (Idented Int v)) do
    forM_ (arr'idented coll) \idented -> do
      _'consAt (toKey $ idented'v idented) idented

hm'mapSomeKeys
  :: forall k1 k2 v
   . Identable k1
  => Identable k2
  => (k1 -> Maybe k2)
  -> HashMap k1 v
  -> HashMap k2 v
hm'mapSomeKeys mapKey hm = sync'x $ _'exec_ @(X'HashMap k2 v) do
  forM_ (hm'entries hm) \(k1 /\ v) -> case mapKey k1 of
    Just k2 -> _'insert k2 v
    Nothing -> pass

newtype SorterF a s = SorterF
  { toS :: a -> s
  , compareS :: s -> s -> Ordering
  , idS :: s -> IdV
  }

type Sorter a = Exists (SorterF a)

sorter :: forall a s. Ord s => Identable s => (a -> s) -> Sorter a
sorter toS = mkExists $ SorterF
  { toS
  , compareS: \s1 s2 -> compare s1 s2
  , idS: ident'get
  }

sorter'rev :: forall a s. Ord s => Identable s => (a -> s) -> Sorter a
sorter'rev toS = mkExists $ SorterF
  { toS
  , compareS: \s1 s2 -> compare s2 s1
  , idS: ident'get
  }

sorter'groups :: forall a. Sorter a -> Array a -> Array (Array a)
sorter'groups = runExists useSorter
  where
  useSorter :: forall s. SorterF a s -> Array a -> Array (Array a)
  useSorter (SorterF { toS, compareS, idS }) arr =
    let
      identedS = \a -> let s = toS a in idented'mk (idS s) s
      entryS = \(idented /\ _) -> idented'v idented
      compareEntries = \e1 e2 -> compareS (entryS e1) (entryS e2)
    in
      map snd $ arr'sortBy compareEntries $ hm'entries $ groupBy identedS arr

multisort'impl :: forall a. List (Sorter a) -> Array a -> Array a
multisort'impl Nil arr = arr
multisort'impl (Cons s s'rest) arr = arr'concat
  $ map (multisort'impl s'rest) (sorter'groups s arr)

multisort :: forall f v. Traversable f => Array (Sorter v) -> f v -> Array v
multisort sorters coll =
  multisort'impl (fromFoldable sorters) $ arr'fromTraversable coll

average :: forall f n. Traversable f => Numberable n => f n -> Number
average = averageBy identity

averageBy
  :: forall f v n. Traversable f => Numberable n => (v -> n) -> f v -> Number
averageBy toN coll = _.avg $ statsBy toN coll

stddev :: forall f n. Traversable f => Numberable n => f n -> Number
stddev = stddevBy identity

stddevBy
  :: forall f v n. Traversable f => Numberable n => (v -> n) -> f v -> Number
stddevBy toN coll = _.stddev $ statsBy toN coll

type Stats = { avg :: Number, stddev :: Number }

stats :: forall f n. Traversable f => Numberable n => f n -> Stats
stats = statsBy identity

statsBy
  :: forall f v n. Traversable f => Numberable n => (v -> n) -> f v -> Stats
statsBy toN coll =
  let
    nums = arr'fromTraversable coll <#> numberable'toNumber <<< toN
    size = numberable'toNumber (arr'size nums)
    avg = sum nums / size
    variances = nums <#> \n -> pow (n - avg) 2.0
    variance = sum variances / size
  in
    { avg, stddev: sqrt variance }

stats'zScore :: forall n. Numberable n => n -> Stats -> Number
stats'zScore n s = (numberable'toNumber n - s.avg) / s.stddev

stats'percentile :: forall n. Numberable n => n -> Stats -> Number
stats'percentile n s = zScoreToPercentile $ stats'zScore n s