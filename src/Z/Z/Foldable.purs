module Z.Z.Foldable
  ( Sorter
  , SorterF
  , arr'idented
  , average
  , averageBy
  , groupBy
  , hm'mapSomeKeys
  , keyBy
  , multisort
  , ordinal
  , sorter
  , sorter'adapt
  , sorter'rev
  , stats
  , stats'percentile
  , stats'zScore
  , statsBy
  , stddev
  , stddevBy
  , withMultisortInd
  ) where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.List (List(..))
import Data.Maybe (Maybe)
import Data.Number (pow, sqrt)
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Z.Z.Core
  ( class Numberable
  , arr'concat
  , arr'size
  , arr'withInd
  , may'or
  , numberable'toNumber
  , zScoreToPercentile
  , (<$$>)
  )
import Z.Z.HashMap
  ( HashMap
  , hm'collect
  , hm'entries
  , hm'lookup
  , hm'vals
  , hm2d'collect
  )
import Z.Z.HashSet (HashSet, hs'collect, hs'has)
import Z.Z.Id (class Identable, IdV, Idented, ident'get, idented'mk, idented'v)
import Z.Z.It
  ( class Iterable
  , arr'collect
  , collect
  , it'some
  , it'sum
  , it'withInd
  , iterable'mk
  , list'collect
  )
import Z.Z.Util (arr'sortBy)

keyBy
  :: forall it k v
   . Identable k
  => Iterable it v
  => (v -> k)
  -> it
  -> HashMap k v
keyBy toKey coll = collect @HashMap $ iterable'mk coll <#> \v -> toKey v /\ v

arr'idented :: forall it v. Iterable it v => it -> Array (Idented Int v)
arr'idented a = uncurry idented'mk <$> (arr'withInd $ arr'collect a)

groupBy
  :: forall it k v
   . Identable k
  => Iterable it v
  => (v -> k)
  -> it
  -> HashMap k (Array v)
groupBy k c = hm'vals <$$> hm2d'collect $ it'withInd c <#> \p -> k (snd p) /\ p

hm'mapSomeKeys
  :: forall k1 k2 v
   . Identable k1
  => Identable k2
  => (k1 -> Maybe k2)
  -> HashMap k1 v
  -> HashMap k2 v
hm'mapSomeKeys mapKey hm = collect @HashMap $ it'some $ iterable'mk hm <#>
  \(k1 /\ v) -> mapKey k1 <#> \k2 -> k2 /\ v

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

sorter'adapt :: forall a1 a2. (a2 -> a1) -> Sorter a1 -> Sorter a2
sorter'adapt f s = runExists useSorter s
  where
  useSorter :: forall s. SorterF a1 s -> Sorter a2
  useSorter (SorterF { toS, compareS, idS }) = mkExists $ SorterF
    { toS: toS <<< f
    , compareS
    , idS
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

multisort :: forall it v. Iterable it v => Array (Sorter v) -> it -> Array v
multisort sorters coll =
  multisort'impl (list'collect sorters) $ arr'collect coll

withMultisortInd
  :: forall it v. Iterable it v => Array (Sorter v) -> it -> Array (Int /\ v)
withMultisortInd sorters coll =
  let
    arr = arr'collect coll
  in
    js_withNewInds (/\) arr (multisort $ sorters <#> sorter'adapt _.el)

average :: forall it n. Iterable it n => Numberable n => it -> Number
average = averageBy identity

averageBy
  :: forall it v n. Iterable it v => Numberable n => (v -> n) -> it -> Number
averageBy toN coll = _.avg $ statsBy toN coll

stddev :: forall it n. Iterable it n => Numberable n => it -> Number
stddev = stddevBy identity

stddevBy
  :: forall it v n. Iterable it v => Numberable n => (v -> n) -> it -> Number
stddevBy toN coll = _.stddev $ statsBy toN coll

type Stats = { avg :: Number, stddev :: Number }

stats :: forall it n. Iterable it n => Numberable n => it -> Stats
stats = statsBy identity

statsBy
  :: forall it v n. Iterable it v => Numberable n => (v -> n) -> it -> Stats
statsBy toN coll =
  let
    nums = arr'collect coll <#> numberable'toNumber <<< toN
    size = numberable'toNumber (arr'size nums)
    avg = it'sum nums / size
    variances = nums <#> \n -> pow (n - avg) 2.0
    variance = it'sum variances / size
  in
    { avg, stddev: sqrt variance }

stats'zScore :: forall n. Numberable n => n -> Stats -> Number
stats'zScore n s = (numberable'toNumber n - s.avg) / s.stddev

stats'percentile :: forall n. Numberable n => n -> Stats -> Number
stats'percentile n s = zScoreToPercentile $ stats'zScore n s

weirdOrdinalSuffixes :: HashSet Int
weirdOrdinalSuffixes = hs'collect [ 11, 12, 13 ]

baseOrdinalSuffixes :: HashMap Int String
baseOrdinalSuffixes = hm'collect [ 1 /\ "st", 2 /\ "nd", 3 /\ "rd" ]

ordinalSuffix :: Int -> String
ordinalSuffix n =
  if hs'has (n `mod` 100) weirdOrdinalSuffixes then "th"
  else may'or "th" $ hm'lookup (n `mod` 10) baseOrdinalSuffixes

ordinal :: Int -> String
ordinal n = show n <> ordinalSuffix n

foreign import js_withNewInds
  :: forall a
   . (Int -> a -> Int /\ a)
  -> Array a
  -> (Array { el :: a } -> Array { el :: a })
  -> Array (Int /\ a)