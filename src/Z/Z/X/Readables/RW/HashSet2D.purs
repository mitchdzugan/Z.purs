module Z.Z.X.Readables.RW.HashSet2D
  ( Eff'HashSet2D
  , HashSet2D
  , R'HashSet2D(..)
  , X'HashSet2D
  , X'HashSet2D'w
  , x'hashset2D
  , x'hashset2D'w
  , x'hashset2D_
  , x'hashset2D_'w
  ) where

import Z.Z.X.UtilPrelude

import Data.Foldable (for_)
import Data.Traversable (for)
import Z.Z.Core (arr'concat)
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
import Z.Z.HashSet (HashSet, hs'vals)
import Z.Z.Id (class Identable)
import Z.Z.X.Core
  ( class X'R'RespondsTo'rw
  , class X'Readable'rw
  , class X'Results'R'rw
  , RW'Tagged
  , W'Tagged
  , X'Evaluable
  , x'evaluable
  , x'evaluable_
  )
import Z.Z.X.Responds
  ( Responds
  , Responds'Const
  , responds'const'eff
  , responds'run'eff
  )

type HashSet2D k a = HashMap k (HashSet a)
type Eff'HashSet2D k a = Eff'Bin2D k a

newtype R'HashSet2D k a = R'HashSet2D (Eff'HashSet2D k a /\ HashSet2D k a)

instance
  ( Identable k
  , Identable a
  ) =>
  X'Readable'rw (R'HashSet2D k a) (HashSet2D k a) where
  x'readable'rw'mk hs'initial = do
    st <- eff'bin2D'new
    eff'hs2D'set hs'initial st
    pure $ R'HashSet2D $ st /\ hs'initial

instance
  ( Identable k
  , Identable a
  ) =>
  X'Results'R'rw (R'HashSet2D k a) (HashSet2D k a) where
  x'results'r'rw'impl (R'HashSet2D (st /\ _)) = eff'hs2D'freeze st

instance
  ( Identable k
  , Identable a
  ) =>
  X'R'RespondsTo'rw (R'HashSet2D k a)
    ( VariantF
        ( extract :: Responds'Const (HashSet2D k a)
        , extractAt :: Responds k (HashSet a)
        , size :: Responds'Const Int
        , sizeAt :: Responds k Int
        , vals :: Responds'Const (Array a)
        , valsAt :: Responds k (Array a)
        , lookup :: Responds (k /\ a) (Maybe a)
        , d1keys :: Responds'Const (Array k)
        )
    )
    ( VariantF
        ( assign :: Responds (HashSet2D k a) Unit
        , assignAt :: Responds (k /\ HashSet a) Unit
        , reset :: Responds'Const Unit
        , resetAt :: Responds k Unit
        , clear :: Responds'Const Unit
        , clearAt :: Responds k Unit
        , add :: Responds (HashSet2D k a) Unit
        , addAt :: Responds (k /\ HashSet a) Unit
        , consAt :: Responds (k /\ a) Unit
        , remove :: Responds (k /\ a) Unit
        )
    )
    ( self :: (HashSet2D k a)
    , element :: a
    , key :: k /\ a
    , d1key :: k
    , d1element :: HashSet a
    ) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r (R'HashSet2D (st /\ _)) = match
    { extract: responds'const'eff $ eff'hs2D'freeze st
    , extractAt: responds'run'eff \k -> eff'hs2D'freezeAt k st
    , size: responds'const'eff $ eff'bin2D'size st
    , sizeAt: responds'run'eff \k -> eff'bin2D'sizeAt k st
    , vals: responds'const'eff
        $ eff'hs2D'freeze st
        <#> hm'vals
        <#> map hs'vals
        <#> arr'concat
    , valsAt: responds'run'eff \k -> eff'bin2D'valsAt k st
    , lookup: responds'run'eff \(k /\ a) -> eff'bin2D'lookup k a st
    , d1keys: responds'const'eff $ eff'bin2D'd1keys st
    }
  x'r'mkResponds'w (R'HashSet2D (st /\ init)) = match
    { assign: responds'run'eff \hs2d -> eff'hs2D'set hs2d st
    , assignAt: responds'run'eff \(k /\ hs2d) -> eff'hs2D'setAt k hs2d st
    , reset: responds'const'eff $ eff'hs2D'set init st
    , resetAt: responds'run'eff \k -> case hm'lookup k init of
        Just hs -> eff'hs2D'setAt k hs st
        Nothing -> eff'bin2D'clearAt k st
    , clear: responds'const'eff $ eff'bin2D'clear st
    , clearAt: responds'run'eff \k -> eff'bin2D'clearAt k st
    , add: responds'run'eff \hs -> eff'hs2D'add hs st
    , addAt: responds'run'eff \(k /\ hs) -> eff'hs2D'addAt k hs st
    , consAt: responds'run'eff \(k /\ a) -> eff'bin2D'insert k a a st
    , remove: responds'run'eff \(k /\ a) -> eff'bin2D'delete k a st
    }

---------------------------------------------------------------------

type X'HashSet2D k a = Reader (RW'Tagged (R'HashSet2D k a))
type X'HashSet2D'w k a = Reader (W'Tagged (R'HashSet2D k a))

x'hashset2D
  :: forall @k @a
   . Identable k
  => Identable a
  => HashMap k (HashSet a)
  -> X'Evaluable (X'HashSet2D k a)
x'hashset2D t = x'evaluable t

x'hashset2D'w
  :: forall @k @a
   . Identable k
  => Identable a
  => HashMap k (HashSet a)
  -> X'Evaluable (X'HashSet2D'w k a)
x'hashset2D'w t = x'evaluable t

x'hashset2D_
  :: forall @k @a. Identable k => Identable a => X'Evaluable (X'HashSet2D k a)
x'hashset2D_ = x'evaluable_

x'hashset2D_'w
  :: forall @k @a. Identable k => Identable a => X'Evaluable (X'HashSet2D'w k a)
x'hashset2D_'w = x'evaluable_

---------------------------------------------------------------------

eff'hs2D'addAt
  :: forall k a
   . Identable k
  => k
  -> HashSet a
  -> Eff'HashSet2D k a
  -> Effect Unit
eff'hs2D'addAt k hs st = eff'bin2D'addAt k (unwrap hs) st

eff'hs2D'add
  :: forall k a
   . Identable k
  => HashSet2D k a
  -> Eff'HashSet2D k a
  -> Effect Unit
eff'hs2D'add hs2d st =
  for_ (hm'entries hs2d) \(k /\ hs) -> eff'hs2D'addAt k hs st

eff'hs2D'setAt
  :: forall k a
   . Identable k
  => k
  -> HashSet a
  -> Eff'HashSet2D k a
  -> Effect Unit
eff'hs2D'setAt k hs st = eff'bin2D'clearAt k st *> eff'hs2D'addAt k hs st

eff'hs2D'set
  :: forall k a
   . Identable k
  => HashSet2D k a
  -> Eff'HashSet2D k a
  -> Effect Unit
eff'hs2D'set hs2d st = eff'bin2D'clear st *> eff'hs2D'add hs2d st

eff'hs2D'freezeAt
  :: forall k1 a. Identable k1 => k1 -> Eff'HashSet2D k1 a -> Effect (HashSet a)
eff'hs2D'freezeAt k st = wrap <$> eff'bin2D'freezeAt k st

eff'hs2D'freeze
  :: forall k1 a. Identable k1 => Eff'HashSet2D k1 a -> Effect (HashSet2D k1 a)
eff'hs2D'freeze st = do
  d1keys <- eff'bin2D'd1keys st
  entries <- for d1keys \k1 -> eff'hs2D'freezeAt k1 st <#> (/\) k1
  pure $ hm'fromFoldable entries