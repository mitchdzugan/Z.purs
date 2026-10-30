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
import Z.Z.Core (arr'concat, may'__, (<##>))
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
  , XG'd1'entries
  , XG'd1'keys
  , XG'entries
  , XG'extract
  , XG'keys
  , XG'reset
  , XG'size
  , XG'vals
  , XM'add
  , XM'addAt
  , XM'assign
  , XM'assignAt
  , XM'clearAt
  , XM'consAt
  , XM'entriesAt
  , XM'extractAt
  , XM'keysAt
  , XM'lookup
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
  )

type HashSet2D k a = HashMap k (HashSet a)
type Eff'HashSet2D k a = Eff'Bin2D k a

newtype R'HashSet2D k a = R'HashSet2D (Eff'HashSet2D k a /\ HashSet2D k a)

instance XM'type (R'HashSet2D k a) XMt'self (HashSet2D k a)

instance XM'type (R'HashSet2D k a) XMt'key (k /\ a)

instance XM'type (R'HashSet2D k a) XMt'value a

instance XM'type (R'HashSet2D k a) XMt'd1'key k

instance XM'type (R'HashSet2D k a) XMt'd1'value (HashSet a)

instance XM'type (R'HashSet2D k a) XMt'd2'key a

---------------------------------------------------------------------

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Getter XG'extract (R'HashSet2D k a) rwTag (HashSet2D k a) where
  eff'rw'get (R'HashSet2D (st /\ _)) = eff'hs2D'freeze st

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'size (R'HashSet2D k a) rwTag Int where
  eff'rw'get (R'HashSet2D (st /\ _)) = eff'bin2D'size st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Getter XG'vals (R'HashSet2D k a) rwTag (Array a) where
  eff'rw'get (R'HashSet2D (st /\ _)) = eff'hs2D'vals st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Getter XG'keys (R'HashSet2D k a) rwTag (Array a) where
  eff'rw'get (R'HashSet2D (st /\ _)) = eff'hs2D'vals st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Getter XG'entries (R'HashSet2D k a) rwTag (Array (a /\ a)) where
  eff'rw'get (R'HashSet2D (st /\ _)) = eff'hs2D'vals st <##> \a -> a /\ a

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'lookup (R'HashSet2D k a) rwTag (k /\ a) (Maybe a) where
  eff'rw'call (R'HashSet2D (st /\ _)) (k /\ a) = eff'bin2D'lookup k a st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'extractAt (R'HashSet2D k a) rwTag k (HashSet a) where
  eff'rw'call (R'HashSet2D (st /\ _)) k = eff'hs2D'freezeAt k st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'sizeAt (R'HashSet2D k a) rwTag k Int where
  eff'rw'call (R'HashSet2D (st /\ _)) k = eff'bin2D'sizeAt k st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'valsAt (R'HashSet2D k a) rwTag k (Array a) where
  eff'rw'call (R'HashSet2D (st /\ _)) k = eff'bin2D'valsAt k st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'keysAt (R'HashSet2D k a) rwTag k (Array a) where
  eff'rw'call (R'HashSet2D (st /\ _)) k = eff'bin2D'valsAt k st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'entriesAt (R'HashSet2D k a) rwTag k (Array (a /\ a)) where
  eff'rw'call (R'HashSet2D (st /\ _)) k =
    eff'bin2D'valsAt k st <##> \a -> a /\ a

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Getter XG'd1'keys (R'HashSet2D k a) rwTag (Array k) where
  eff'rw'get (R'HashSet2D (st /\ _)) = eff'bin2D'd1keys st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Getter XG'd1'entries
    (R'HashSet2D k a)
    rwTag
    (Array (k /\ HashSet a)) where
  eff'rw'get (R'HashSet2D (st /\ _)) = eff'hs2D'd1entries st

-----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'assign (R'HashSet2D k a) rwTag (HashSet2D k a) Unit where
  eff'rw'call (R'HashSet2D (st /\ _)) hs2d = eff'hs2D'set hs2d st

-----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'assignAt (R'HashSet2D k a) rwTag (k /\ HashSet a) Unit where
  eff'rw'call (R'HashSet2D (st /\ _)) (k /\ hs) = eff'hs2D'setAt k hs st

-----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Getter XG'reset (R'HashSet2D k a) rwTag Unit where
  eff'rw'get (R'HashSet2D (st /\ init)) = eff'hs2D'set init st

-----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'resetAt (R'HashSet2D k a) rwTag k Unit where
  eff'rw'call (R'HashSet2D (st /\ init)) k = may'__
    (\_ -> eff'bin2D'clearAt k st)
    (\hs -> eff'hs2D'setAt k hs st)
    (hm'lookup k init)

-----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Getter XG'clear (R'HashSet2D k a) rwTag Unit where
  eff'rw'get (R'HashSet2D (st /\ _)) = eff'bin2D'clear st

-----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'clearAt (R'HashSet2D k a) rwTag k Unit where
  eff'rw'call (R'HashSet2D (st /\ _)) k = eff'bin2D'clearAt k st

-----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'add (R'HashSet2D k a) rwTag (HashSet2D k a) Unit where
  eff'rw'call (R'HashSet2D (st /\ _)) hs2d = eff'hs2D'add hs2d st

-----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'addAt (R'HashSet2D k a) rwTag (k /\ HashSet a) Unit where
  eff'rw'call (R'HashSet2D (st /\ _)) (k /\ hs) = eff'hs2D'addAt k hs st

-----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'consAt (R'HashSet2D k a) rwTag (k /\ a) Unit where
  eff'rw'call (R'HashSet2D (st /\ _)) (k /\ a) = eff'bin2D'insert k a a st

-----------------------------

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  , Identable a
  ) =>
  Eff'RW'Method XM'remove (R'HashSet2D k a) rwTag (k /\ a) Unit where
  eff'rw'call (R'HashSet2D (st /\ _)) (k /\ a) = eff'bin2D'delete k a st

---------------------------------------------------------------------

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
eff'hs2D'freeze st = eff'hs2D'd1entries st <#> hm'fromFoldable

eff'hs2D'd1entries
  :: forall k1 a
   . Identable k1
  => Eff'HashSet2D k1 a
  -> Effect (Array (k1 /\ HashSet a))
eff'hs2D'd1entries st = do
  d1keys <- eff'bin2D'd1keys st
  for d1keys \k1 -> eff'hs2D'freezeAt k1 st <#> (/\) k1

eff'hs2D'vals
  :: forall k1 a
   . Identable k1
  => Identable a
  => Eff'HashSet2D k1 a
  -> Effect (Array a)
eff'hs2D'vals st =
  eff'hs2D'freeze st <#> hm'vals <#> map hs'vals <#> arr'concat