module Z.Z.X.Readables.RW.HashMap2D
  ( B'HashMap2D
  , Eff'HashMap2D
  , HashMap2D
  , R'HashMap2D(..)
  , X'HashMap2D
  , X'HashMap2D'w
  , x'hashmap2D
  , x'hashmap2D'w
  , x'hashmap2D_
  , x'hashmap2D_'w
  ) where

import Z.Z.X.UtilPrelude

import Data.Foldable (for_)
import Data.Traversable (for)
import Z.Z.Core (arr'concat, may'__, (<##>))
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
import Z.Z.HashMap (HashMap, hm'entries, hm'fromFoldable, hm'lookup)
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
  , XM'entriesAt
  , XM'extractAt
  , XM'insert
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

type HashMap2D k1 k2 v = HashMap k1 (HashMap k2 v)
type Eff'HashMap2D k1 k2 v = Eff'Bin2D k1 { k :: k2, v :: v }

newtype R'HashMap2D k1 k2 v =
  R'HashMap2D (Eff'HashMap2D k1 k2 v /\ HashMap2D k1 k2 v)

instance XM'type (R'HashMap2D k1 k2 v) XMt'self (HashMap2D k1 k2 v)

instance XM'type (R'HashMap2D k1 k2 v) XMt'key (k1 /\ k2)

instance XM'type (R'HashMap2D k1 k2 v) XMt'value v

instance XM'type (R'HashMap2D k1 k2 v) XMt'd1'key k1

instance XM'type (R'HashMap2D k1 k2 v) XMt'd1'value (HashMap k2 v)

instance XM'type (R'HashMap2D k1 k2 v) XMt'd2'key k2

---------------------------------------------------------------------

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Getter XG'extract (R'HashMap2D k1 k2 v) rwTag (HashMap2D k1 k2 v) where
  eff'rw'get (R'HashMap2D (st /\ _)) = eff'hm2D'freeze st

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'size (R'HashMap2D k1 k2 v) rwTag Int where
  eff'rw'get (R'HashMap2D (st /\ _)) = eff'bin2D'size st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Getter XG'vals (R'HashMap2D k1 k2 v) rwTag (Array v) where
  eff'rw'get (R'HashMap2D (st /\ _)) = eff'hm2D'entries st <##> \e -> e.v

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Getter XG'keys (R'HashMap2D k1 k2 v) rwTag (Array (k1 /\ k2)) where
  eff'rw'get (R'HashMap2D (st /\ _)) =
    eff'hm2D'entries st <#> map \e -> e.k1 /\ e.k2

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Getter
    XG'entries
    (R'HashMap2D k1 k2 v)
    rwTag
    (Array ((k1 /\ k2) /\ v)) where
  eff'rw'get (R'HashMap2D (st /\ _)) =
    eff'hm2D'entries st <#> map \e -> (e.k1 /\ e.k2) /\ e.v

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method XM'lookup (R'HashMap2D k1 k2 v) rwTag (k1 /\ k2) (Maybe v) where
  eff'rw'call (R'HashMap2D (st /\ _)) (k1 /\ k2) =
    eff'bin2D'lookup k1 k2 st <#> map _.v

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method XM'extractAt (R'HashMap2D k1 k2 v) rwTag k1 (HashMap k2 v) where
  eff'rw'call (R'HashMap2D (st /\ _)) k1 = eff'hm2D'freezeAt k1 st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method XM'sizeAt (R'HashMap2D k1 k2 v) rwTag k1 Int where
  eff'rw'call (R'HashMap2D (st /\ _)) k1 = eff'bin2D'sizeAt k1 st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method XM'valsAt (R'HashMap2D k1 k2 v) rwTag k1 (Array v) where
  eff'rw'call (R'HashMap2D (st /\ _)) k1 = eff'bin2D'valsAt k1 st <#> map _.v

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method
    XM'entriesAt
    (R'HashMap2D k1 k2 v)
    rwTag
    k1
    (Array (k2 /\ v)) where
  eff'rw'call (R'HashMap2D (st /\ _)) k1 =
    eff'bin2D'valsAt k1 st <#> map \e -> e.k /\ e.v

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method XM'keysAt (R'HashMap2D k1 k2 v) rwTag k1 (Array k2) where
  eff'rw'call (R'HashMap2D (st /\ _)) k1 = eff'bin2D'valsAt k1 st <#> map _.k

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Getter XG'd1'keys (R'HashMap2D k1 k2 v) rwTag (Array k1) where
  eff'rw'get (R'HashMap2D (st /\ _)) = eff'bin2D'd1keys st

instance
  ( RW'Tag'CanRead rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Getter XG'd1'entries
    (R'HashMap2D k1 k2 v)
    rwTag
    (Array (k1 /\ HashMap k2 v)) where
  eff'rw'get (R'HashMap2D (st /\ _)) = eff'hm2D'd1entries st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method
    XM'assign
    (R'HashMap2D k1 k2 v)
    rwTag
    (HashMap2D k1 k2 v)
    Unit where
  eff'rw'call (R'HashMap2D (st /\ _)) hm2d = eff'hm2D'set hm2d st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Getter XG'reset (R'HashMap2D k1 k2 v) rwTag Unit where
  eff'rw'get (R'HashMap2D (st /\ init)) = eff'hm2D'set init st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'clear (R'HashMap2D k1 k2 v) rwTag Unit where
  eff'rw'get (R'HashMap2D (st /\ _)) = eff'bin2D'clear st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method
    XM'add
    (R'HashMap2D k1 k2 v)
    rwTag
    (HashMap2D k1 k2 v)
    Unit where
  eff'rw'call (R'HashMap2D (st /\ _)) hm2d = eff'hm2D'add hm2d st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method XM'remove (R'HashMap2D k1 k2 v) rwTag (k1 /\ k2) Unit where
  eff'rw'call (R'HashMap2D (st /\ _)) (k1 /\ k2) = eff'bin2D'delete k1 k2 st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method
    XM'insert
    (R'HashMap2D k1 k2 v)
    rwTag
    ((k1 /\ k2) /\ v)
    Unit where
  eff'rw'call (R'HashMap2D (st /\ _)) ((k1 /\ k2) /\ v) =
    eff'bin2D'insert k1 k2 { k: k2, v } st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method XM'assignAt
    (R'HashMap2D k1 k2 v)
    rwTag
    (k1 /\ HashMap k2 v)
    Unit where
  eff'rw'call (R'HashMap2D (st /\ _)) (k1 /\ hm2d) = eff'hm2D'setAt k1 hm2d st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method XM'resetAt
    (R'HashMap2D k1 k2 v)
    rwTag
    k1
    Unit where
  eff'rw'call (R'HashMap2D (st /\ init)) k1 = may'__
    (\_ -> eff'bin2D'clearAt k1 st)
    (\hm -> eff'hm2D'setAt k1 hm st)
    (hm'lookup k1 init)

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method XM'clearAt
    (R'HashMap2D k1 k2 v)
    rwTag
    k1
    Unit where
  eff'rw'call (R'HashMap2D (st /\ _)) k1 = eff'bin2D'clearAt k1 st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k1
  , Identable k2
  ) =>
  Eff'RW'Method XM'addAt
    (R'HashMap2D k1 k2 v)
    rwTag
    (k1 /\ HashMap k2 v)
    Unit where
  eff'rw'call (R'HashMap2D (st /\ _)) (k1 /\ hm2d) = eff'hm2D'addAt k1 hm2d st

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
eff'hm2D'freeze st = eff'hm2D'd1entries st <#> hm'fromFoldable

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

eff'hm2D'd1entries
  :: forall k1 k2 v
   . Identable k1
  => Identable k2
  => Eff'HashMap2D k1 k2 v
  -> Effect (Array $ k1 /\ HashMap k2 v)
eff'hm2D'd1entries st = do
  keys <- eff'bin2D'd1keys st
  for keys \k -> eff'hm2D'freezeAt k st <#> (/\) k

---------------------------------------------------------------------

type B'HashMap2D
  :: forall k. ((Type -> Type) -> Type -> k) -> Type -> Type -> Type -> k
type B'HashMap2D sel k1 k2 v = sel (X'HashMap2D k1 k2 v) (HashMap2D k1 k2 v)