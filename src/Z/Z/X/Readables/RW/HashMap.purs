module Z.Z.X.Readables.RW.HashMap
  ( B'HashMap
  , R'HashMap
  , X'HashMap
  , X'HashMap'w
  , x'hashmap
  , x'hashmap'w
  , x'hashmap_
  , x'hashmap_'w
  ) where

import Z.Z.X.UtilPrelude

import Z.Z.Defaultable (whenJust)
import Z.Z.Eff.Bin
  ( Eff'Bin
  , eff'bin'add
  , eff'bin'clear
  , eff'bin'delete
  , eff'bin'freeze
  , eff'bin'insert
  , eff'bin'lookup
  , eff'bin'new
  , eff'bin'size
  , eff'bin'vals
  )
import Z.Z.HashMap (HashMap)
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
  , XG'entries
  , XG'extract
  , XG'keys
  , XG'reset
  , XG'size
  , XG'vals
  , XM'add
  , XM'assign
  , XM'insert
  , XM'lookup
  , XM'remove
  , XMt'key
  , XMt'self
  , XMt'value
  )

type Eff'HashMap k v = Eff'Bin { k :: k, v :: v }

newtype R'HashMap k v = R'HashMap (Eff'HashMap k v /\ HashMap k v)

instance XM'type (R'HashMap k v) XMt'self (HashMap k v)

instance XM'type (R'HashMap k v) XMt'key k

instance XM'type (R'HashMap k v) XMt'value v

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'extract (R'HashMap k v) rwTag (HashMap k v) where
  eff'rw'get (R'HashMap (st /\ _)) = eff'hm'freeze st

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'size (R'HashMap k v) rwTag Int where
  eff'rw'get (R'HashMap (st /\ _)) = eff'bin'size st

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'vals (R'HashMap k v) rwTag (Array v) where
  eff'rw'get (R'HashMap (st /\ _)) = eff'bin'vals st <#> map _.v

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'keys (R'HashMap k v) rwTag (Array k) where
  eff'rw'get (R'HashMap (st /\ _)) = eff'bin'vals st <#> map _.k

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'entries (R'HashMap k v) rwTag (Array (k /\ v)) where
  eff'rw'get (R'HashMap (st /\ _)) = eff'bin'vals st <#> map \e -> e.k /\ e.v

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  ) =>
  Eff'RW'Method XM'lookup (R'HashMap k v) rwTag k (Maybe v) where
  eff'rw'call (R'HashMap (st /\ _)) k = eff'bin'lookup k st <#> map _.v

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'assign (R'HashMap k v) rwTag (HashMap k v) Unit where
  eff'rw'call (R'HashMap (st /\ _)) hm = eff'hm'set hm st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'reset (R'HashMap k v) rwTag Unit where
  eff'rw'get (R'HashMap (st /\ init)) = eff'hm'set init st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'clear (R'HashMap k v) rwTag Unit where
  eff'rw'get (R'HashMap (st /\ _)) = eff'bin'clear st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'add (R'HashMap k v) rwTag (HashMap k v) Unit where
  eff'rw'call (R'HashMap (st /\ _)) hm = eff'bin'add (unwrap hm) st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  ) =>
  Eff'RW'Method XM'remove (R'HashMap k v) rwTag k Unit where
  eff'rw'call (R'HashMap (st /\ _)) k = eff'bin'delete k st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable k
  ) =>
  Eff'RW'Method XM'insert (R'HashMap k v) rwTag (k /\ v) Unit where
  eff'rw'call (R'HashMap (st /\ _)) (k /\ v) = eff'bin'insert k { k, v } st

instance
  Identable k =>
  X'Readable'rw (R'HashMap k v) (HashMap k v) where
  x'readable'rw'mk hm'initial = do
    st <- eff'bin'new
    eff'hm'set hm'initial st
    pure $ R'HashMap $ st /\ hm'initial

instance
  Identable k =>
  X'Results'R'rw (R'HashMap k v) (HashMap k v) where
  x'results'r'rw'impl (R'HashMap (st /\ _)) = eff'hm'freeze st

---------------------------------------------------------------------

type X'HashMap k v = Reader (RW'Tagged (R'HashMap k v))
type X'HashMap'w k v = Reader (W'Tagged (R'HashMap k v))

x'hashmap
  :: forall @k @v. Identable k => HashMap k v -> X'Evaluable (X'HashMap k v)
x'hashmap t = x'evaluable t

x'hashmap'w
  :: forall @k @v. Identable k => HashMap k v -> X'Evaluable (X'HashMap'w k v)
x'hashmap'w t = x'evaluable t

x'hashmap_ :: forall @k @v. Identable k => X'Evaluable (X'HashMap k v)
x'hashmap_ = x'evaluable_

x'hashmap_'w :: forall @k @v. Identable k => X'Evaluable (X'HashMap'w k v)
x'hashmap_'w = x'evaluable_

---------------------------------------------------------------------

eff'hm'set :: forall k v. HashMap k v -> Eff'HashMap k v -> Effect Unit
eff'hm'set hm st = eff'bin'clear st *> do
  eff'bin'add (unwrap hm) st

eff'hm'freeze :: forall k v. Eff'HashMap k v -> Effect (HashMap k v)
eff'hm'freeze st = wrap <$> eff'bin'freeze st

type B'HashMap
  :: forall k1. ((Type -> Type) -> Type -> k1) -> Type -> Type -> k1
type B'HashMap sel k v = sel (X'HashMap k v) (HashMap k v)