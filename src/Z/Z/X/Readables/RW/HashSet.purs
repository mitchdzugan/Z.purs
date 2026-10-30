module Z.Z.X.Readables.RW.HashSet
  ( B'HashSet
  , R'HashSet
  , X'HashSet
  , X'HashSet'w
  , x'hashset
  , x'hashset'w
  , x'hashset_
  , x'hashset_'w
  ) where

import Z.Z.X.UtilPrelude

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
import Z.Z.HashSet (HashSet)
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
  , XM'cons
  , XM'lookup
  , XM'remove
  , XMt'key
  , XMt'self
  , XMt'value
  )

newtype R'HashSet a = R'HashSet (Eff'Bin a /\ HashSet a)

instance XM'type (R'HashSet a) XMt'self (HashSet a)

instance XM'type (R'HashSet a) XMt'key a

instance XM'type (R'HashSet a) XMt'value a

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'extract (R'HashSet a) rwTag (HashSet a) where
  eff'rw'get (R'HashSet (st /\ _)) = eff'hs'freeze st

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'size (R'HashSet a) rwTag Int where
  eff'rw'get (R'HashSet (st /\ _)) = eff'bin'size st

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'vals (R'HashSet a) rwTag (Array a) where
  eff'rw'get (R'HashSet (st /\ _)) = eff'bin'vals st

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'keys (R'HashSet a) rwTag (Array a) where
  eff'rw'get (R'HashSet (st /\ _)) = eff'bin'vals st

instance
  ( RW'Tag'CanRead rwTag
  ) =>
  Eff'RW'Getter XG'entries (R'HashSet a) rwTag (Array (a /\ a)) where
  eff'rw'get (R'HashSet (st /\ _)) = eff'bin'vals st <#> map \e -> e /\ e

instance
  ( RW'Tag'CanRead rwTag
  , Identable k
  ) =>
  Eff'RW'Method XM'lookup (R'HashSet a) rwTag k (Maybe a) where
  eff'rw'call (R'HashSet (st /\ _)) k = eff'bin'lookup k st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'assign (R'HashSet a) rwTag (HashSet a) Unit where
  eff'rw'call (R'HashSet (st /\ _)) hs = eff'hs'set hs st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'reset (R'HashSet a) rwTag Unit where
  eff'rw'get (R'HashSet (st /\ init)) = eff'hs'set init st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Getter XG'clear (R'HashSet a) rwTag Unit where
  eff'rw'get (R'HashSet (st /\ _)) = eff'bin'clear st

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'add (R'HashSet a) rwTag (HashSet a) Unit where
  eff'rw'call (R'HashSet (st /\ _)) hs = eff'bin'add (unwrap hs) st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable a
  ) =>
  Eff'RW'Method XM'remove (R'HashSet a) rwTag a Unit where
  eff'rw'call (R'HashSet (st /\ _)) k = eff'bin'delete k st

instance
  ( RW'Tag'CanWrite rwTag
  , Identable a
  ) =>
  Eff'RW'Method XM'cons (R'HashSet a) rwTag a Unit where
  eff'rw'call (R'HashSet (st /\ _)) a = eff'bin'insert a a st

instance
  Identable a =>
  X'Readable'rw (R'HashSet a) (HashSet a) where
  x'readable'rw'mk hs'initial = do
    st <- eff'bin'new
    eff'hs'set hs'initial st
    pure $ R'HashSet $ st /\ hs'initial

instance
  Identable a =>
  X'Results'R'rw (R'HashSet a) (HashSet a) where
  x'results'r'rw'impl (R'HashSet (st /\ _)) = eff'hs'freeze st

---------------------------------------------------------------------

type X'HashSet a = Reader (RW'Tagged (R'HashSet a))
type X'HashSet'w a = Reader (W'Tagged (R'HashSet a))

x'hashset :: forall @a. Identable a => HashSet a -> X'Evaluable (X'HashSet a)
x'hashset t = x'evaluable t

x'hashset'w
  :: forall @a. Identable a => HashSet a -> X'Evaluable (X'HashSet'w a)
x'hashset'w t = x'evaluable t

x'hashset_ :: forall @a. Identable a => X'Evaluable (X'HashSet a)
x'hashset_ = x'evaluable_

x'hashset_'w :: forall @a. Identable a => X'Evaluable (X'HashSet'w a)
x'hashset_'w = x'evaluable_

---------------------------------------------------------------------

eff'hs'set :: forall a. HashSet a -> Eff'Bin a -> Effect Unit
eff'hs'set hs st = eff'bin'clear st *> eff'bin'add (unwrap hs) st

eff'hs'freeze :: forall a. Eff'Bin a -> Effect (HashSet a)
eff'hs'freeze st = wrap <$> eff'bin'freeze st

type B'HashSet :: forall k. ((Type -> Type) -> Type -> k) -> Type -> k
type B'HashSet sel a = sel (X'HashSet a) (HashSet a)