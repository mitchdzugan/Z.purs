module Z.Z.X.Readables.RW.Writer
  ( R'Writer(..)
  , X'Writer
  , x'writer
  ) where

import Prelude

import Z.Z.Eff.Array
  ( Eff'Array
  , eff'array'cons
  , eff'array'empty
  , eff'array'freeze
  )
import Z.Z.It (it'fold)
import Z.Z.X.Core
  ( class RW'Tag'CanWrite
  , class X'Readable'rw
  , class X'Results'R'rw
  , RW'Tagged
  , X'Evaluable
  , x'evaluable_
  )
import Z.Z.X.Method
  ( class Eff'RW'Method
  , class XM'type
  , XM'cons
  , XMt'key
  , XMt'self
  , XMt'value
  )

newtype R'Writer monoid = R'Writer (Eff'Array monoid)

instance Monoid monoid => X'Readable'rw (R'Writer monoid) Unit where
  x'readable'rw'mk _ = R'Writer <$> eff'array'empty

instance Monoid monoid => X'Results'R'rw (R'Writer monoid) monoid where
  x'results'r'rw'impl (R'Writer st) = eff'array'freeze st <#> it'fold

instance
  ( RW'Tag'CanWrite rwTag
  ) =>
  Eff'RW'Method XM'cons (R'Writer a) rwTag a Unit where
  eff'rw'call (R'Writer st) el = eff'array'cons el st

instance XM'type (R'Writer a) XMt'self (Array a)

instance XM'type (R'Writer a) XMt'key Int

instance XM'type (R'Writer a) XMt'value a

---------------------------------------------------------------------

type X'Writer a = RW'Tagged (R'Writer a)

x'writer :: forall @monoid. Monoid monoid => X'Evaluable (X'Writer monoid)
x'writer = x'evaluable_