module Z.Z.X6.Index
  ( A'
  , E'
  , R'
  , RS'
  , S'
  , X
  , X'A
  , X'E
  , X'R
  , X'S
  , X'W
  , X'Wa
  , X'flipped
  , async'x
  , module Methods
  , r'ask
  , s'get
  , s'set
  , sync'x
  , type (<@<)
  , type (<@@)
  , type (>@>)
  , type (@@>)
  , x'aff
  , x'now
  , x'nowMS
  , x'out
  , x'outErr
  , x'outWarn
  ) where

import Z.Z.X6.UtilPrelude

import Effect.Aff (Aff)
import Run.Except (Except)
import Z.Z.DateTime (DateTime, dateTime'toMS)
import Z.Z.X6.Async (AffF(..), x'aff'')
import Z.Z.X6.Core (x'eval_)
import Z.Z.X6.Methods as Methods
import Z.Z.X6.Readables.Base (LogLevel(..), X'Base, x'now'', x'out'')
import Z.Z.X6.Readables.RW.Ref (X'Ref)
import Z.Z.X6.Readables.RW.Vector (X'Writer)

type X x res = Run (_'x'base :: X'Base | x) res

type X'flipped res x = X x res

infixr 0 type X'flipped as <@<
infixr 0 type X as >@>

infixr 0 type X'flipped as <@@
infixr 0 type X as @@>

x'now :: forall x. DateTime <@< x
x'now = x'now'' @"_'x'base"

x'nowMS :: forall x. Number <@< x
x'nowMS = x'now <#> dateTime'toMS

x'out :: forall x a. a -> Unit <@< x
x'out a = x'out'' @"_'x'base" LogLevel'Info a

x'outErr :: forall x a. a -> Unit <@< x
x'outErr a = x'out'' @"_'x'base" LogLevel'Error a

x'outWarn :: forall x a. a -> Unit <@< x
x'outWarn a = x'out'' @"_'x'base" LogLevel'Warning a

type X'R t = Reader (Identity t)
type X'W t = X'Writer t
type X'Wa t = X'Writer (Array t)
type X'S t = X'Ref t

type X'E :: forall k. Type -> k -> Type
type X'E e = Except e

type X'A = AffF

type R' r x = (_'x'reader :: X'R r | x)
type W' w x = (_'x'writer :: X'W w | x)
type Wa' w x = (_'x'writer :: X'Wa w | x)
type S' s x = (_'x'state :: X'S s | x)
type E' e x = (_'x'except :: X'E e | x)
type A' x = (_'x'aff :: AffF | x)

type RS' r s x = R' r $ S' s x

r'ask :: forall x r. Run (R' r x) r
r'ask = Methods.x'extract @"_'x'reader"

s'get :: forall x s. Run (S' s x) s
s'get = Methods.x'extract @"_'x'state"

s'set :: forall x s. s -> Run (S' s x) Unit
s'set = Methods.x'assign @"_'x'state"

x'aff :: forall x a. Aff a -> Run (A' x) a
x'aff = x'aff'' @"_'x'aff"

sync'x :: forall a. () @@> a -> a
sync'x m = unsafePerformEffect $ runBaseEffect $ expand
  $ x'eval_ @"_'x'base" @X'Base m

async'x :: forall a. A' () @@> a -> Aff a
async'x m = match { _'x'aff: \(AffCmd a) -> a } # run $ expand
  $ x'eval_ @"_'x'base" @X'Base m