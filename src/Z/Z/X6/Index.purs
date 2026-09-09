module Z.Z.X6.Index
  ( R'
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
  , r'ask
  , s'get
  , s'set
  , type (<@<)
  , type (>@>)
  , x'now
  , x'out
  , module Methods
  ) where

import Z.Z.X6.UtilPrelude

import Run.Except (Except)
import Z.Z.DateTime (DateTime)
import Z.Z.X6.Methods as Methods
import Z.Z.X6.Readables.Base (LogLevel(..), X'Base, x'now'', x'out'')
import Z.Z.X6.Readables.RW.Ref (X'Ref)
import Z.Z.X6.Readables.RW.Vector (X'Writer)

type X x res = Run (_'x'base :: X'Base | x) res

type X'flipped res x = X x res

infixr 0 type X'flipped as <@<
infixr 0 type X as >@>

x'now :: forall x. DateTime <@< x
x'now = x'now'' @"_'x'base"

x'out :: forall x a. a -> Unit <@< x
x'out a = x'out'' @"_'x'base" LogLevel'Info a

type X'R t = Reader (Identity t)
type X'W t = X'Writer t
type X'Wa t = X'Writer (Array t)
type X'S t = X'Ref t

type X'E :: forall k. Type -> k -> Type
type X'E e = Except e

type X'A = X'Ref Int

type R' r x = (_'x'reader :: X'R r | x)
type W' w x = (_'x'writer :: X'W w | x)
type Wa' w x = (_'x'writer :: X'Wa w | x)
type S' s x = (_'x'state :: X'S s | x)

type RS' r s x = R' r $ S' s x

r'ask :: forall x r. Run (R' r x) r
r'ask = Methods.x'extract @"_'x'reader"

s'get :: forall x s. Run (S' s x) s
s'get = Methods.x'extract @"_'x'state"

s'set :: forall x s. s -> Run (S' s x) Unit
s'set = Methods.x'assign @"_'x'state"
