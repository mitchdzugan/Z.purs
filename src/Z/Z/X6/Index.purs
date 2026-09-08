module Z.Z.X6.Index where

import Z.Z.X6.UtilPrelude

import Z.Z.DateTime (DateTime)
import Z.Z.X6.Base (LogLevel(..), X'Base, x'now'', x'out'')

type X x a = Run (_'x'base :: X'Base | x) a

type X'flipped a x = X x a

infixr 0 type X'flipped as <\

infixr 0 type X as />

x'now :: forall x. DateTime <\ x
x'now = x'now'' @"_'x'base"

x'out :: forall x a. a -> Unit <\ x
x'out a = x'out'' @"_'x'base" LogLevel'Info a