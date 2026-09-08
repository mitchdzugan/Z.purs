module Z.Z.X6.Methods
  ( x'get
  , x'reset
  , x'set
  ) where

import Z.Z.X6.UtilPrelude

import Z.Z.X6.Core (class X'RespondsTo, x'respondTo, x'respondTo_)
import Z.Z.X6.Responds (Responds, Responds'Const)

type T'self :: forall k. k -> Row k -> Row k
type T'self t rest = (self :: t | rest)

---------------------------------------------------------------------

type T'get t rest = (get :: Responds'Const t | rest)

x'get
  :: forall @p m x' x t resp'rest resp't'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'get t resp'rest) (T'self t resp't'rest)
  => Run x t
x'get = x'respondTo_ @p @"get"

---------------------------------------------------------------------

type T'set t rest = (set :: Responds t Unit | rest)

x'set
  :: forall @p m x' x t resp'rest resp't'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'set t resp'rest) (T'self t resp't'rest)
  => t
  -> Run x Unit
x'set = x'respondTo @p @"set"

---------------------------------------------------------------------

type T'reset rest = (reset :: Responds'Const Unit | rest)

x'reset
  :: forall @p m x' x t resp'rest resp't'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'reset resp'rest) (T'self t resp't'rest)
  => Run x Unit
x'reset = x'respondTo_ @p @"reset"
