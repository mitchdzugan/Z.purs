module Node.Z.Prelude
  ( T'NodeX
  , T'NodeXflipped
  , module Prelude
  , module Sys
  , module SysImpl
  , type (@$>)
  , type (<$@)
  ) where

import Node.Z.Sys.SysImpl as SysImpl
import Z.Prelude as Prelude
import Z.Sys.Module (FSDataError(..)) as Sys

type T'NodeX x a = SysImpl.X'Node x Prelude.@@> a
type T'NodeXflipped a x = T'NodeX x a

infixr 0 type T'NodeX as @$>

infixr 0 type T'NodeXflipped as <$@