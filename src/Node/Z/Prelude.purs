module Node.Z.Prelude
  ( NodeXflipped
  , module Prelude
  , module SysImpl
  , module Sys
  , type (##>)
  , type (<##)
  ) where

import Z.Prelude as Prelude
import Node.Z.Sys.SysImpl as SysImpl
import Z.Sys.Module (FSDataError(..)) as Sys

type NodeXflipped a x = SysImpl.XNode x a

infixr 0 type SysImpl.XNode as ##>

infixr 0 type NodeXflipped as <##