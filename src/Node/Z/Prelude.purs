module Node.Z.Prelude
  ( NodeXflipped
  , module Prelude
  , module SysImpl
  , module Sys
  , type (##>)
  , type (<##)
  ) where

import Z.Prelude as Prelude
import Node.Z.Sys.SysImpl (class Pathlike, EnvPaths, Path, Platform(..), XNode, XNodeF, argParse, basename, decodeAnyYamlExt, decodeTextFile, decodeYamlFile, dirname, encodeTextFile, encodeTextFileP, envCfg, envData, envTmp, lookupEnv, mkdir, mkdirP, pathJoin, pathJoinAbs, pathStr, readFile, readTextFile, writeTextFile, writeTextFileP, xArgv, xEnvPaths, xExecAndExit, xExecAndExitArgv, xLookupEnv, xPlatform, xWd, (/./), (/.|//)) as SysImpl
import Z.Sys.Module (FSDataError(..)) as Sys

type NodeXflipped a x = SysImpl.XNode x a

infixr 0 type SysImpl.XNode as ##>

infixr 0 type NodeXflipped as <##