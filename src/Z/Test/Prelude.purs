module Z.Test.Prelude
  ( (=?=)
  , SpecMain
  , TestMain
  , module Spec
  , module SpecAssert
  , module SpecDisc
  , module SpecRepCon
  , module ZPrelude
  ) where

import Test.Spec (Spec, describe, it, pending) as Spec
import Test.Spec.Assertions (shouldEqual) as SpecAssert
import Test.Spec.Discovery (discoverAndRunSpecs) as SpecDisc
import Test.Spec.Reporter.Console (consoleReporter) as SpecRepCon
import Z.Prelude as ZPrelude

type TestMain = ZPrelude.Effect ZPrelude.Unit
type SpecMain = Spec.Spec ZPrelude.Unit

infixr 0 SpecAssert.shouldEqual as =?=