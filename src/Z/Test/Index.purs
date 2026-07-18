module Z.Test.Index
  ( module Spec
  , module SpecAssert
  , module SpecDisc
  , module SpecRepCon
  , SpecMain
  , TestMain
  ) where

import Prelude
import Effect (Effect)
import Test.Spec (pending, describe, it, Spec) as Spec
import Test.Spec.Assertions (shouldEqual) as SpecAssert
import Test.Spec.Discovery (discoverAndRunSpecs) as SpecDisc
import Test.Spec.Reporter.Console (consoleReporter) as SpecRepCon

type TestMain = Effect Unit
type SpecMain = Spec.Spec Unit
