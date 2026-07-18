module Z.Test.Index
  ( module Spec
  , module SpecAssert
  , module SpecDisc
  , module SpecRepCon
  ) where

import Test.Spec (pending, describe, it, Spec) as Spec
import Test.Spec.Assertions (shouldEqual) as SpecAssert
import Test.Spec.Discovery (discoverAndRunSpecs) as SpecDisc
import Test.Spec.Reporter.Console (consoleReporter) as SpecRepCon
