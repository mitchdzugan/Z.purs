module Test.Main where

import Prelude

import Z.Z as Z
import Z.Test.Index as T

main :: Z.Effect Unit
main = T.discoverAndRunSpecs [ T.consoleReporter ] """Test\.Z\..*"""
