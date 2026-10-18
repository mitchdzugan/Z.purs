module Test.Main where

import Z.Test.Prelude

main :: TestMain
main = discoverAndRunSpecs [ consoleReporter ] """Test\.Z\..*"""
