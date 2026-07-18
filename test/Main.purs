module Test.Main where

import Z.Test.Index as T

main :: T.TestMain
main = T.discoverAndRunSpecs [ T.consoleReporter ] """Test\.Z\..*"""
