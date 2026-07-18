module Test.Z.Z where

import Prelude

import Z.Test.Index as T

spec :: T.SpecMain
spec = do
  T.it "adds 1 and 1" do (1 + 1) `T.shouldEqual` 2
