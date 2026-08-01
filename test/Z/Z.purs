module Test.Z.Z where

import Z.Test.Prelude

spec :: SpecMain
spec = do
  it "adds 1 and 1" do
    1 + 1 =?= 2
