module Test.Scratch where

import Node.Z.Prelude

import Z.SSBM.Slp.Read.Impl as SlpRead
import Z.Z.Bin (xbin'insert, xbin'run, xbin'size, xbin'vals)
import Z.Z.Id as Id

testCachePath :: String
testCachePath = "/home/dz/Repo/PS-WS/.cache-path"

main :: Effect Unit
main = runXAThenExit do
  xOut $ encode
    [ Id.ident'uuid 1
    , Id.ident'uuid 2
    , Id.ident'uuid 0
    , Id.ident'uuid 0.0
    , Id.ident'uuid 0.001
    , Id.ident'uuid 0.002
    , Id.ident'uuid (-1)
    , Id.ident'uuid 99
    , Id.ident'uuid 100
    , Id.ident'uuid 101
    , Id.ident'uuid ""
    , Id.ident'uuid "1"
    , Id.ident'uuid "asdf"
    , Id.ident'uuid true
    , Id.ident'uuid false
    , Id.ident'uuid $ Just 0 ~ Just 0
    , Id.ident'uuid $ Just 0 ~ Nothing
    ]
  xbin'run @"test" do
    v1 <- xbin'vals @"test"
    s1 <- xbin'size @"test"
    xbin'insert @"test" "a" 123
    v2 <- xbin'vals @"test"
    s2 <- xbin'size @"test"
    xOut { v1, v2, s1, s2 }
  b <- xReadFile "/home/dz/Slippi/Game_20260709T183630.slp"
  parsed <- e'map un' $ SlpRead.xParse b
  xOut $ key parsed