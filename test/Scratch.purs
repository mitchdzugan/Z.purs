module Test.Scratch where

import Node.Z.Prelude

import Heterogeneous.Mapping (hmap)
import Z.SSBM.Slp.Read.Impl as SlpRead
import Z.Z.Buildable as B

testCachePath :: String
testCachePath = "/home/dz/Repo/PS-WS/.cache-path"

main :: Effect Unit
main = runXAThenExit @Void @Void do
  -- b <- xReadFile "/home/dz/Slippi/Game_20260709T183630.slp"
  -- parsed <- e'map un' $ SlpRead.xParse b
  -- xOut $ key parsed
  let
    bspec =
      { i: Cons (B.Const'Set unit) Nil
      , s: Cons (B.Const'Set "curr") (Cons (B.Const'Set "prev") Nil)
      }
  xOut $ hmap B.BuildableMapper bspec