module Test.Scratch where

import Node.Z.Prelude

import Z.SSBM.Slp.Read.Impl as SlpRead

testCachePath :: String
testCachePath = "/home/dz/Repo/PS-WS/.cache-path"

main :: Effect Unit
main = runXAThenExit do
  b <- xReadFile "/home/dz/Slippi/Game_20260709T183630.slp"
  parsed <- g @XMapE un' $ SlpRead.xParse b
  xOut $ key parsed
