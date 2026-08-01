module Test.Scratch where

import Node.Z.Prelude
import Z.SSBM.Slp.Read.Impl as SlpRead

testCachePath :: String
testCachePath = "/home/dz/Repo/PS-WS/.cache-path"

main :: Effect Unit
main = xExecAndExit do
  -- b <- readFile "/home/dz/Slippi/Game_20260709T183630.slp"
  -- let game = SlpRead.game b
  -- xInfo $ SlpRead.stats game
  _ <- xFail $ jsError "asdf" "adf"
  xInfo $ key
    [ key (-1)
    , key (0 /\ 3)
    , key 1
    , key [ 1, 2, 3, 4 ]
    , key 3
    , key 98
    , key 99
    , key 100
    , key 101
    ]
