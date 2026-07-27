module Test.Scratch where

import Node.Z.Prelude
import Node.Z.H2h as H2h
import Z.SSBM.Slp.Read.Impl as SlpRead

testCachePath :: String
testCachePath = "/home/dz/Repo/PS-WS/.cache-path"

main :: Effect Unit
main = xExecAndExit do
  b <- readFile "/home/dz/Slippi/Game_20260709T183630.slp"
  let game = SlpRead.game b
  xInfo $ SlpRead.stats game
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
{-}
authToken <- Z.xMapE Z.Left $ Sys.xLookupEnv "CLM_STATS_GG_AUTH" >>=
  Z.xUnwrap'
client <- pure $ H2h.mkClient do
  Z.xSet_ @"authToken" $ Z.Just authToken
  Z.xSet_ @"cachePath" $ Z.Just testCachePath
-- let slug = "tournament/bracket-at-the-emporium-3/event/melee-singles"
-- let source = H2h.startggSource slug
let slug = "840lhvjn"
let source = H2h.challongeSource slug
eventDataRes <- H2h.getEventData source client Z.default
eventData <- Z.xMapE Z.Right $ Z.xUnresult eventDataRes
Z.xOut $ Z.encode eventData
-}