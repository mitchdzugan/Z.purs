module Main where

import Prelude

import Z as Z
import Z.H2h.Node.Module as H2h
import Z.SSBM.Slp.Rec.Node.Impl as SlpRec
import Z.SSBM.Slp.Read.Impl as SlpRead
import Z.Sys.Node.Module as Sys

testCachePath :: String
testCachePath = "/home/dz/Repo/PS-WS/.cache-path"

main :: Z.Effect Unit
main = Sys.xExecAndExit do
  Z.xMapE Z.Left $ SlpRec.run Z.Nothing
  _ <- Z.xFail $ Z.Left $ Z.jsError "asdf" "adf"
  b <- Z.xMapE Z.Left $ Sys.readFile "/home/dz/Slippi/Game_20260709T183630.slp"
  let game = SlpRead.game b
  Z.xInfo $ SlpRead.stats game
  _ <- Z.xFail $ Z.Left $ Z.jsError "asdf" "adf"
  Z.xInfo $ Z.key
    [ Z.key (-1)
    , Z.key (0 Z./\ 3)
    , Z.key 1
    , Z.key [ 1, 2, 3, 4 ]
    , Z.key 3
    , Z.key 98
    , Z.key 99
    , Z.key 100
    , Z.key 101
    ]
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
