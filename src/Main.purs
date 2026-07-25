module Main where

import Prelude

import Z as Z
import Z.H2h.Node.Module as H2h
import Z.Sys.Node.Module as Sys

testCachePath :: String
testCachePath = "/home/dz/Repo/PS-WS/.cache-path"

main :: Z.Effect Unit
main = Sys.xExecAndExit do
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
  authToken <- Sys.xLookupEnv "CLM_STATS_GG_AUTH" >>= Z.xUnwrap'
  client <- pure $ H2h.mkClient do
    Z.xSet_ @"authToken" $ Z.Just authToken
    Z.xSet_ @"cachePath" $ Z.Just testCachePath
  -- let slug = "tournament/bracket-at-the-emporium-3/event/melee-singles"
  let slug = "840lhvjn"
  let source = H2h.challongeSource slug
  eventData <- H2h.getEventData source client Z.default
  Z.xInfo $ Z.encode eventData
