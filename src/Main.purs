module Main where

import Prelude

import Z.Node.Gql as Gql
import Z.Node.H2h as H2h
import Z.Node.Sys as Sys
import Z.Z as Z

testCachePath :: String
testCachePath =
  "/home/dz/Repo/PS-WS/.cache-path"

main :: Z.Effect Unit
main = Sys.xExecAndExit do
  authToken <- Sys.xLookupEnv "CLM_STATS_GG_AUTH" >>= Z.xUnwrap'
  client <- pure $ H2h.mkClient do
    Z.xSet Gql._authToken $ Z.Just authToken
    Z.xSet Gql._cachePath $ Z.Just testCachePath
  let slug = "tournament/bracket-at-the-emporium-3/event/melee-singles"
  let source = H2h.startggSource slug
  eventData <- H2h.getEventData source client Z.default
  Z.xInfo eventData
