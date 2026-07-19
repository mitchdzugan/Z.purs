module Main where

import Prelude

import Z.Node.Gql as Gql
import Z.Node.H2h as H2h
import Z.Node.Sys as Sys
import Z.Z as Z

testCachePath :: String
testCachePath =
  "/home/dz/Projects/workspace.dz-ts/@dz-ssbm/gql/test/test_files/.cache-path"

main :: Z.Effect Unit
main = Sys.xExecAndExit do
  txtRes <- Sys.readTextFile "/home/dz/Repo/PS-WS/index.js"
  Z.logInfo { txtRes }
  authToken <- Sys.xLookupEnv "CLM_STATS_GG_AUTH" >>= Z.xUnwrap
    (Z.jsError "Nothing#unwrap" "")
  client <- pure $ H2h.mkClient do
    Z.xSet Gql._authToken $ Z.Just authToken
    Z.xSet Gql._cachePath $ Z.Just testCachePath
  let slug = "tournament/rpm-97/event/melee-singles"
  let source = H2h.startggSource slug
  eventData <- H2h.getEventData source client Z.default
  Z.logInfo eventData
