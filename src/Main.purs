module Main where

import Prelude

import Z.Node.Gql.Index as Gql
import Z.Node.H2h.Index as H2h
import Z.Node.Sys.Index as Sys
import Z.Z as Z

main :: Z.Effect Unit
main = Sys.xExecAndExit do
  txtRes <- Sys.readTextFile "/home/dz/Repo/PS-WS/index.js"
  Z.logInfo { txtRes }
  authToken <- Sys.xLookupEnv "CLM_STATS_GG_AUTH" >>= Z.xUnwrap
    (Z.jsError "Nothing#unwrap" "")
  let
    client = H2h.mkClient do
      Z.xSet Gql._authToken $ Z.Just authToken
      Z.xSet Gql._cachePath $ Z.Just "/home/dz/.cache-path"
    slug = "tournament/bracket-at-the-emporium-3/event/melee-singles"
    source = H2h.startggSource slug
  {-
  Z.logInfo "Starting aff"
  res <- Z.xTry $ Z.auto $ Gql.operate client Q.tourneyData
    { pageE: 1
    , pageS: 1
    , slug: "tournament/bracket-at-the-emporium-3/event/melee-singles"
    }
  Z.logInfo { res, b: { xD: "asdf" } }
  -}
  eventData <- H2h.getEventData source client Z.default
  Z.logInfo eventData
