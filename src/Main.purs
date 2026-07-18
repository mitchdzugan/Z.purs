module Main where

import Prelude

import Z.Node.Gql.Index as Gql
import Z.Node.H2h.GGQueries.Queries as Q
import Z.Node.H2h.Index as H2h
import Z.Node.Sys.Index as Sys
import Z.Z as Z

main :: Z.Effect Unit
main = Sys.xExecAndExit do
  Z.logInfo "hi"
  txtRes <- Sys.readTextFile "/home/dz/Repo/PS-WS/index.js"
  Z.logInfo { txtRes }
  authToken <- Sys.xLookupEnv "CLM_STATS_GG_AUTH"
  let client = H2h.mkClient $ Z.s_set Gql._authToken authToken
  Z.logInfo "Starting aff"
  res <- Z.xTry $ Z.auto $ Gql.operate client Q.tourneyData
    { pageE: 1
    , pageS: 1
    , slug: "tournament/bracket-at-the-emporium-3/event/melee-singles"
    }
  Z.logInfo { res, b: { xD: "asdf" } }
