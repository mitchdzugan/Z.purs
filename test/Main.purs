module Test.Main where

import Prelude

import Z.Node.H2h.Index as H2h
import Z.Node.Gql.Index as Gql
import Z.Node.H2h.GGQueries.Queries as Q
import Z.Node.Sys.Index as Sys
import Z.Z as Z

main :: Z.Effect Unit
main = do
  authToken <- Sys.lookupEnv "CLM_STATS_GG_AUTH"
  let client = H2h.mkClient $ Z.s_set Gql._authToken authToken
  Z.launchAff_ $ Z.discard $ Z.runBaseAff $ Z.runExcept do
    Z.logInfo "Starting aff"
    res <- Z.runExcept $ Z.auto $ Gql.operate client Q.tourneyData
      { pageE: 1
      , pageS: 1
      , slug: "tournament/bracket-at-the-emporium-3/event/melee-singles"
      }
    Z.logInfo { res, b: { xD: "asdf" } }
