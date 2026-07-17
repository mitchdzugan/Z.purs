module Test.Main where

import Prelude

import H2h.Node as H2h
import Gql.Node as Gql
import H2h.GGQueries.Queries as Q
import Sys.Node as Sys
import Z as Z

main :: Z.Effect Unit
main = do
  authToken <- Sys.lookupEnv "CLM_STATS_GG_AUTH"
  let client = H2h.mkClient $ Z.s_set Gql._authToken authToken
  Z.launchAff_ $ Z.discard $ Z.runBaseAff $ Z.runExcept do
    Z.logInfo "Starting aff"
    res <- Z.runExcept $ Gql.operate' client Q.tourneyData
      { pageE: 1
      , pageS: 1
      , slug: "tournament/bracket-at-the-emporium-3/event/melee-singles"
      }
    Z.logInfo { res, b: { xD: "asdf" } }
