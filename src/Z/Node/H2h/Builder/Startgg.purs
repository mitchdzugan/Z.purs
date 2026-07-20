module Z.Node.H2h.Builder.Startgg where

import Prelude

import Z.H2h as H2h
import Z.Node.Gql as Gql
import Z.Node.H2h.Builder.Startgg.Queries as Q
import Z.Node.H2h.Util as U
import Z.Z as Z

getEventData :: forall x. U.GetDataFn x
getEventData = U.adaptBuilder do
  Z.xInfo "Starting aff"
  { client, slug } <- Z.xAsk
  let vars1 = { pageE: 1, pageS: 1, slug }
  let vars2 = { pageE: 0, pageS: 1, slug }
  res1 <- Z.xMapE H2h.Fetch $ Gql.operate client Q.tourneyData vars1 Z.default
  Z.xInfo res1
  res2 <- Z.xMapE H2h.Fetch $ Gql.operate client Q.tourneyData vars2 Z.default
  Z.xInfo res2
  pure
    { id: Z.asOrNum "tourneyId"
    , name: "Melee Singles"
    , slug
    , state: "COMPLETE"
    , tournamentName: "Bracket at the Emporium 3"
    , site: H2h.Startgg
    }
