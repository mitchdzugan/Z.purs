module Z.Node.H2h.Builder.Startgg where

import Prelude

import Z.H2h.Index as H2h
import Z.Node.Gql.Index as Gql
import Z.Node.H2h.Builder.Startgg.Queries as Q
import Z.Node.H2h.Util as U
import Z.Z as Z

getEventData :: forall x. U.GetDataFn x
getEventData = U.adaptBuilder do
  Z.logInfo "Starting aff"
  { client, slug } <- Z.xAsk
  let vars = { pageE: 1, pageS: 1, slug }
  res <- Z.xMapE H2h.Fetch $ Gql.operate client Q.tourneyData vars Z.default
  Z.logInfo res
  pure
    { id: Z.asOrNum "tourneyId"
    , name: "Melee Singles"
    , slug
    , state: "COMPLETE"
    , tournamentName: "Bracket at the Emporium 3"
    , site: H2h.Startgg
    }
