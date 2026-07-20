module Z.Node.H2h.Builder.Startgg.Index where

import Prelude

import Z.H2h as H2h
import Z.Node.H2h.Builder.Startgg.Queries as Q
import Z.Node.H2h.Util as U
import Z.Z as Z

propN l p = l <<< Z.prop p

infixl 6 propN as <.<

getEventData :: forall x. U.GetDataFn x
getEventData = U.adaptBuilder do
  { client, slug } <- Z.xAsk
  let vars = { pageE: 0, pageS: 0, slug }
  res <- Z.xMapWE H2h.GqlW H2h.GqlE $ U.runGGQueryAll Q.tourneyData queryAllData
    vars
    client
    Z.default
  Z.xInfo res
  pure
    { id: Z.asStringOr res.event.id
    , name: res.event.name
    , slug
    , state: res.event.state
    , tournamentName: res.event.tournament.name
    , site: H2h.Startgg
    }
  where
  queryAllData = do
    U.ggQueryAll
      (Z.prop (Z.p :: Z.P "pageE"))
      (Z.prop (Z.p :: Z.P "event") <.< (Z.p :: Z.P "entrants"))
    U.ggQueryAll
      (Z.prop (Z.p :: Z.P "pageS"))
      (Z.prop (Z.p :: Z.P "event") <.< (Z.p :: Z.P "standings"))
