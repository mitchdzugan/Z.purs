module Z.H2h.Node.Builder.Startgg.Impl where

import Prelude

import Z as Z
import Z.Gql.Node.Module as Gql
import Z.H2h.Module as H2h
import Z.H2h.Node.Builder.API as B
import Z.H2h.Node.Builder.Startgg.All as All
import Z.H2h.Node.Builder.Startgg.Queries as Q

fetchRawEventData :: forall x. Z.X (B.BuildX x) Q.TourneyDataRes
fetchRawEventData = do
  fetchImpl Q.tourneyDataSmall (Z.xSet Gql._networkControl Gql.CacheOnly)
  where
  fetchImpl q plusEdit = do
    { client, slug, optsEdit } <- Z.xAsk
    let initVars = { pageE: 0, pageS: 0, slug }
    let eSpec = All.ggPageSpec (Z.px @"pageE") (Z.ppx @"event" @"entrants")
    let sSpec = All.ggPageSpec (Z.px @"pageS") (Z.ppx @"event" @"standings")
    let pageSpecs = [ eSpec, sSpec ]
    Z.xMapWE H2h.GqlW H2h.GqlE do
      All.ggQueryAll q initVars pageSpecs client $ optsEdit *> plusEdit

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder do
  { slug } <- Z.xAsk
  res <- fetchRawEventData
  Z.xInfo res
  pure
    { id: Z.asStringOr res.event.id
    , name: res.event.name
    , slug
    , state: res.event.state
    , tournamentName: res.event.tournament.name
    , site: H2h.Startgg
    }
