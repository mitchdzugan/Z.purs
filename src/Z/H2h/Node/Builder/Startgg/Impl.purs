module Z.H2h.Node.Builder.Startgg.Impl where

import Prelude

import Z.H2h.Module as H2h
import Z.H2h.Node.Builder.Startgg.Queries as Q
import Z.H2h.Node.Builder.API as B
import Z.H2h.Node.Builder.Startgg.All as All
import Z as Z

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder do
  { client, slug, optsEdit } <- Z.xAsk
  let initVars = { pageE: 0, pageS: 0, slug }
  let eSpec = All.ggPageSpec (Z.px @"pageE") (Z.ppx @"event" @"entrants")
  let sSpec = All.ggPageSpec (Z.px @"pageS") (Z.ppx @"event" @"standings")
  let pageSpecs = [ eSpec, sSpec ]
  res <- Z.xMapWE H2h.GqlW H2h.GqlE do
    All.ggQueryAll Q.tourneyData initVars pageSpecs client optsEdit
  Z.xInfo res
  pure
    { id: Z.asStringOr res.event.id
    , name: res.event.name
    , slug
    , state: res.event.state
    , tournamentName: res.event.tournament.name
    , site: H2h.Startgg
    }

