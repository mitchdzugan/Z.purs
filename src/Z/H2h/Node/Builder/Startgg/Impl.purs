module Z.H2h.Node.Builder.Startgg.Impl where

import Prelude

import Z.Gql.Node.Module as Gql
import Z.H2h.Module as H2h
import Z.H2h.Node.Builder.API as B
import Z.H2h.Node.Builder.Startgg.All as All
import Z.H2h.Node.Builder.Startgg.Queries as Q
import Z as Z

fetchRawEventData :: forall x. Z.X (B.BuildX x) Q.TourneyDataRes
fetchRawEventData = Z.xTryUntil
  (f' Q.tourneyData $ Z.xSet Gql._networkControl Gql.CacheOnly)
  [ \_ -> (f' Q.tourneyDataSmall $ Z.xSet Gql._networkControl Gql.CacheOnly)
  , \_ -> (f' Q.tourneyData Z.default)
  , \_ -> (f' Q.tourneyDataSmall Z.default)
  ]
  where
  f' q plusEdit = do
    { client, slug, optsEdit } <- Z.xAsk
    let initVars = { pageE: 0, pageS: 0, slug }
    let eSpec = All.ggPageSpec (Z.px @"pageE") (Z.ppx @"event" @"entrants")
    let sSpec = All.ggPageSpec (Z.px @"pageS") (Z.ppx @"event" @"standings")
    let pSpecs = [ eSpec, sSpec ]
    Z.xMapWE H2h.GqlW H2h.GqlE do
      All.ggQueryAll q initVars pSpecs client $ optsEdit *> plusEdit

fetchRawPhaseGroupData :: forall x. Int -> Z.X (B.BuildX x) Q.PhaseGroupDataRes
fetchRawPhaseGroupData phaseGroupId = do
  { client, optsEdit } <- Z.xAsk
  let initVars = { page: 0, phaseGroupId }
  let pSpecs = [ All.ggPageSpec (Z.px @"page") (Z.ppx @"phaseGroup" @"sets") ]
  Z.xMapWE H2h.GqlW H2h.GqlE do
    All.ggQueryAll Q.phaseGroupData initVars pSpecs client optsEdit

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder $ Z.xEvalS initState do
  { slug } <- Z.xAsk
  { event } <- fetchRawEventData
  let entrantNodes = event.entrants.nodes
  participants <- flip Z.mapM entrantNodes $ \entrantNode -> do
    pure entrantNode
  Z.forM_ event.phaseGroups $ \phaseGroup -> do
    pgData <- fetchRawPhaseGroupData phaseGroup.id
    Z.xInfo pgData
  { entrants, phaseGroups } <- Z.xGet
  pure
    { id: Z.asStringOr event.id
    , name: event.name
    , slug
    , state: event.state
    , tournamentName: event.tournament.name
    , site: H2h.Startgg
    , entrants
    , phaseGroups
    , numEntrants: Z.mapSize entrants
    }
  where
  initState =
    { phaseGroups: Z.arrEmpty @H2h.PhaseGroup
    , entrants: Z.mapEmpty @Z.StringOrNum @H2h.Entrant
    }
