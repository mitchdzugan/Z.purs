module Z.H2h.Node.Builder.Startgg.Impl (getEventData) where

import Prelude

import Z as Z
import Z.Gql.Node.Module as Gql
import Z.H2h.Module as H2h
import Z.H2h.Node.Builder.API as B
import Z.H2h.Node.Builder.Startgg.All as All
import Z.H2h.Node.Builder.Startgg.Queries as Q

mapOfPairsWithType
  :: forall @l lt r'' r' r
   . Z.IsSymbol l
  => Z.IsSymbol lt
  => Z.TypeEquals lt "type"
  => Z.Cons lt String r'' r
  => Z.Cons l String r' r
  => Array { | r }
  -> Z.Map String String
mapOfPairsWithType = Z.reduce reducer Z.mapEmpty
  where
  reducer m i = Z.mapSet (Z.view (Z.px @lt) i) (Z.view (Z.px @l) i) m

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder $ Z.xEvalS initState do
  { slug } <- Z.xAsk
  { event } <- fetchRawEventData
  let
    entrantNodes = event.entrants.nodes
  Z.forM_ entrantNodes $ \entrantNode -> do
    participants <- Z.forM entrantNode.participants $ \participant -> do
      let { player } = participant
      let playerImages = Z.orDefault $ player.user <#> Z.view (Z.px @"images")
      let auths = Z.orDefault $ player.user >>= Z.view (Z.px @"authorizations")
      pure
        { gamerTag: participant.gamerTag
        , prefix: participant.prefix
        , playerOrder: entrantNode.id
        , player:
            { id: Z.asStringOr player.id
            , gamerTag: player.gamerTag
            , prefix: player.prefix
            , pronouns: player.user >>= Z.view (Z.px @"genderPronoun")
            , name: player.user >>= Z.view (Z.px @"name")
            , socials: mapOfPairsWithType @"externalUsername" auths
            , images: mapOfPairsWithType @"url" playerImages
            }
        }
    let
      entrantId = Z.asStringOr entrantNode.id
      entrant =
        { id: entrantId
        , participants
        , standing: { placement: 0, isFinal: false }
        }
    Z.xOver (Z.px @"entrants") (Z.mapSet entrantId entrant)
  Z.forM_ event.standings.nodes $ \standing -> do
    let entrantId = Z.asStringOr standing.entrant.id
    Z.xSet (Z.px @"entrants" <<< (Z.ix entrantId) <<< Z.px @"standing")
      { placement: standing.placement, isFinal: standing.isFinal }

  let pgs = Z.arrSortWith (Z.view $ Z.px @"id") event.phaseGroups
  Z.forM_ pgs $ \pg -> Z.xPlusS @"sets" (Z.mapEmpty @Z.StringOrNum) do
    { phaseGroup } <- fetchRawPhaseGroupData pg.id
    Z.forM_ phaseGroup.sets.nodes $ \set -> do
      let isDQ = set.displayScore == Z.Just "DQ"
      let isBye = Z.reduce (\a s -> a || Z.isNothing s.entrant) false set.slots
      slotScoreA Z./\ slotScoreB <- Z.xWithRet do
        let games = Z.orDefault set.games
        when (Z.arrSize games > 0) do
          Z.xReturn $ "asdf" Z./\ "qwer"
        pure $ "" Z./\ ""
      Z.xInfo { isDQ, isBye }

  { entrants, phaseGroups } <- Z.xGet
  -- Z.xInfo $ Z.arrFromFoldable entrants
  pure
    { id: Z.asStringOr event.id
    , name: event.name
    , slug
    , state: event.state
    , site: H2h.Startgg
    , entrants
    , phaseGroups
    , tournament:
        { id: Z.asStringOr event.tournament.id
        , name: event.tournament.name
        , images: mapOfPairsWithType @"url" event.tournament.images
        , endAt: event.tournament.endAt
        }
    }
  where
  initState =
    { phaseGroups: Z.arrEmpty @H2h.PhaseGroup
    , entrants: Z.mapEmpty @Z.StringOrNum @H2h.Entrant
    }
  fetchRawPhaseGroupData phaseGroupId = do
    { client, networkControl } <- Z.xAsk
    let initVars = { page: 0, phaseGroupId }
    let pSpecs = [ All.ggPageSpec (Z.px @"page") (Z.ppx @"phaseGroup" @"sets") ]
    Z.xMapWE H2h.GqlW H2h.GqlE do
      All.ggQueryAll Q.phaseGroup initVars pSpecs client networkControl
  fetchRawEventData = Z.xTryUntil
    (f' Q.event $ Z.Just Gql.CacheOnly)
    [ const (f' Q.eventSmall $ Z.Just Gql.CacheOnly)
    , const (f' Q.event Z.Nothing)
    , const (f' Q.eventSmall Z.Nothing)
    ]
    where
    f' q override = do
      { client, slug, networkControl } <- Z.xAsk
      let initVars = { pageE: 0, pageS: 0, slug }
      let eSpec = All.ggPageSpec (Z.px @"pageE") (Z.ppx @"event" @"entrants")
      let sSpec = All.ggPageSpec (Z.px @"pageS") (Z.ppx @"event" @"standings")
      let pSpecs = [ eSpec, sSpec ]
      let nc = Z.fromMaybe networkControl override
      Z.xMapWE H2h.GqlW H2h.GqlE $ All.ggQueryAll q initVars pSpecs client nc
