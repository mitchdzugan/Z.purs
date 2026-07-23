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
  reducer m i = Z.mapSet (Z.view (Z.lpt @lt) i) (Z.view (Z.lpt @l) i) m

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder $ Z.xEvalS initState do
  { slug } <- Z.xAsk
  { event } <- fetchRawEventData
  let entrantNodes = event.entrants.nodes
  Z.forM_ entrantNodes $ \entrantNode -> do
    participants <- Z.forM entrantNode.participants $ \participant -> do
      let { player } = participant
      let playerImages = Z.orDefault $ Z.firstOf (Z.l @"user?.images") player
      let auths = Z.orDefault $ Z.firstOf (Z.l @"user?.authorizations?") player
      pure
        { gamerTag: participant.gamerTag
        , prefix: participant.prefix
        , playerOrder: entrantNode.id
        , player:
            { id: Z.sOrN player.id
            , gamerTag: player.gamerTag
            , prefix: player.prefix
            , pronouns: Z.view (Z.l @"user?.genderPronoun") player
            , name: Z.view (Z.l @"user?.name") player
            , socials: mapOfPairsWithType @"externalUsername" auths
            , images: mapOfPairsWithType @"url" playerImages
            }
        }
    let
      entrantId = Z.sOrN entrantNode.id
      entrant =
        { id: entrantId
        , participants
        , standing: { placement: 0, isFinal: false }
        }
    Z.xOver (Z.l @"entrants") (Z.mapSet entrantId entrant)
  Z.forM_ event.standings.nodes $ \standing -> do
    let entrantId = Z.sOrN standing.entrant.id
    Z.xSet (Z.l @"entrants" <<< (Z.ix entrantId) <<< Z.l @"standing")
      { placement: standing.placement, isFinal: standing.isFinal }

  let rawPgs = Z.arrSortWith (Z.view $ Z.l @"id") event.phaseGroups
  pgs <- Z.forM rawPgs $ \pg -> Z.xPlusS @"sets" (Z.mapEmpty @Z.SorN) do
    { phaseGroup } <- fetchRawPhaseGroupData pg.id
    Z.forM_ phaseGroup.sets.nodes $ \set -> do
      let setId = Z.sOrN set.id
      let isDQ = set.displayScore == Z.Just "DQ"
      let isBye = Z.reduce (\a s -> a || Z.isNothing s.entrant) false set.slots
      let eIdA = Z.firstOfOn set.slots (Z.ix 0 <<< Z.l @"entrant?.id")
      let eIdB = Z.firstOfOn set.slots (Z.ix 1 <<< Z.l @"entrant?.id")
      let isWinA = eIdA == set.winnerId && Z.isJust set.winnerId
      slotScoreA Z./\ slotScoreB <- Z.xWithRet do
        let games = Z.orDefault set.games
        let winnerIds = games <#> \g -> g.winnerId
        let doneGames = Z.arrSize $ Z.arrFilter Z.isJust winnerIds
        when (Z.arrSize games == doneGames && doneGames > 0) do
          let w1Games = Z.arrSize $ Z.arrFilter (eq eIdA) winnerIds
          let w2Games = doneGames - w1Games
          Z.xReturn $ H2h.mkScoreCount w1Games Z./\ H2h.mkScoreCount w2Games
        Z.whenJust set.displayScore $ \displayScore -> do
          when (displayScore == "DQ") do
            Z.xReturn $ H2h.mkScoreDQ isWinA Z./\ H2h.mkScoreDQ (not isWinA)
          Z.xLogWarning { warn: "UNMADE SCORES", displayScore }
        pure $ H2h.NoScore Z./\ H2h.NoScore
      let slotA = { entrantId: eIdA <#> Z.sOrN, score: slotScoreA }
      let slotB = { entrantId: eIdB <#> Z.sOrN, score: slotScoreB }
      Z.xSet (Z.l @"sets" <<< Z.at setId) $ Z.Just
        { id: setId
        , fullRoundText: set.fullRoundText
        , isDQ
        , isBye
        , displayScore: set.displayScore
        , winnerId: set.winnerId <#> Z.sOrN
        , doesCount: true -- TODO
        , isLosers: true -- TODO
        , isDropRound: true -- TODO
        , isGrands: true -- TODO
        , depth: 0 -- TODO
        , slots: slotA Z./\ slotB
        }
    { sets } <- Z.xGet
    pure
      { id: Z.sOrN pg.id
      , displayIdentifier: pg.displayIdentifier
      , sets
      , phase:
          { id: Z.sOrN pg.phase.id
          , name: pg.phase.name
          , phaseOrder: pg.phase.phaseOrder
          }
      }
  Z.xInfo pgs
  { entrants } <- Z.xGet
  pure
    { id: Z.sOrN event.id
    , name: event.name
    , slug
    , state: event.state
    , site: H2h.Startgg
    , entrants
    , phaseGroups: pgs
    , tournament:
        { id: Z.sOrN event.tournament.id
        , name: event.tournament.name
        , images: mapOfPairsWithType @"url" event.tournament.images
        , endAt: event.tournament.endAt
        }
    }
  where
  initState = { entrants: Z.mapEmpty @Z.SorN @H2h.Entrant }
  fetchRawPhaseGroupData phaseGroupId = do
    { client, networkControl } <- Z.xAsk
    let initVars = { page: 0, phaseGroupId }
    let pSpecs = [ All.ggPageSpec (Z.l @"page") (Z.l @"phaseGroup.sets") ]
    Z.xMapWE H2h.GqlW H2h.GqlE do
      All.ggQueryAll Q.phaseGroup initVars pSpecs client networkControl
  fetchRawEventData = Z.xTryUntil
    (f' Q.eventMaxDataPerReq $ Z.Just Gql.CacheOnly)
    [ const (f' Q.evenMinComplexityPerReq $ Z.Just Gql.CacheOnly)
    , const (f' Q.eventMaxDataPerReq Z.Nothing)
    , const (f' Q.evenMinComplexityPerReq Z.Nothing)
    ]
    where
    f' q ncOverride = do
      { client, slug } <- Z.xAsk
      nc <- Z.xAsk <#> \r -> Z.fromMaybe r.networkControl ncOverride
      let initVars = { pageE: 0, pageS: 0, slug }
      let eSpec = All.ggPageSpec (Z.l @"pageE") (Z.l @"event.entrants")
      let sSpec = All.ggPageSpec (Z.l @"pageS") (Z.l @"event.standings")
      let pSpecs = [ eSpec, sSpec ]
      Z.xMapWE H2h.GqlW H2h.GqlE do All.ggQueryAll q initVars pSpecs client nc
