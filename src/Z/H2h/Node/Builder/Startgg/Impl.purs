module Z.H2h.Node.Builder.Startgg.Impl (getEventData) where

import Prelude

import Z.Z.Shorthand (_o, _o_, g_, gmOr'_, o_)
import Z as Z
import Z.Gql.Node.Module as Gql
import Z.H2h.Error as H2hE
import Z.H2h.Module as H2h
import Z.H2h.Node.Builder.API as B
import Z.H2h.Node.Builder.Startgg.All as All
import Z.H2h.Node.Builder.Startgg.Queries as Q
import Z.H2h.Warning as H2hW
import Z.Z.Barlow (__)

mapOfJsonElsWithFieldsTypeAnd_t
  :: forall @t ttype tLns ttypeLns tr' ttyper' r
   . Z.IsSymbol t
  => Z.IsSymbol ttype
  => Z.TypeEquals ttype "type"
  => Z.Cons ttype String ttyper' r
  => Z.Cons t String tr' r
  => Z.ParseSymbol t tLns
  => Z.ConstructBarlow tLns (Z.Forget String) { | r } { | r } String String
  => Z.ParseSymbol ttype ttypeLns
  => Z.ConstructBarlow ttypeLns (Z.Forget String) { | r } { | r } String String
  => Array { | r }
  -> Z.Map String String
mapOfJsonElsWithFieldsTypeAnd_t = Z.reduce reducer Z.mapEmpty
  where
  reducer m i = Z.mapSet (g_ @ttype i) (g_ @t i) m

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder $ Z.xEvalS initState do
  { slug } <- Z.xAsk
  { event } <- fetchRawEventData
  let entrantNodes = event.entrants.nodes
  Z.forM_ entrantNodes $ \entrantNode -> do
    participants <- Z.forM entrantNode.participants $ \participant -> do
      let { player } = participant
      let playerImages = gmOr'_ @"user?.images" player
      let auths = gmOr'_ @"user?.authorizations?" player
      pure
        { gamerTag: participant.gamerTag
        , prefix: participant.prefix
        , playerOrder: entrantNode.id
        , player:
            { id: Z.sOrN player.id
            , gamerTag: player.gamerTag
            , prefix: player.prefix
            , pronouns: g_ @"user?.genderPronoun" player
            , name: g_ @"user?.name" player
            , socials: mapOfJsonElsWithFieldsTypeAnd_t @"externalUsername" auths
            , images: mapOfJsonElsWithFieldsTypeAnd_t @"url" playerImages
            }
        }
    let
      entrantId = Z.sOrN entrantNode.id
      entrant =
        { id: entrantId
        , participants
        , standing: { placement: 0, isFinal: false }
        }
    Z.xOver_ @"entrants" (Z.mapSet entrantId entrant)
  Z.forM_ event.standings.nodes $ \standing -> do
    let entrantId = Z.sOrN standing.entrant.id
    Z.xSet (_o_ @"entrants" @"standing" (Z.ix entrantId))
      { placement: standing.placement, isFinal: standing.isFinal }

  let rawPgs = Z.arrSortWith (g_ @"id") event.phaseGroups
  pgs <- Z.forM rawPgs $ \pg -> Z.xPlusS @"sets" (Z.mapEmpty @Int) do
    { phaseGroup } <- fetchRawPhaseGroupData pg.id
    Z.forM_ phaseGroup.sets.nodes $ \set -> do
      let
        setId = set.id
        isDQ = set.displayScore == Z.Just "DQ"
        isBye = Z.reduce (\a s -> a || Z.isNothing s.entrant) false set.slots
        eIdA = Z.preview (Z.ix 0 # o_ @"entrant?.id") set.slots
        eIdB = Z.preview (Z.ix 1 # o_ @"entrant?.id") set.slots
        isWinA = eIdA == set.winnerId && Z.isJust set.winnerId
        winner =
          if Z.isNothing set.winnerId then Z.Nothing
          else if isWinA then Z.Just Z.Up
          else Z.Just Z.Down
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
      Z.xSet (_o @"sets" $ Z.at setId) $ Z.Just
        { id: setId
        , roundText: set.fullRoundText
        , overrideScoreText: set.displayScore
        , isDQ
        , isBye
        , winner
        , doesCount: (not isBye) && (not isDQ) && (Z.isJust set.winnerId)
        , slots: slotA Z.~ slotB
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
  let { endAt } = event.tournament
  date <- Z.xUnwrap (H2hE.InvalidInstant endAt) do
    Z.instant (Z.Milliseconds (Z.toNumber endAt)) <#> Z.toDateTime
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
        , images: mapOfJsonElsWithFieldsTypeAnd_t @"url" event.tournament.images
        , date
        }
    }
  where
  initState = { entrants: Z.mapEmpty @Z.SorN @H2h.Entrant }
  fetchRawPhaseGroupData phaseGroupId = do
    { client, networkControl } <- Z.xAsk
    let initVars = { page: 0, phaseGroupId }
    let pSpecs = [ All.ggPageSpec (__ @"page") (__ @"phaseGroup.sets") ]
    Z.xMapWE H2hW.Gql H2hE.Gql do
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
      nc <- Z.xAsk <#> \r -> Z.or r.networkControl ncOverride
      let initVars = { pageE: 0, pageS: 0, slug }
      let eSpec = All.ggPageSpec (__ @"pageE") (__ @"event.entrants")
      let sSpec = All.ggPageSpec (__ @"pageS") (__ @"event.standings")
      let pSpecs = [ eSpec, sSpec ]
      Z.xMapWE H2hW.Gql H2hE.Gql do All.ggQueryAll q initVars pSpecs client nc
