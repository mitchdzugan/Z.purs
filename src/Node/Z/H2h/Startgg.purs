module Node.Z.H2h.Startgg (getEventData) where

import Z.Prelude
import Z.H2h.Error as H2hE
import Z.H2h.Module as H2h
import Z.H2h.Warning as H2hW
import Node.Z.Gql as Gql
import Node.Z.H2h.Builder as B
import Node.Z.H2h.Startgg.All as All
import Node.Z.H2h.Startgg.Queries as Q

mapOfJsonElsWithFieldsTypeAnd_t
  :: forall @t ttype tLns ttypeLns tr' ttyper' r
   . IsSymbol t
  => IsSymbol ttype
  => TypeEquals ttype "type"
  => Cons ttype String ttyper' r
  => Cons t String tr' r
  => ParseSymbol t tLns
  => ConstructBarlow tLns (Forget String) { | r } { | r } String String
  => ParseSymbol ttype ttypeLns
  => ConstructBarlow ttypeLns (Forget String) { | r } { | r } String String
  => Array { | r }
  -> Map String String
mapOfJsonElsWithFieldsTypeAnd_t = reduce reducer mapEmpty
  where
  reducer m i = mapSet (g_ @ttype i) (g_ @t i) m

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder $ x EvalS initState do
  { slug } <- x AtR
  { event } <- fetchRawEventData
  let entrantNodes = event.entrants.nodes
  forM_ entrantNodes $ \entrantNode -> do
    participants <- forM entrantNode.participants $ \participant -> do
      let { player } = participant
      let playerImages = player # gmOr'_ @"user?.images"
      let auths = player # gmOr'_ @"user?.authorizations?"
      pure
        { gamerTag: participant.gamerTag
        , prefix: participant.prefix
        , playerOrder: entrantNode.id
        , player:
            { id: sOrN player.id
            , gamerTag: player.gamerTag
            , prefix: player.prefix
            , pronouns: player # g_ @"user?.genderPronoun"
            , name: player # g_ @"user?.name"
            , socials: mapOfJsonElsWithFieldsTypeAnd_t @"externalUsername" auths
            , images: mapOfJsonElsWithFieldsTypeAnd_t @"url" playerImages
            }
        }
    let
      entrantId = sOrN entrantNode.id
      entrant =
        { id: entrantId
        , participants
        , standing: { placement: 0, isFinal: false }
        }
    x (Over_ @"entrants") $ mapSet entrantId entrant
  forM_ event.standings.nodes $ \standing -> do
    let entrantId = sOrN standing.entrant.id
    x Set (_o_ @"entrants" @"standing" (ix entrantId))
      { placement: standing.placement, isFinal: standing.isFinal }

  let rawPgs = arrSortWith (g_ @"id") event.phaseGroups
  pgs <- forM rawPgs $ \pg -> x (PlusS @"sets") (mapEmpty @Int) do
    { phaseGroup } <- fetchRawPhaseGroupData pg.id
    forM_ phaseGroup.sets.nodes $ \set -> do
      let
        setId = set.id
        isDQ = set.displayScore == Just "DQ"
        isBye = reduce (\a s -> a || isNothing s.entrant) false set.slots
        eIdA = preview (ix 0 # o_ @"entrant?.id") set.slots
        eIdB = preview (ix 1 # o_ @"entrant?.id") set.slots
        isWinA = eIdA == set.winnerId && isJust set.winnerId
        winner =
          if isNothing set.winnerId then Nothing
          else if isWinA then Just Pos
          else Just Neg
      slotScoreA /\ slotScoreB <- x WithRet do
        let games = orDefault set.games
        let winnerIds = games <#> _.winnerId
        let doneGames = arrSize $ arrFilter isJust winnerIds
        when (arrSize games == doneGames && doneGames > 0) do
          let w1Games = arrSize $ arrFilter (eq eIdA) winnerIds
          let w2Games = doneGames - w1Games
          x Return $ H2h.mkScoreCount w1Games /\ H2h.mkScoreCount w2Games
        whenJust set.displayScore $ \displayScore -> do
          when (displayScore == "DQ") do
            x Return $ H2h.mkScoreDQ isWinA /\ H2h.mkScoreDQ (not isWinA)
          xLogWarning { warn: "UNMADE SCORES", displayScore }
        pure $ H2h.NoScore /\ H2h.NoScore
      let slotA = { entrantId: eIdA <#> sOrN, score: slotScoreA }
      let slotB = { entrantId: eIdB <#> sOrN, score: slotScoreB }
      x Set (_o @"sets" $ at setId) $ Just
        { id: setId
        , roundText: set.fullRoundText
        , overrideScoreText: set.displayScore
        , isDQ
        , isBye
        , winner
        , doesCount: (not isBye) && (not isDQ) && (isJust set.winnerId)
        , slots: slotA ~ slotB
        }
    { sets } <- x AtS
    pure
      { id: sOrN pg.id
      , displayIdentifier: pg.displayIdentifier
      , sets
      , phase:
          { id: sOrN pg.phase.id
          , name: pg.phase.name
          , phaseOrder: pg.phase.phaseOrder
          }
      }
  { entrants } <- x AtS
  let { endAt } = event.tournament
  date <- xUnwrap (H2hE.InvalidInstant endAt) do
    instant (Milliseconds (toNumber endAt)) <#> toDateTime
  pure
    { id: sOrN event.id
    , name: event.name
    , slug
    , state: event.state
    , site: H2h.Startgg
    , entrants
    , phaseGroups: pgs
    , tournament:
        { id: sOrN event.tournament.id
        , name: event.tournament.name
        , images: mapOfJsonElsWithFieldsTypeAnd_t @"url" event.tournament.images
        , date
        }
    }
  where
  initState = { entrants: mapEmpty @SorN @H2h.Entrant }
  fetchRawPhaseGroupData phaseGroupId = do
    { client, networkControl } <- x AtR
    let initVars = { page: 0, phaseGroupId }
    let pSpecs = [ All.ggPageSpec (__ @"page") (__ @"phaseGroup.sets") ]
    xMapWE H2hW.Gql H2hE.Gql do
      All.ggQueryAll Q.phaseGroup initVars pSpecs client networkControl
  fetchRawEventData = x TryUntil
    (f' Q.eventMaxDataPerReq $ Just Gql.CacheOnly)
    [ const (f' Q.evenMinComplexityPerReq $ Just Gql.CacheOnly)
    , const (f' Q.eventMaxDataPerReq Nothing)
    , const (f' Q.evenMinComplexityPerReq Nothing)
    ]
    where
    f' q ncOverride = do
      { client, slug } <- x AtR
      nc <- x AtR <#> \r -> jOr r.networkControl ncOverride
      let initVars = { pageE: 0, pageS: 0, slug }
      let eSpec = All.ggPageSpec (__ @"pageE") (__ @"event.entrants")
      let sSpec = All.ggPageSpec (__ @"pageS") (__ @"event.standings")
      let pSpecs = [ eSpec, sSpec ]
      xMapWE H2hW.Gql H2hE.Gql do All.ggQueryAll q initVars pSpecs client nc
