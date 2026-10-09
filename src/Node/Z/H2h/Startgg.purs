module Node.Z.H2h.Startgg (getEventData) where

import Z.Prelude

import Node.Z.Gql as Gql
import Node.Z.H2h.Builder as B
import Node.Z.H2h.Startgg.All as All
import Node.Z.H2h.Startgg.Queries as Q
import Z.Gql.Error as GqlE
import Z.H2h.Error as H2hE
import Z.H2h.Module as H2h

mapOfJsonElsWithFieldsTypeAnd
  :: forall @t ttype tLns ttypeLns tr' ttyper' r
   . IsSymbol t
  => IsSymbol ttype
  => TypeEquals ttype "type"
  => Cons ttype String ttyper' r
  => Cons t (Maybe String) tr' r
  => ParseSymbol t tLns
  => ConstructBarlow'Get' tLns { | r } (Maybe String)
  => ParseSymbol ttype ttypeLns
  => ConstructBarlow'Get' ttypeLns { | r } String
  => Array { | r }
  -> HashMap String String
mapOfJsonElsWithFieldsTypeAnd els =
  sync'x $ _'exec_ @(X'HashMap String String) $ forM_ els \el ->
    case (g_ @t el) of
      (Just s) -> _'insert (g_ @ttype el) s
      Nothing -> pass

toSocialType :: String -> Maybe H2h.SocialType
toSocialType "DISCORD" = Just H2h.Discord
toSocialType "TWITCH" = Just H2h.Twitch
toSocialType "TWITTER" = Just H2h.Twitter
toSocialType "XBOX" = Just H2h.XBox
toSocialType _ = Nothing

toImageType :: String -> Maybe H2h.ImageType
toImageType "profile" = Just H2h.Profile
toImageType "banner" = Just H2h.Banner
toImageType _ = Nothing

toSocialMap :: HashMap String String -> HashMap H2h.SocialType String
toSocialMap = hm'mapSomeKeys toSocialType

toImageMap :: HashMap String String -> HashMap H2h.ImageType String
toImageMap = hm'mapSomeKeys toImageType

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder do
  x'eval_ @"entrants" @(X'HashMap SorN H2h.Entrant) do
    { slug } <- r'ask
    { event } <- fetchRawEventData
    let entrantNodes = event.entrants.nodes
    forM_ entrantNodes $ \entrantNode -> do
      participants <- forM entrantNode.participants $ \participant -> do
        let { player } = participant
        let playerImages = player # gmOr'_ @"user?.images"
        let auths = player # gmOr'_ @"user?.authorizations?"
        pure
          { gamerTag: participant.gamerTag
          , prefix: may'remove_ $ participant.prefix
          , playerOrder: entrantNode.id
          , player:
              { id: sOrN player.id
              , gamerTag: player.gamerTag
              , prefix: may'remove_ $ player.prefix
              , pronouns: may'remove_ $ player # g_ @"user?.genderPronoun"
              , name: may'remove_ $ player # g_ @"user?.name"
              , socials: toSocialMap $
                  mapOfJsonElsWithFieldsTypeAnd @"externalUsername" auths
              , images: toImageMap $
                  mapOfJsonElsWithFieldsTypeAnd @"url" playerImages
              }
          }
      let emptyEntrantErr = H2hE.EmptyEntrant entrantNode.id
      participant <- e'unwrap emptyEntrantErr $ nth participants 0
      let
        entrantId = sOrN entrantNode.id
        entrant =
          { id: entrantId
          , participants
          , participant
          , standing: { placement: 0, isFinal: false }
          }
      x'insert @"entrants" entrantId entrant
    forM_ event.standings.nodes $ \standing -> do
      let entrantId = sOrN standing.entrant.id
      x'modify @"entrants" entrantId $ rec'set @"standing"
        { placement: standing.placement, isFinal: standing.isFinal }

    let rawPgs = arr'sortWith (g_ @"id") event.phaseGroups
    pgs <- forM rawPgs \pg -> x'eval_ @"sets" @(X'HashMap SorN H2h.H2hSet) do
      pgRes <- fetchRawPhaseGroupData pg.id
      forM_ (g_ @"phaseGroup!.sets.nodes" pgRes) $ \set -> do
        let
          setId = set.id
          isDQ = set.displayScore == Just "DQ"
          isBye = reduce (\a s -> a || isNothing s.entrant) false set.slots
          eIdA = preview (ix 0 # o_ @"entrant?.id") set.slots
          eIdB = preview (ix 1 # o_ @"entrant?.id") set.slots
          isComplete = isJust set.winnerId
          isWinA = eIdA == set.winnerId && isComplete
          winner =
            if isNothing set.winnerId then Nothing
            else if isWinA then Just Pos
            else Just Neg
        let games = orDefault set.games
        slotScoreA /\ slotScoreB <- x'withReturn \xReturn -> do
          let winnerIds = games <#> _.winnerId
          let doneGames = arr'size $ arr'filter isJust winnerIds
          when (arr'size games == doneGames && doneGames > 0) do
            let w1Games = arr'size $ arr'filter (eq eIdA) winnerIds
            let w2Games = doneGames - w1Games
            xReturn $ H2h.mkScoreCount w1Games /\ H2h.mkScoreCount w2Games
          whenJust set.displayScore $ \displayScore -> do
            when (displayScore == "DQ") do
              xReturn $ H2h.mkScoreDQ isWinA /\ H2h.mkScoreDQ (not isWinA)
            flip whenJust xReturn $ hush $ runParser displayScore do
              parseAnyTill_ do
                parseString_ " "
                scoreA <- H2h.mkScoreCount <$> parseInt <* parseString_ " -"
                scoreB <- parseAnyTill_ do
                  parseString_ " "
                  H2h.mkScoreCount <$> parseInt <* parseEof
                pure $ scoreA /\ scoreB
          let
            completeScores = H2h.mkScoreWL isWinA /\ H2h.mkScoreWL
              (not isWinA)
          pure
            if isComplete then completeScores else H2h.NoScore /\ H2h.NoScore
        let slotA = { entrantId: eIdA <#> sOrN, score: slotScoreA }
        let slotB = { entrantId: eIdB <#> sOrN, score: slotScoreB }
        let
          toH2hGame game =
            { winnerId: game.winnerId
            , orderNum: game.orderNum
            , entrant1Score: game.entrant1Score
            , entrant2Score: game.entrant2Score
            , selections:
                sync'x $ _'exec_ @(X'HashMap PairKey H2h.H2hGameSelection) do
                  forM_ (fromMaybe [] game.selections) \selection -> do
                    let
                      maybePairKey =
                        if Just selection.entrant.id == eIdA then Just Pos
                        else if Just selection.entrant.id == eIdB then Just Neg
                        else Nothing
                    whenJust maybePairKey \pairKey -> do
                      _'insert pairKey
                        { orderNum: selection.orderNum
                        , selectionValue: selection.selectionValue
                        , character: selection.character
                        }
            }
        x'insert @"sets" (sOrN setId)
          { id: sOrN setId
          , roundText: set.fullRoundText
          , overrideScoreText: set.displayScore
          , isDQ
          , isBye
          , winner
          , doesCount: (not isBye) && (not isDQ) && (isJust set.winnerId)
          , slots: slotA ~ slotB
          , eventOrder: setId
          , games: games <#> toH2hGame
          }
      sets <- x'extract @"sets"
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
    entrants <- x'extract @"entrants"
    let { endAt } = event.tournament
    let endAtMS = (toNumber endAt) * 1000.0
    date <- e'unwrap (H2hE.InvalidInstant endAt) $ dateTime'fromMS endAtMS
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
          , images: toImageMap $
              mapOfJsonElsWithFieldsTypeAnd @"url" event.tournament.images
          , date
          }
      }
  where
  fetchRawPhaseGroupData phaseGroupId = do
    { client, networkControl } <- r'ask
    let initVars = { page: 0, phaseGroupId }
    let pSpecs = [ All.ggPageSpec (__ @"page") (__ @"phaseGroup!.sets") ]
    All.ggQueryAll Q.phaseGroup initVars pSpecs client networkControl
  fetchRawEventData = do
    e'try fetchRawEventDataImpl >>= case _ of
      Left fullE@(H2hE.Gql (GqlE.ResponseError e)) -> do
        case decodeJson @Q.EventDataRes_ e.response.data of
          Left _ -> e'fail fullE
          Right res -> pure
            { event: rec'merge res.event
                { standings: { nodes: [], pageInfo: { total: 0 } }
                , entrants: { nodes: [], pageInfo: { total: 0 } }
                }
            }
      Left fullE -> e'fail fullE
      Right r -> pure r
  fetchRawEventDataImpl = e'tryUntil
    (f' Q.eventMaxDataPerReq $ Just Gql.CacheOnly)
    [ const (f' Q.evenMinComplexityPerReq $ Just Gql.CacheOnly)
    , const (f' Q.eventMaxDataPerReq Nothing)
    , const (f' Q.evenMinComplexityPerReq Nothing)
    ]
    where
    f' q ncOverride = do
      { client, slug } <- r'ask
      nc <- r'ask <#> \r -> jOr r.networkControl ncOverride
      let initVars = { pageE: 0, pageS: 0, slug }
      let eSpec = All.ggPageSpec (__ @"pageE") (__ @"event.entrants")
      let sSpec = All.ggPageSpec (__ @"pageS") (__ @"event.standings")
      let pSpecs = [ eSpec, sSpec ]
      All.ggQueryAll q initVars pSpecs client nc
