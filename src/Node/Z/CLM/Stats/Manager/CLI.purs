module Node.Z.CLM.Stats.Manager.CLI where

import Node.Z.Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Node.Z.CLM.Stats.Manager.Action as Act
import Node.Z.CLM.Stats.Manager.Error as ClmStE
import Node.Z.CLM.Stats.Manager.Legacy (CLMStatsLegacyBlob)
import Node.Z.CLM.Stats.Manager.Spec as Spec
import Node.Z.CLM.Stats.Manager.Warning as ClmStW
import Node.Z.CLM.Stats.Queries as Q
import Node.Z.Gql as Gql
import Node.Z.H2h as H2h
import Node.Z.H2h.Startgg.All as All

wrapH2hWE
  :: forall x a
   . Run (WaE' H2h.Warning H2h.Error $ WaE' ClmStW.T ClmStE.T x) a
  -> Run (WaE' ClmStW.T ClmStE.T x) a
wrapH2hWE = we'map ClmStW.H2h ClmStE.H2h

type EnvR r =
  { isDevEnv :: Boolean
  , ggAuth :: String
  , dataRoot :: String
  , appCOPath :: String
  , pagesCOPath :: String
  , client :: Gql.Client
  , legacyBlob :: CLMStatsLegacyBlob
  | r
  }

type ClmSeason'Event =
  { eventName :: String
  , numEntrants :: Int
  , date :: DateTime
  , slug :: String
  , prEligible :: Boolean
  , tournamentName :: String
  , imageUrl :: String
  , eventId :: SorN
  }

type ClmSeason'Player'Char =
  { charId :: Int
  , color :: Int
  , usage :: Number
  }

type ClmSeason'Player =
  { clmId :: Int
  , rating :: Maybe Int
  , clmRank :: Maybe Int
  , wins :: Int
  , losses :: Int
  , prEvents :: Int
  , lastEvent :: { id :: SorN, placing :: Int }
  , image :: Maybe String
  , socials :: HashMap H2h.SocialType String
  , gamerTag :: String
  , prefix :: Maybe String
  , realName :: Maybe String
  , pronouns :: Maybe String
  , otherTags :: HashSet String
  , otherSeasons :: HashSet Int
  , chars :: Array ClmSeason'Player'Char
  }

type ClmSeason =
  { seasonId :: Int
  , title :: String
  , isAll :: Boolean
  , ranks :: Array ClmSeason'Player
  , events :: HashMap SorN ClmSeason'Event
  }

type ClmPlayer'Season'Set =
  { id :: SorN
  , eventId :: SorN
  , won :: Boolean
  , dq :: Boolean
  , round :: String
  , wonGames :: String
  , lostGames :: String
  , opponentName :: Maybe String
  , winnerName :: Maybe String
  , loserName :: Maybe String
  }

type ClmPlayer'Season'Event =
  { eventId :: SorN
  , placingString :: String
  , setIds :: Array SorN
  , numWins :: Int
  , numLosses :: Int
  , losses :: Array String
  , didDQ :: Boolean
  }

type ClmPlayer'Season'H2h =
  { opponent :: Either String Int
  , setIds :: Array SorN
  }

type ClmPlayer'Season =
  { sets :: HashMap SorN ClmPlayer'Season'Set
  , events :: Array ClmPlayer'Season'Event
  , h2hs :: Array ClmPlayer'Season'H2h
  }

type ClmV r x =
  ( RWaEA' (EnvR r) ClmStW.T ClmStE.T
      ( seasons :: X'HashMap Int ClmSeason
      , playerSeasons :: X'HashMap2D Int Int ClmPlayer'Season
      , seasonEvents :: X'HashMap2D Int SorN ClmSeason'Event
      , nextClmId :: X'Ref Int
      , playerIdsByClmId :: X'HashSet2D Int SorN
      , clmIdByPlayerId :: X'HashMap SorN Int
      | x
      )
  )

type PureActions = Array Act.PureAction

type ActionData = { manual :: PureActions, auto :: PureActions }

type AutoActionDef =
  (Parser String Unit) /\ (Parser String Unit) /\ (String -> Act.PureAction)

defAutoAction
  :: forall s1 s2
   . Parser String s1
  -> Parser String s2
  -> (String -> Act.Action ())
  -> AutoActionDef
defAutoAction p1 p2 f =
  (p1 <#> const unit) /\ (p2 <#> const unit) /\ (Act.PureAction <<< f)

autoActionDefs :: Array AutoActionDef
autoActionDefs =
  [ defAutoAction parseRest parseRest \slug -> Act.AddEvent { slug }
  , defAutoAction (parseAnyAroundString "bunker") (parseAnyAroundString "crazy")
      \slug -> Act.SetIsPrEligible { slug, isEligible: false }
  ]

initialManual :: PureActions
initialManual = map Act.PureAction
  [ Act.AddEvent { slug: "fgzs2x09" }
  , Act.AddEvent $ ggD "melee-at-my-house-1-the-first-one-3" "melee-singles" {}
  , Act.AddEvent $ ggD "melee-at-my-house-2-the-one-for-emmas-birthday"
      "melee-singles"
      {}
  , Act.AddEvent $ ggD "the-botlane-show-5-yolk-grunker" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-6-jess-dang3r" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-7-kadence" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-8-dz" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-9-unsure" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-10-jair-the-creator" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-11-jisp" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-12-fluid-lucinasd" "melee-singles" {}
  , Act.MarkChallonge { slug: "fgzs2x09" }
  , Act.MarkDoneUpdating $ ggD "the-bunker-11" "crazy-doubles" {}
  , Act.MarkDoneUpdating $ ggD "the-bunker-12" "crazy-doubles" {}
  , Act.MarkDoneUpdating $ ggD "the-bunker-13" "crazy-doubles" {}
  , Act.MarkDoneUpdating $ ggD "the-bunker-14-ft-everyone" "crazy-doubles" {}
  , Act.MarkDoneUpdating $ ggD "heat-check-uic-4" "melee-singles" {}
  , Act.MarkDoneUpdating $ ggD "vertex-1-1" "melee-singles" {}
  , Act.MarkDoneUpdating $ ggD "fight-night-uic-21" "melee-singles" {}
  , Act.MarkDoneUpdating $ ggD lastFudds "melee-singles" {}
  , Act.MarkDoneUpdating $ ggD lastFudds "melee-amateur-bracket" {}
  , Act.SetClmId { clmId: 993, playerSorN: "4253823" }
  , Act.SetClmId { clmId: 1714, playerSorN: "5211563" }
  , Act.SetClmId { clmId: 891, playerSorN: "13423" }
  ]
  where
  lastFudds = "fudds-house-17-last-fudds"
  ggD t e r = rec'insert @"slug" ("tournament/" <> t <> "/event/" <> e) r

getActions :: forall r x. PureActions -> Boolean -> ClmV r x @@> ActionData
getActions newActions usePrevAuto = do
  buildDataPath <- r'ask <#> \r -> r.pagesCOPath /./ "build.json"
  baseRes <- x'decodeTextFile @ActionData buildDataPath # e'try <#> case _ of
    Left _ -> { manual: initialManual <> newActions, auto: [] }
    Right d -> { manual: d.manual <> newActions, auto: d.auto }
  if usePrevAuto then pure baseRes
  else do
    { client } <- r'ask
    before <- x'nowMS'number <#>
      \n -> (60 * 60 * 24 * floor (n / 1000.0 / 24.0 / 60.0 / 60.0)) +
        (24 * 60 * 60)
    let after = 1767225600
    let pSpecs = [ All.ggPageSpec (__ @"page") (__ @"tournaments") ]
    let initVars = { after, before, page: 0 }
    clmEvents <- wrapH2hWE $
      All.ggQueryAll Q.clmEvents initVars pSpecs client Gql.CacheFirst
    let slugs = clmEvents.tournaments.nodes <#> _.events # arr'concat <#> _.slug
    let
      newAuto = slugs >>= \slug -> do
        let pieces = str'split (Pattern "/") slug
        case pieces of
          [ "tournament", tslug, "event", eslug ] -> do
            tparse /\ eparse /\ mkAction <- autoActionDefs
            case (runParser tslug tparse /\ runParser eslug eparse) of
              (Right _ /\ Right _) -> [ mkAction slug ]
              _ -> []
          _ -> []
    pure $ rec'set @"auto" newAuto baseRes

seasonIdByDate :: DateTime -> Int
seasonIdByDate dt = seasonYearContrib + seasonMonthContrib
  where
  seasonYearContrib = 2 + (3 * (dateTime'year dt - 2022))
  seasonMonthContrib = dateTime'month'i0 dt / 4

getH2hData
  :: forall r x
   . Spec.Spec
  -> Boolean
  -> ClmV r x @@> Array (Int /\ HashMap SorN H2h.Event)
getH2hData spec allowRefetch = x'eval_ @"seasonH2hEvents"
  @(X'HashMap2D Int SorN H2h.Event)
  do
    { client } <- r'ask
    x'info { numEvents: hs'size spec.eventSlugs }
    forM_ (hs'vals spec.eventSlugs) \slug -> do
      let
        isChallonge = hs'has slug spec.challongeSlugs
        sourceFn =
          if isChallonge then H2h.challongeSource else H2h.startggSource
        source = sourceFn slug
        isDone = hs'has slug spec.doneUpdating
        needsRefetch = hs'has slug spec.eventsToRefetch
        wantsRefetch = not isDone || needsRefetch
        canRefetchEvent = allowRefetch && wantsRefetch
        networkControl = case (isDone /\ needsRefetch) of
          (true /\ _) -> Gql.CacheOnly
          (_ /\ true) -> Gql.ForceFetch
          _ -> default
        getEventData nc =
          wrapH2hWE $ we'unresult =<< H2h.getEventData source client nc
      eventData' <- getEventData networkControl
      let
        shouldRefetchEvent = eventData'.state /= "COMPLETED" && canRefetchEvent
      eventData <-
        if shouldRefetchEvent then getEventData Gql.ForceFetch
        else pure eventData'
      let seasonId = seasonIdByDate eventData.tournament.date
      x'insert @"seasonH2hEvents" (seasonId /\ eventData.id) eventData
    x'd1entries @"seasonH2hEvents" <#> arr'sortWith fst

type XSeason x =
  ( clmIdByEntrantId :: X'HashMap SorN Int
  , wins :: X'HashMap Int Int
  , losses :: X'HashMap Int Int
  , lastEventId :: X'HashMap Int SorN
  , prEventIds :: X'HashSet2D Int SorN
  , chars :: X'HashMap Int (Array ClmSeason'Player'Char)
  , ratingData ::
      X'HashMap Int
        { raw :: Number
        , hasAny :: Boolean
        , hasConfidence :: Boolean
        , prEvents :: Int
        , clmId :: Int
        , participant :: H2h.Participant
        }
  , eventPlacing :: X'HashMap (Int /\ SorN) Int
  , eventWins :: X'HashMap (Int /\ SorN) Int
  , eventLosses :: X'HashMap (Int /\ SorN) Int
  , eventSetIds :: X'HashSet2D (Int /\ SorN) SorN
  , eventBeaters :: X'HashSet2D (Int /\ SorN) Int
  , eventDidDQ :: X'HashMap (Int /\ SorN) Boolean
  , h2hOpps :: X'HashSet2D Int Int
  , h2hSetIds :: X'HashSet2D (Int /\ Int) SorN
  , eventIdBySetId :: X'HashMap SorN SorN
  , setGroups :: X'Vector2D H2h.H2hSet
  , sets :: X'HashMap SorN H2h.H2hSet
  , usedTags :: X'HashSet2D Int String
  , participants :: X'HashMap Int H2h.Participant
  , charUsedGames :: X'HashMap2D Int Int Int
  , charColorUsedGames :: X'HashMap2D (Int /\ Int) Int Int
  , seasonOrds :: X'HashMap Int Int
  | x
  )

runSeason :: forall x a. Run (XSeason x) a -> Run x a
runSeason = id
  <<< x'eval_ @"clmIdByEntrantId"
  <<< x'eval_ @"wins"
  <<< x'eval_ @"losses"
  <<< x'eval_ @"lastEventId"
  <<< x'eval_ @"prEventIds"
  <<< x'eval_ @"chars"
  <<< x'eval_ @"ratingData"
  <<< x'eval_ @"eventPlacing"
  <<< x'eval_ @"eventWins"
  <<< x'eval_ @"eventLosses"
  <<< x'eval_ @"eventSetIds"
  <<< x'eval_ @"eventBeaters"
  <<< x'eval_ @"eventDidDQ"
  <<< x'eval_ @"h2hOpps"
  <<< x'eval_ @"h2hSetIds"
  <<< x'eval_ @"setGroups"
  <<< x'eval_ @"sets"
  <<< x'eval_ @"eventIdBySetId"
  <<< x'eval_ @"usedTags"
  <<< x'eval_ @"participants"
  <<< x'eval_ @"charUsedGames"
  <<< x'eval_ @"charColorUsedGames"
  <<< x'eval_ @"seasonOrds"

addPlayerIdToClmId :: forall x. SorN -> Int -> ClmV () x @@> Unit
addPlayerIdToClmId playerId clmId = do
  x'consAt @"playerIdsByClmId" clmId playerId
  x'insert @"clmIdByPlayerId" playerId clmId

runClm
  :: forall x a
   . ClmV () (E' JsError x) @@> a
  -> EA' JsError x @@> Result ClmStW.T ClmStE.T a
runClm m = do
  let
    getEnv s = x'lookupEnv s >>= e'unwrap (jsError "Required Env Var Missing" s)
  isDevEnv <- x'lookupEnv "CLM_STATS_IS_DEV" <#> (==) (Just "y")
  ggAuth <- getEnv "CLM_STATS_GG_AUTH"
  dataRoot <- getEnv "CLM_STATS_DATA_DIR"
  appCOPath <- getEnv "CLM_STATS_APP_CO"
  pagesCOPath <- getEnv "CLM_STATS_PAGES_CO"
  client <- pure $ H2h.mkClient do
    s'set'b @"authToken" $ Just ggAuth
    let cachePath = dataRoot /./ "cache" /./ "startgg.gqlCache"
    s'set'b @"cachePath" $ Just $ pathStr $ cachePath
  let tryDecode = x'decodeTextFile $ dataRoot /./ "FULL_LEGACY.json"
  legacyBlob <- e'try tryDecode >>= e'ok <<< constL (jsError "legacy read" "")
  clmIdByPlayerId <- hm'fromFoldable <$> forM
    (obj'entries legacyBlob.nameDataByPlayerId)
    \(ggIdS /\ { ident }) -> do
      ggId <- e'ok $ constL (jsError "invalid ggId" "") $
        runParser ggIdS parseInt
      clmId <- e'unwrap (jsError "unfound clmId" "") $ obj'lookup ident
        legacyBlob."IDENT_CLM_IDS"
      pure $ sOrN ggId /\ clmId
  x'out clmIdByPlayerId
  playerIdsByClmId <- _'exec_ @(X'HashSet2D Int SorN) do
    forM_ (hm'entries clmIdByPlayerId) \(pId /\ clmId) -> _'consAt clmId pId
  x'info playerIdsByClmId
  we'runResult
    $ x'eval_ @"seasons"
    $ x'eval_ @"seasonEvents"
    $ x'eval_ @"playerSeasons"
    $ x'eval @"playerIdsByClmId" playerIdsByClmId
    $ x'eval @"clmIdByPlayerId" clmIdByPlayerId
    $ x'eval @"nextClmId" legacyBlob.nextIdTry
    $ flip r'run m
        { isDevEnv
        , ggAuth
        , dataRoot
        , appCOPath
        , pagesCOPath
        , client
        , legacyBlob
        }

placeholderImageUrl :: String
placeholderImageUrl = "/img/CLM_Logo_Avatar_Placeholder.png"

getClmIdForEvent
  :: forall x
   . Spec.Spec
  -> H2h.Event
  -> H2h.Player
  -> ClmV () x @@> Boolean /\ Int
getClmIdForEvent { eventPlayerIdMasks } { slug } player = x'withReturn
  \x'return -> do
    let maskedClmId = hm'lookup slug eventPlayerIdMasks >>= hm'lookup player.id
    knownClmId <- x'lookup @"clmIdByPlayerId" player.id <#> (>|>) maskedClmId
    whenJust (knownClmId <#> (/\) (isJust maskedClmId)) x'return
    getClmId player.id <#> (/\) false

getClmId :: forall x. SorN -> ClmV () x @@> Int
getClmId playerId = x'withReturn \x'return -> do
  knownClmId <- x'lookup @"clmIdByPlayerId" playerId
  whenJust knownClmId x'return
  nextClmId <- x'extract @"nextClmId"
  clmId <- flip tailRecM nextClmId \tryId -> do
    exitingsPlayers <- x'sizeAt @"playerIdsByClmId" tryId
    pure $ if exitingsPlayers == 0 then Done tryId else Loop $ inc tryId
  x'assign @"nextClmId" $ inc clmId
  addPlayerIdToClmId playerId clmId
  pure clmId

getSeasonId :: DateTime -> Int
getSeasonId dt = periodMonthContrib + periodYearContrib
  where
  periodYearContrib = (((dateTime'year dt) - 2022) * 3) + 2
  periodMonthContrib = floor (toNumber (dateTime'month'i0 dt) / 4.0)

getActiveSeasonId :: forall x. x @@> Int
getActiveSeasonId = x'now <#> getSeasonId

xRun :: forall x. Array String -> EA' JsError x @@> Unit
xRun args = do
  x'info args
  activeSeasonId <- getActiveSeasonId
  res <- runClm do
    actionData <- getActions [] false
    let spec = Act.buildSpec $ actionData.manual <> actionData.auto
    forM_ (hm'entries spec.clmIdByPlayerId) \(clmId /\ playerId) ->
      addPlayerIdToClmId clmId playerId
    seasonH2hEvents <- getH2hData spec true
    forM_ seasonH2hEvents \(seasonId /\ h2hEvents) -> runSeason do
      x'out { seasonId }
      let isActiveSeason = seasonId == activeSeasonId
      let eventList = arr'sortWith (\e -> e.tournament.date) $ hm'vals h2hEvents
      forM_ eventList \event -> x'withReturn \xReturn'outer -> do
        isSingles <- x'withReturn \xReturn -> do
          forM_ (hm'vals event.entrants) \entrant -> do
            when (arr'size entrant.participants > 1) $ xReturn false
          pure true
        when (not isSingles) $ xReturn'outer unit
        let prIneligible = hs'has event.slug spec.ineligibleSlugs

        forM_ (hm'vals event.entrants) \entrant -> do
          let { participant, standing } = entrant
          let { player } = participant
          isMask /\ clmId <- getClmIdForEvent spec event player
          x'insert @"clmIdByEntrantId" entrant.id clmId
          x'insert @"eventPlacing" (clmId /\ event.id) standing.placement
          x'insert @"lastEventId" clmId event.id
          whenNot isMask do
            x'consAt @"usedTags" clmId participant.gamerTag
            x'insert @"participants" clmId participant

        x'd1cons_ @"setGroups"
        let eventSets = arr'concat $ event.phaseGroups <#> hm'vals <<< _.sets
        forM_ (arr'sortWith _.eventOrder eventSets) \set -> do
          x'consLast @"setGroups" set
          x'insert @"eventIdBySetId" set.id event.id

        x'insert @"seasonEvents" (seasonId /\ event.id)
          { eventName: event.name
          , numEntrants: hm'size event.entrants
          , date: event.tournament.date
          , slug: event.slug
          , prEligible: not prIneligible
          , tournamentName: event.tournament.name
          , imageUrl: jOr placeholderImageUrl $
              hm'lookup H2h.Profile event.tournament.images
          , eventId: event.id
          }
      s'eval 0 $ whr'eval @Int show (s'set'b @"w2" $ Just 20) do
        x'extract @"setGroups" >>= flip forM_ \sets -> do
          groupCount <- s'get
          anyCounted <- s'exec false do
            forM_ sets \set -> x'withReturn_ \x'return_ -> do
              eventId <- x'lookup @"eventIdBySetId" set.id >>= sc'unwrap_
              event <- x'lookup @"seasonEvents" (seasonId /\ eventId) >>=
                sc'unwrap_
              let
                getSlotClmId slot = do
                  entrantId <- sc'unwrap_ slot.entrantId
                  clmId <- x'lookup @"clmIdByEntrantId" entrantId >>= sc'unwrap_
                  pure $ clmId
              let slot1 ~ slot2 = set.slots
              clmId1 <- getSlotClmId slot1
              clmId2 <- getSlotClmId slot2
              clmIdW /\ clmIdL <- sc'unwrap_ set.winner <#> case _ of
                Pos -> clmId1 /\ clmId2
                Neg -> clmId2 /\ clmId1
              let incFrom0 = Just <<< inc <<< fromMaybe 0
              x'alter @"eventWins" (clmIdW /\ eventId) incFrom0
              x'alter @"eventLosses" (clmIdL /\ eventId) incFrom0
              x'consAt @"eventSetIds" (clmIdW /\ eventId) set.id
              x'consAt @"eventSetIds" (clmIdL /\ eventId) set.id
              x'consAt @"eventBeaters" (clmIdL /\ eventId) clmIdW
              x'insert @"eventDidDQ" (clmIdL /\ eventId) false
              x'insert @"eventDidDQ" (clmIdL /\ eventId) set.isDQ
              x'consAt @"h2hOpps" clmIdW clmIdL
              x'consAt @"h2hOpps" clmIdL clmIdW
              x'consAt @"h2hSetIds" (clmIdW /\ clmIdL) set.id
              x'consAt @"h2hSetIds" (clmIdL /\ clmIdW) set.id
              when (not set.doesCount || not event.prEligible) x'return_
              s'put true
              if set.isDQ then x'remove @"prEventIds" $ clmIdL /\ eventId
              else do
                x'insert @"lastEventId" clmIdL eventId
                x'insert @"lastEventId" clmIdW eventId
                x'consAt @"prEventIds" clmIdL eventId
                x'consAt @"prEventIds" clmIdW eventId
              forM_ set.games \game -> do
                let { selections, entrant1Score, entrant2Score } = game
                forM_ (hm'entries selections) \(pkey /\ selection) -> do
                  let clmId = pair'get pkey $ clmId1 ~ clmId2
                  let may'score = pair'get pkey $ entrant1Score ~ entrant2Score
                  let sggCharId = selection.character.id
                  whenJust (hm'lookup sggCharId sggSlippiCharIdMap) \chId -> do
                    x'alter @"charUsedGames" (clmId /\ chId) incFrom0
                    whenJust may'score \score -> do
                      when (score >= 100) do
                        let colorId = (score / 100) - 1
                        x'alter @"charColorUsedGames"
                          ((clmId /\ chId) /\ colorId)
                          incFrom0
              x'alter @"wins" clmIdW incFrom0
              x'alter @"losses" clmIdL incFrom0
              whr'createGame clmIdW clmIdL groupCount 0
            x'out { groupCount }
          when anyCounted $ s'update inc
        whr'iterate 100
        participants <- x'extract @"participants"
        forM_ (hm'entries participants) \(clmId /\ participant) -> do
          ratings <- whr'ratingsForPlayer clmId
          let rawRating = (+) 1000.0 <$> (arr'last ratings >>= flip nth 1)
          prEvents <- x'extractAt @"prEventIds" clmId
            <#> hs'vals
            <#> map (\id -> hm'lookup id h2hEvents <#> \e -> e.tournament.id)
            <#> arr'concatMaybes
            <#> hs'fromFoldable
            <#> hs'size
          x'insert @"ratingData" clmId
            { raw: may'or 0.0 rawRating
            , prEvents
            , hasAny: isJust rawRating
            , hasConfidence: prEvents > 2
            , clmId
            , participant
            }
          charUsedGames <- x'extractAt @"charUsedGames" clmId
            <#> (arr'reverse <<< arr'sortWith snd <<< hm'entries)
          let totGames = toNumber $ sumBy snd charUsedGames
          let mostGames = ceil $ totGames * 2.0 / 3.0
          chars <- _'exec_ @(X'Vector ClmSeason'Player'Char) $ s'eval 0 do
            forM_ charUsedGames \(charId /\ usedGames) -> do
              addedGames <- s'get
              when (addedGames <= mostGames) do
                color <- x'extractAt @"charColorUsedGames" (clmId /\ charId)
                  <#> (hm'entries >>> maximumWith snd >>> map fst >>> may'or 0)
                _'cons { charId, usage: toNumber usedGames / totGames, color }
          x'insert @"chars" clmId chars
        sortedRatingData <- x'extract @"ratingData"
          <#> hm'vals
          <#> multisort
            [ sorter'rev _.hasAny
            , sorter'rev _.hasConfidence
            , sorter'rev _.raw
            , sorter'rev _.prEvents
            ]
        let rated = arr'filter _.hasConfidence sortedRatingData
        let rated'stats = statsBy _.raw rated
        let getPercentile = \r -> stats'percentile r.raw rated'stats
        let getRating = \r -> floor $ 10.0 * (getPercentile r)
        let addRating = \r -> rec'insert @"rating" (getRating r) r
        x'out $ sortedRatingData <#> addRating
        when (arr'size rated > 1) do
          let title = ""
          let isAll = false
          seasonEvents <- x'extractAt @"seasonEvents" seasonId
          ranks <- _'exec_ @(X'Vector ClmSeason'Player) do
            s'eval 0 $ forM_ (arr'withInd sortedRatingData) \pair -> do
              let seasonOrd /\ ratingData = pair
              let { clmId, hasConfidence, prEvents, hasAny } = ratingData
              let { participant } = ratingData
              let { player } = participant
              let { socials, gamerTag, prefix, pronouns } = player
              let realName = player.name
              let ratingVal = getRating ratingData
              let rating = if hasConfidence then Just ratingVal else Nothing
              let otherSeasons = hs'empty
              let image = hm'lookup H2h.Profile player.images
              let hasRank = if isActiveSeason then hasAny else hasConfidence
              x'insert @"seasonOrds" clmId seasonOrd
              clmRank <- case hasRank of
                false -> pure Nothing
                true -> s'update inc *> s'get <#> Just
              wins <- x'lookup @"wins" clmId <#> may'or 0
              losses <- x'lookup @"losses" clmId <#> may'or 0
              chars <- x'lookup @"chars" clmId <#> may'or []
              lastEventId <- x'lookup @"lastEventId" clmId >>=
                e'unwrap (ClmStE.MissingEvent seasonId clmId)
              lastEventPlacing <-
                x'lookup @"eventPlacing" (clmId /\ lastEventId) >>=
                  e'unwrap (ClmStE.MissingEvent seasonId clmId)
              let lastEvent = { id: lastEventId, placing: lastEventPlacing }
              otherTags <- x'extractAt @"usedTags" clmId
              _'cons
                { clmId
                , chars
                , rating
                , clmRank
                , wins
                , losses
                , prEvents
                , image
                , socials
                , gamerTag
                , prefix
                , realName
                , pronouns
                , otherTags
                , otherSeasons
                , lastEvent
                }
          x'insert @"seasons" seasonId
            { seasonId
            , title
            , isAll
            , ranks
            , events: seasonEvents
            }
          seasonOrds <- x'extract @"seasonOrds"
          forM_ sortedRatingData \{ clmId } -> do
            opps <- x'extractAt @"h2hOpps" clmId
              <#> (arr'sortWith (flip hm'lookup seasonOrds) <<< hs'vals)
            events <- _'exec_ @(X'Vector ClmPlayer'Season'Event) do
              pure unit
            h2hs <- _'exec_ @(X'Vector ClmPlayer'Season'H2h) do
              pure unit
            let sets = hm'empty
            x'insert @"playerSeasons" (seasonId /\ clmId) { events, h2hs, sets }
  x'info { activeSeasonId }
  x'info $ case res.v of
    Left e -> encode e
    Right _ -> "data intake: success"

sggSlippiCharIdMap :: HashMap Int Int
sggSlippiCharIdMap = hm'fromFoldable
  [ 1 /\ 5
  , 2 /\ 0
  , 3 /\ 1
  , 4 /\ 22
  , 5 /\ 20
  , 6 /\ 2
  , 7 /\ 25
  , 8 /\ 14
  , 9 /\ 15
  , 10 /\ 4
  , 11 /\ 6
  , 12 /\ 7
  , 13 /\ 8
  , 14 /\ 9
  , 15 /\ 10
  , 16 /\ 3
  , 18 /\ 12
  , 20 /\ 13
  , 21 /\ 23
  , 22 /\ 16
  , 23 /\ 19
  , 24 /\ 17
  , 25 /\ 21
  , 26 /\ 18
  ]

luckyStatsIdByClmId :: HashMap Int String
luckyStatsIdByClmId = show <$> hm'fromFoldable
  [ 420 /\ 322 -- dz
  , 1062 /\ 21081 -- ober
  , 1286 /\ 72859 -- pleeba
  , 845 /\ 12922 -- mof
  , 870 /\ 240 -- DannyPhantom
  , 665 /\ 21094 -- unsure
  , 1978 /\ 100 -- Stella
  , 450 /\ 39548 -- Latin
  , 189 /\ 22100 -- Ferocitii
  , 588 /\ 21103 -- Forest
  , 1044 /\ 21115 -- Arpy
  , 1 /\ 84416 -- Raymond
  , 642 /\ 47243 -- GI0STAR
  , 1543 /\ 49815 -- easy
  , 1795 /\ 73996 -- eggy
  , 397 /\ 8548 -- ReLaxn
  , 417 /\ 11631 -- jisp
  , 295 /\ 74000 -- Fasthands
  , 238 /\ 474 -- Frost
  , 1836 /\ 148 -- Fry
  , 413 /\ 640 -- Kick
  , 42069 /\ 91 -- Rocks
  , 1513 /\ 369 -- Casual
  , 946 /\ 74486 -- Umma
  , 1449 /\ 1214031 -- JustJoe
  , 1132 /\ 14460 -- WattPheasant
  , 1755 /\ 74480 -- greg
  , 703 /\ 816606 -- cratylus
  , 1196 /\ 73029 -- mase
  , 1868 /\ 59962 -- Basil
  , 1187 /\ 6793944 -- Trix
  , 1567 /\ 558313 -- evil lesbian
  , 1576 /\ 11628 -- Trevor
  , 569 /\ 85311 -- notsocrazy
  , 26 /\ 33663 -- jade anxious
  , 1186 /\ 74455 -- Orotis
  , 422 /\ 73420 -- Surskim
  , 1714 /\ 11625 -- Killablue
  , 1163 /\ 73182 -- FeelinGood
  , 192 /\ 60535 -- Merlisker
  , 993 /\ 7125053 -- Rizz Princess
  , 666 /\ 80605 -- ink
  , 764 /\ 19408 -- Anconoid
  , 741 /\ 1223 -- Tawm,
  , 1436 /\ 21392 -- Nephenee
  , 3 /\ 7455 -- jess dang3r
  , 1732 /\ 51257 -- sambone
  , 943 /\ 252971 -- Lazarous
  , 10 /\ 11625 -- kevin tenacity
  , 932 /\ 74538 -- yolk
  , 1041 /\ 21147 -- sunshine
  , 1440 /\ 110 -- kadence
  , 1206 /\ 21166 -- homeslice
  , 682 /\ 73168 -- jaq
  , 1873 /\ 21139 -- kibby
  , 432 /\ 60659 -- pregnanyand16
  , 32 /\ 21400 -- prax
  , 100 /\ 395433 -- Blowfishh
  , 1704 /\ 74451 -- gamer
  , 210 /\ 10032 -- Erebus
  , 1033 /\ 1062907 -- Zoraakk
  , 155 /\ 37559 -- CJ
  , 1691 /\ 74630 -- pansy_scheme
  , 2106 /\ 13046 -- jagr
  , 777 /\ 68176 -- yuunia
  , 1222 /\ 74565 -- Carter
  , 1474 /\ 21403 -- Axel
  , 218 /\ 1075246 -- Lil Nach
  , 890 /\ 74560 -- sir ghei
  , 35 /\ 73425 -- Grunker
  , 249 /\ 21396 -- Brendan
  , 1263 /\ 80722 -- June
  , 517 /\ 394302 -- frog catcher
  , 1997 /\ 74638 -- Monarc
  , 272 /\ 85340 -- Scoob
  , 1668 /\ 1017351 -- Mild
  , 1459 /\ 589086 -- penny
  , 879 /\ 17251 -- trail
  , 1784 /\ 6119255 -- nobie
  , 1447 /\ 17975 -- n0tch
  , 2753 /\ 381359 -- cam
  , 1437 /\ 243120 -- euph0ria
  , 1865 /\ 1025879 -- genuinely
  , 1620 /\ 74647 -- BENT
  , 357 /\ 33729 -- lemmanade
  ]
