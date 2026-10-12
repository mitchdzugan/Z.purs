module Node.Z.CLM.Stats.Manager.CLI where

import Node.Z.Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Node.Z.CLM.Stats.Manager.Action as Act
import Node.Z.CLM.Stats.Manager.Constants
  ( luckyStatsIdByClmId
  , sggSlippiCharIdMap
  )
import Node.Z.CLM.Stats.Manager.Error as ClmStE
import Node.Z.CLM.Stats.Manager.Legacy as ClmStLegacy
import Node.Z.CLM.Stats.Manager.Spec as Spec
import Node.Z.CLM.Stats.Manager.Warning as ClmStW
import Node.Z.CLM.Stats.Queries as Q
import Node.Z.Gql as Gql
import Node.Z.H2h as H2h
import Node.Z.H2h.Startgg.All as All
import Z.CLM.Stats.Types
  ( ClmPlayer'Season
  , ClmPlayer'Season'Event
  , ClmPlayer'Season'H2h
  , ClmPlayer'Season'Set
  , ClmSeason
  , ClmSeason'Event
  , ClmSeason'Player
  , ClmSeason'Player'Char
  )

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
  , legacyBlob :: ClmStLegacy.CLMStatsLegacyBlob
  | r
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
  , allEventIds :: X'HashSet2D Int SorN
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
  <<< x'eval_ @"allEventIds"
  <<< x'eval_ @"prEventIds"
  <<< x'eval_ @"chars"
  <<< x'eval_ @"ratingData"
  <<< x'eval_ @"eventPlacing"
  <<< x'eval_ @"eventWins"
  <<< x'eval_ @"eventLosses"
  <<< x'eval_ @"eventSetIds"
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
getClmIdForEvent spec { slug } player = x'withReturn \x'return -> do
  let maskedId = hm'lookup slug spec.eventPlayerIdMasks >>= hm'lookup player.id
  knownClmId <- x'lookup @"clmIdByPlayerId" player.id <#> (>|>) maskedId
  whenJust (knownClmId <#> (/\) (isJust maskedId)) x'return
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

buildCurrentSeasons :: forall x. Spec.Spec -> ClmV () x @@> Unit
buildCurrentSeasons spec = do
  activeSeasonId <- getActiveSeasonId
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
        x'consAt @"allEventIds" clmId event.id
        x'insert @"lastEventId" clmId event.id
        whenNot isMask do
          x'consAt @"usedTags" clmId participant.gamerTag
          x'insert @"participants" clmId participant

      x'd1cons_ @"setGroups"
      let eventSets = arr'concat $ event.phaseGroups <#> hm'vals <<< _.sets
      forM_ (arr'sortWith _.eventOrder eventSets) \set -> do
        x'consLast @"setGroups" set
        x'insert @"eventIdBySetId" set.id event.id
        x'insert @"sets" set.id set

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
            let { gamerTag, prefix, pronouns } = player
            let rawSocials = player.socials
            let realName = player.name
            let ratingVal = getRating ratingData
            let rating = if hasConfidence then Just ratingVal else Nothing
            let otherSeasons = hs'empty
            let image = hm'lookup H2h.Profile player.images
            let hasRank = if isActiveSeason then hasAny else hasConfidence
            socials <- _'exec @(X'HashMap H2h.SocialType String) rawSocials do
              whenJust (hm'lookup clmId luckyStatsIdByClmId) \luckyStatsId ->
                _'insert H2h.LuckyStats luckyStatsId
            x'insert @"seasonOrds" clmId seasonOrd
            clmRank <- case hasRank of
              false -> pure Nothing
              true -> s'update inc *> s'get <#> Just
            wins <- x'lookup @"wins" clmId <#> may'or 0
            losses <- x'lookup @"losses" clmId <#> may'or 0
            chars <- x'lookup @"chars" clmId <#> may'or []
            x'lookup @"lastEventId" clmId >>= flip whenJust \lastEventId -> do
              lastEventPlacing <-
                x'lookup @"eventPlacing" (clmId /\ lastEventId) >>=
                  e'unwrap (ClmStE.MissingPlacing seasonId clmId lastEventId)
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
        seasonSets <- x'extract @"sets"
        let setOrder = \setId -> hm'lookup setId seasonSets <#> _.eventOrder
        forM_ sortedRatingData \{ clmId } -> do
          eventIds <- x'extractAt @"allEventIds" clmId
            <#> hs'vals
          events <- _'exec_ @(X'Vector ClmPlayer'Season'Event) do
            forM_ eventIds \eventId -> do
              placing <-
                x'lookup @"eventPlacing" (clmId /\ eventId) >>=
                  e'unwrap (ClmStE.MissingPlacing seasonId clmId eventId)
              numWins <- x'lookup @"eventWins" (clmId /\ eventId)
                <#> may'or 0
              numLosses <- x'lookup @"eventLosses" (clmId /\ eventId)
                <#> may'or 0
              didDQ <- x'lookup @"eventDidDQ" (clmId /\ eventId)
                <#> may'or false
              setIds <- x'extractAt @"eventSetIds" (clmId /\ eventId)
                <#> (arr'sortWith setOrder <<< hs'vals)
              let placingString = if didDQ then "DQ" else ordinal placing
              _'cons
                { eventId
                , didDQ
                , setIds
                , numWins
                , numLosses
                , placingString
                }

          opps <- x'extractAt @"h2hOpps" clmId
            <#> (arr'sortWith (flip hm'lookup seasonOrds) <<< hs'vals)
          h2hs <- _'exec_ @(X'Vector ClmPlayer'Season'H2h) do
            forM_ opps \oppId -> do
              setIds <- x'extractAt @"h2hSetIds" (clmId /\ oppId)
                <#> hs'vals
              _'cons { opponent: Right oppId, setIds }
          let
            setIds = hs'vals $ hs'fromFoldable $ arr'concat $ arr'concat
              [ events <#> _.setIds
              , h2hs <#> _.setIds
              ]
          x'out setIds

          sets <- _'exec_ @(X'HashMap SorN ClmPlayer'Season'Set) do
            forM_ setIds \id -> whenJust (hm'lookup id seasonSets) \set -> do
              x'out set

          x'insert @"playerSeasons" (seasonId /\ clmId) { events, h2hs, sets }

buildLegacySeasons :: forall x. ClmV () x @@> Unit
buildLegacySeasons = do
  { legacyBlob } <- r'ask
  forM_ (obj'vals legacyBlob.seasons) \season -> do
    x'out season
    let { seasonId, title, isAll } = season
    let ranks = []
    events <- _'exec_ @(X'HashMap SorN ClmSeason'Event) do
      forM_ (obj'vals season.events) \{ eventId } -> do
        x'out eventId
        whenJust (obj'lookup (show eventId) legacyBlob.events) \event -> do
          let trueEventId = sOrN eventId
          date <- e'unwrap (ClmStE.InvalidLegacyDate event.slug event.date)
            $ dateTime'fromMS'Int (event.date * 1000)
          _'insert trueEventId
            { eventId: trueEventId
            , numEntrants: event.numEntrants
            , slug: event.slug
            , eventName: event.eventName
            , tournamentName: event.tournamentName
            , imageUrl: event.imageUrl
            , prEligible: event.prEligible
            , date
            }
    x'insert @"seasons" seasonId
      { seasonId
      , title
      , isAll
      , ranks
      , events
      }
  pure unit

xRun :: forall x. Array String -> EA' JsError x @@> Unit
xRun args = do
  x'info args
  activeSeasonId <- getActiveSeasonId
  res <- runClm do
    actionData <- getActions [] false
    let spec = Act.buildSpec $ actionData.manual <> actionData.auto
    forM_ (hm'entries spec.clmIdByPlayerId) \(clmId /\ playerId) ->
      addPlayerIdToClmId clmId playerId
    -- buildCurrentSeasons spec
    buildLegacySeasons
  x'info { activeSeasonId }
  x'info $ case res.v of
    Left e -> encode e
    Right _ -> "data intake: success"
