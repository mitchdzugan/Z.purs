module Node.Z.CLM.Stats.Manager.Build
  ( buildClm
  ) where

import Node.Z.Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Node.Z.CLM.Stats.App.HtmlDoc (mkHtmlDoc)
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
import Node.Z.Git.Client
  ( GitClient
  , GitError
  , git'addAll
  , git'checkout
  , git'client
  , git'client'baseDir
  , git'commitM
  , git'currentBranch
  , git'isClean
  , git'lastCommitShort
  , git'pull
  , git'push
  , git'status
  )
import Node.Z.Gql as Gql
import Node.Z.H2h as H2h
import Node.Z.H2h.Startgg.All as All
import Z.CLM.Stats.Season (seasonId'fromDateTime)
import Z.CLM.Stats.Types
  ( ClmActionData
  , ClmBuildMeta(..)
  , ClmPlayer
  , ClmPlayer'Season
  , ClmPlayer'Season'Event
  , ClmPlayer'Season'H2h
  , ClmPlayer'Season'Set
  , ClmPlayerSeason'Json
  , ClmSeason'Event
  , ClmSeason'Json
  , ClmSeason'Player
  , ClmSeason'Player'Char
  , clmBuildMeta
  )

wrapH2hWE
  :: forall x a
   . Run (WaE' H2h.Warning H2h.Error $ WaE' ClmStW.T ClmStE.T x) a
  -> Run (WaE' ClmStW.T ClmStE.T x) a
wrapH2hWE = we'map ClmStW.H2h ClmStE.H2h

data GitClientLabel = GitClient'App | GitClient'Pages

type EnvR r =
  { isProdEnv :: Boolean
  , ggAuth :: String
  , dataRoot :: String
  , appCOPath :: String
  , pagesCOPath :: String
  , client :: Gql.Client
  , activeSeasonId :: Int
  , legacyBlob :: ClmStLegacy.CLMStatsLegacyBlob
  , pages'git :: GitClient
  , app'git :: GitClient
  , nowMS'number :: Number
  , buildMetaPath :: Path
  , lastBuildMeta :: Maybe ClmBuildMeta
  | r
  }

type TagData = { gamerTag :: String, prefix :: Maybe String }

type ClmV r x =
  ( RWaEA' (EnvR r) ClmStW.T ClmStE.T
      ( clmRanks :: X'HashMap Int (Array ClmSeason'Player)
      , clmEvents :: X'HashMap2D Int SorN ClmSeason'Event
      , clmPlayers :: X'HashMap Int ClmPlayer
      , clmPlayerSeasons :: X'HashMap2D Int Int ClmPlayer'Season
      , nextClmId :: X'Ref Int
      , playerIdsByClmId :: X'HashSet2D Int SorN
      , clmIdByPlayerId :: X'HashMap SorN Int
      , orderByClmIdAndSeasonId :: X'HashMap2D Int Int Int
      , usedTags :: X'HashSet2D Int String
      , tagDataByEntrantId :: X'HashMap SorN TagData
      | x
      )
  )

type AutoActionDef =
  (Parser String Unit) /\ (Parser String Unit) /\ (String -> Act.PureAction)

defAutoAction
  :: forall s1 s2
   . Parser String s1
  -> Parser String s2
  -> (String -> Act.Action ())
  -> AutoActionDef
defAutoAction p1 p2 f =
  (p1 *> pure unit) /\ (p2 *> pure unit) /\ (Act.PureAction <<< f)

autoActionDefs :: Array AutoActionDef
autoActionDefs =
  [ defAutoAction parseRest parseRest \slug -> Act.AddEvent { slug }
  , defAutoAction (parseAnyAroundString "bunker") (parseAnyAroundString "crazy")
      \slug -> Act.SetIsPrEligible { slug, isEligible: false }
  ]

initialManual :: Array Act.PureAction
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
  , Act.MarkDoneUpdating $ ggD "fight-night-uic-22" "melee-singles" {}
  , Act.MarkDoneUpdating $ ggD lastFudds "melee-singles" {}
  , Act.MarkDoneUpdating $ ggD lastFudds "melee-amateur-bracket" {}
  , Act.SetClmId { playerSorN: "4253823", clmId: 993 }
  , Act.SetClmId { playerSorN: "5211563", clmId: 1714 }
  , Act.SetClmId { playerSorN: "13423", clmId: 891 }
  , Act.SetDefaultSeasonId { seasonId: 15 }
  ]
  where
  lastFudds = "fudds-house-17-last-fudds"
  ggD t e r = rec'insert @"slug" ("tournament/" <> t <> "/event/" <> e) r

getGitClient :: forall r x. GitClientLabel -> ClmV r x @@> GitClient
getGitClient GitClient'App = r'ask <#> \r -> r.app'git
getGitClient GitClient'Pages = r'ask <#> \r -> r.pages'git

type T'useGit'fm'res =
  forall r x res
   . (GitClient -> E' GitError (ClmV r x) @@> res)
  -> ClmV r x @@> res

useGit :: GitClientLabel -> T'useGit'fm'res
useGit label fm = getGitClient label >>= \c -> ClmStE.GitClientError !$! fm c

useAppGit :: T'useGit'fm'res
useAppGit = useGit GitClient'App

usePagesGit :: T'useGit'fm'res
usePagesGit = useGit GitClient'Pages

git'ensureClean :: forall r x. GitClientLabel -> ClmV r x @@> Unit
git'ensureClean label = do
  baseDir <- getGitClient label <#> git'client'baseDir
  status <- useGit label $ git'status $ s'setPure'b @"porcelain" true
  when (status /= "") $ e'fail $ ClmStE.GitDirUnclean baseDir status
  let requiredBranch = git'requiredBranch label
  currentBranch <- useGit label $ git'currentBranch
  when (currentBranch /= requiredBranch) do
    e'fail $ ClmStE.GitIncorrectBranch baseDir requiredBranch currentBranch

git'requiredBranch :: GitClientLabel -> String
git'requiredBranch GitClient'App = "impl/purs"
git'requiredBranch GitClient'Pages = "impl/purs"

git'ensureCleanLatest :: forall r x. GitClientLabel -> ClmV r x @@> Unit
git'ensureCleanLatest label = do
  git'ensureClean label
  useGit label $ git'pull pass
  git'ensureClean label

get'nowMS'Rounded :: forall r x. ClmV r x @@> Int
get'nowMS'Rounded = r'ask <#> _.nowMS'number <#> \n ->
  (60 * 60 * 24 * floor (n / 1000.0 / 24.0 / 60.0 / 60.0)) + (24 * 60 * 60)

getActions
  :: forall r x. Array Act.PureAction -> Boolean -> ClmV r x @@> ClmActionData
getActions newActions usePrevAuto = do
  baseRes <- r'ask <#> _.lastBuildMeta <#> case _ of
    Nothing -> { manual: initialManual <> newActions, auto: [] }
    Just (ClmBuildMeta { actionData }) ->
      { manual: actionData.manual <> newActions, auto: actionData.auto }
  if usePrevAuto then pure baseRes
  else do
    { client } <- r'ask
    before <- get'nowMS'Rounded
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
getH2hData spec allowRefetch =
  x'eval_ @"seasonH2hEvents" @(X'HashMap2D Int SorN H2h.Event) do
    { client } <- r'ask
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

type RatingData =
  { raw :: Number
  , prEvents :: Int
  , clmId :: Int
  , lastEvent :: { id :: SorN, placing :: Int }
  , chars :: Array ClmSeason'Player'Char
  , wins :: Int
  , losses :: Int
  }

type XSeason x =
  ( clmIdByEntrantId :: X'HashMap SorN Int
  , wins :: X'HashMap Int Int
  , losses :: X'HashMap Int Int
  , lastEventId :: X'HashMap Int SorN
  , allEventIds :: X'HashSet2D Int SorN
  , prEventIds :: X'HashSet2D Int SorN
  , ratingData :: X'HashMap Int RatingData
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
  , participants :: X'HashMap Int H2h.Participant
  , charUsedGames :: X'HashMap2D Int Int Int
  , charColorUsedGames :: X'HashMap2D (Int /\ Int) Int Int
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
  <<< x'eval_ @"participants"
  <<< x'eval_ @"charUsedGames"
  <<< x'eval_ @"charColorUsedGames"

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
  isProdEnv <- x'lookupEnv "CLM_STATS_IS_PROD" <#> (==) (Just "y")
  ggAuth <- getEnv "CLM_STATS_GG_AUTH"
  dataRoot <- getEnv "CLM_STATS_DATA_DIR"
  appCOPath <- getEnv "CLM_STATS_APP_CO"
  pagesCOPath <- getEnv "CLM_STATS_PAGES_CO"
  client <- pure $ H2h.mkClient do
    s'setPure'b @"authToken" ggAuth
    let cachePath = dataRoot /./ "cache" /./ "startgg.gqlCache"
    s'setPure'b @"cachePath" $ pathStr cachePath
  activeSeasonId <- getActiveSeasonId
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
  playerIdsByClmId <- _'exec_ @(X'HashSet2D Int SorN) do
    forM_ (hm'entries clmIdByPlayerId) \(pId /\ clmId) -> _'consAt clmId pId
  nowMS'number <- x'nowMS'number
  let buildMetaPath = pagesCOPath /./ "build.json"
  lastBuildMeta <- x'decodeTextFile @ClmBuildMeta buildMetaPath # e'try <#> hush
  we'runResult
    $ x'eval_ @"clmRanks"
    $ x'eval_ @"clmEvents"
    $ x'eval_ @"clmPlayers"
    $ x'eval_ @"clmPlayerSeasons"
    $ x'eval_ @"orderByClmIdAndSeasonId"
    $ x'eval_ @"usedTags"
    $ x'eval_ @"tagDataByEntrantId"
    $ x'eval @"playerIdsByClmId" playerIdsByClmId
    $ x'eval @"clmIdByPlayerId" clmIdByPlayerId
    $ x'eval @"nextClmId" legacyBlob.nextIdTry
    $ flip r'run m
        { isProdEnv
        , ggAuth
        , dataRoot
        , appCOPath
        , pagesCOPath
        , client
        , legacyBlob
        , activeSeasonId
        , pages'git: git'client pagesCOPath
        , app'git: git'client appCOPath
        , nowMS'number
        , lastBuildMeta
        , buildMetaPath
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

addRanks :: forall x. Int -> Array RatingData -> ClmV () x @@> Unit
addRanks seasonId ratingData = do
  { activeSeasonId } <- r'ask
  let
    p'hasConfidence = \d -> d.prEvents > 2
    isActiveSeason = seasonId == activeSeasonId
    sortedRatingData = flip multisort ratingData
      [ sorter'rev \d -> d.prEvents > 0
      , sorter'rev p'hasConfidence
      , sorter'rev _.raw
      , sorter'rev _.prEvents
      ]
    rated = arr'filter p'hasConfidence sortedRatingData
    rated'stats = statsBy _.raw rated
    getPercentile = \r -> stats'percentile r.raw rated'stats
    getRating = \r -> floor $ 10.0 * (getPercentile r)
  when (arr'size rated > 1) do
    ranks <- _'exec_ @(X'Vector ClmSeason'Player) do
      s'eval 0 $ forM_ (arr'withInd sortedRatingData) \pair -> do
        let seasonOrd /\ ratingData = pair
        let { clmId, prEvents, lastEvent, chars, wins, losses } = ratingData
        let hasRequirements = prEvents > 7
        let hasConfidence = prEvents > 2
        let ratingVal = getRating ratingData
        let rating = if hasConfidence then Just ratingVal else Nothing
        let hasRank = if isActiveSeason then hasConfidence else hasRequirements
        x'insert @"orderByClmIdAndSeasonId" (seasonId /\ clmId) seasonOrd
        clmRank <- case hasRank of
          false -> pure Nothing
          true -> s'update inc *> s'get <#> Just
        _'cons
          { clmId
          , rating
          , clmRank
          , wins
          , losses
          , prEvents
          , chars
          , lastEvent
          }
    x'insert @"clmRanks" seasonId ranks

getActiveSeasonId :: forall x. x @@> Int
getActiveSeasonId = x'now <#> seasonId'fromDateTime

buildCurrentSeasons :: forall x. Spec.Spec -> ClmV () x @@> Unit
buildCurrentSeasons spec = do
  seasonH2hEvents <- getH2hData spec true
  forM_ seasonH2hEvents \(seasonId /\ h2hEvents) -> runSeason do
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
        x'insert @"tagDataByEntrantId" entrant.id
          { gamerTag: participant.gamerTag
          , prefix: participant.prefix
          }
        x'insert @"clmIdByEntrantId" entrant.id clmId
        x'insert @"eventPlacing" (clmId /\ event.id) standing.placement
        x'consAt @"allEventIds" clmId event.id
        x'insert @"lastEventId" clmId event.id
        whenNot isMask do
          x'consAt @"usedTags" clmId participant.gamerTag
          x'consAt @"usedTags" clmId player.gamerTag
          x'insert @"participants" clmId participant

      x'd1cons_ @"setGroups"
      let eventSets = arr'concat $ event.phaseGroups <#> hm'vals <<< _.sets
      forM_ (arr'sortWith _.eventOrder eventSets) \set -> do
        x'consLast @"setGroups" set
        x'insert @"eventIdBySetId" set.id event.id
        x'insert @"sets" set.id set

      x'insert @"clmEvents" (seasonId /\ event.id)
        { eventName: event.name
        , numEntrants: hm'size event.entrants
        , date: event.tournament.date
        , source: { slug: event.slug, site: event.site }
        , prEligible: not prIneligible
        , tournamentName: event.tournament.name
        , imageUrl: jOr placeholderImageUrl $
            hm'lookup H2h.Profile event.tournament.images
        , eventId: event.id
        }
    s'eval 0 $ whr'eval @Int show (s'setPure'b @"w2" 20) do
      x'extract @"setGroups" >>= flip forM_ \sets -> do
        groupCount <- s'get
        anyCounted <- s'exec false do
          forM_ sets \set -> x'withReturn_ \x'return_ -> do
            eventId <- x'lookup @"eventIdBySetId" set.id >>= sc'unwrap_
            event <- x'lookup @"clmEvents" (seasonId /\ eventId) >>= sc'unwrap_
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
            x'insert @"eventDidDQ" (clmIdL /\ eventId) false
            x'insert @"eventDidDQ" (clmIdL /\ eventId) set.isDQ
            x'consAt @"h2hOpps" clmIdW clmIdL
            x'consAt @"h2hOpps" clmIdL clmIdW
            x'consAt @"eventSetIds" (clmIdW /\ eventId) set.id
            x'consAt @"eventSetIds" (clmIdL /\ eventId) set.id
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
        when anyCounted $ s'update inc
      whr'iterate 100
      participants <- x'extract @"participants"
      forM_ (hm'entries participants) \(clmId /\ participant) -> do
        let { player } = participant
        let { gamerTag, prefix, pronouns, socials } = player
        let realName = player.name
        let image = hm'lookup H2h.Profile player.images
        ratings <- whr'ratingsForPlayer clmId
        let rawRating = (+) 1000.0 <$> (arr'last ratings >>= flip nth 1)
        prEvents <- x'extractAt @"prEventIds" clmId
          <#> hs'vals
          <#> map (\id -> hm'lookup id h2hEvents <#> \e -> e.tournament.id)
          <#> arr'concatMaybes
          <#> hs'fromFoldable
          <#> hs'size
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
        x'lookup @"lastEventId" clmId >>= flip whenJust \lastEventId -> do
          lastEventPlacing <-
            x'lookup @"eventPlacing" (clmId /\ lastEventId) >>=
              e'unwrap (ClmStE.MissingPlacing seasonId clmId lastEventId)
          let lastEvent = { id: lastEventId, placing: lastEventPlacing }
          wins <- x'lookup @"wins" clmId <#> may'or 0
          losses <- x'lookup @"losses" clmId <#> may'or 0
          x'insert @"ratingData" clmId
            { raw: may'or 0.0 rawRating
            , prEvents
            , clmId
            , lastEvent
            , chars
            , wins
            , losses
            }
        x'insert @"clmPlayers" clmId
          { image
          , socials
          , gamerTag
          , prefix
          , realName
          , pronouns
          , clmId
          }
      x'extract @"ratingData" >>= addRanks seasonId <<< hm'vals
      seasonOrds <- x'extractAt @"orderByClmIdAndSeasonId" seasonId
      seasonSets <- x'extract @"sets"
      let setOrder = \setId -> hm'lookup setId seasonSets <#> _.eventOrder
      forM_ (hm'keys participants) \clmId -> do
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

        sets <- _'exec_ @(X'HashMap SorN ClmPlayer'Season'Set) do
          forM_ setIds \id -> whenJust (hm'lookup id seasonSets) \set -> do
            let { slots } = set
            whenJust set.winner \winner -> do
              let
                may'eIds = do
                  let eIdSlots = slots <#> _.entrantId
                  eId1 <- pair'get Pos eIdSlots
                  eId2 <- pair'get Neg eIdSlots
                  pure $ eId1 ~ eId2
              whenJust may'eIds \eIds -> do
                let eIdW = pair'get winner eIds
                may'clmIdW <- x'lookup @"clmIdByEntrantId" eIdW
                let won = may'clmIdW == Just clmId
                let selfGetter = if won then pair'get else pair'getOppo
                let oppoGetter = if won then pair'getOppo else pair'get
                let eIdSelf = selfGetter winner eIds
                let eIdOppo = oppoGetter winner eIds
                let slotsPosSelf = if won then slots else pair'swap slots
                let selfScore = (pair'get Pos slotsPosSelf).score
                let oppoScore = (pair'get Neg slotsPosSelf).score
                may'clmIdOppo <- x'lookup @"clmIdByEntrantId" eIdOppo
                whenJust may'clmIdOppo \clmIdOppo -> do
                  eventId <- x'lookup @"eventIdBySetId" id >>=
                    e'unwrap (ClmStE.MissingSetEventId seasonId id)
                  selfTagData <- x'lookup @"tagDataByEntrantId" eIdSelf >>=
                    e'unwrap (ClmStE.MissingTagData seasonId eIdSelf)
                  oppoTagData <- x'lookup @"tagDataByEntrantId" eIdOppo >>=
                    e'unwrap (ClmStE.MissingTagData seasonId eIdOppo)
                  _'insert id
                    { id
                    , eventId
                    , dq: set.isDQ
                    , isOverridden: false -- TODO
                    , won
                    , round: set.roundText
                    , opponentId: Just clmIdOppo
                    , oppo:
                        { gamerTag: oppoTagData.gamerTag
                        , prefix: oppoTagData.prefix
                        , score: show oppoScore
                        }
                    , self:
                        { gamerTag: selfTagData.gamerTag
                        , prefix: selfTagData.prefix
                        , score: show selfScore
                        }
                    }

        x'insert @"clmPlayerSeasons" (seasonId /\ clmId) { events, h2hs, sets }

buildLegacySeasons :: forall x. ClmV () x @@> Unit
buildLegacySeasons = do
  { legacyBlob } <- r'ask
  allPlayerSeasons <-
    _'exec_ @(X'Vector ClmStLegacy.CLMStatsLegacyBlob'PlayerSeason) do
      forM_ (obj'vals legacyBlob.players) \playerSeasons ->
        forM_ (obj'entries playerSeasons) \(seasonId /\ playerSeason) -> do
          whenNot (seasonId == "0") $ _'cons playerSeason
  playerSeasonByClmIdAndSeasonId <- _'exec_
    @(X'HashMap2D Int Int ClmStLegacy.CLMStatsLegacyBlob'PlayerSeason)
    do
      forM_ (obj'vals legacyBlob.players) \playerSeasons ->
        forM_ (obj'entries playerSeasons) \(seasonIdStr /\ playerSeason) -> do
          whenJust (tryParseInt seasonIdStr) \seasonId -> do
            whenNot (seasonId == 0) do
              whenJust playerSeason.clmId \clmId -> do
                _'insert (clmId /\ seasonId) playerSeason
  let
    lookupPlayerSeason = \clmId seasonId ->
      hm'lookup clmId playerSeasonByClmIdAndSeasonId >>= hm'lookup seasonId
  clmIdBySetResult <- _'exec_ @(X'HashMap2D String PairKey Int) do
    forM_ allPlayerSeasons \(playerSeason) -> do
      let { events, h2hs } = playerSeason
      whenJust playerSeason.clmId \clmId -> do
        let eventSetSummaries = events <#> _.setSummaries
        let h2hSetSummaries = h2hs <#> \h2h -> h2h.sets <#> _.setInfo
        let setSummaryGroups = [ eventSetSummaries, h2hSetSummaries ]
        let setSummaries = arr'concat $ arr'concat setSummaryGroups
        forM_ setSummaries \{ id, won } -> whenJust id \setId -> do
          let pairKey = if won then Pos else Neg
          _'insert (setId /\ pairKey) clmId
    forM_ allPlayerSeasons \(playerSeason) -> do
      forM_ playerSeason.h2hs \h2h -> do
        may'oppId <- x'withReturn \x'return -> do
          forM_ h2h.sets \{ setInfo } -> do
            whenJust setInfo.id \setId -> do
              let oppResult = if setInfo.won then Neg else Pos
              may'may'oppId <- _'lookup (setId /\ oppResult) <#> Just
              whenJust may'may'oppId x'return
          pure Nothing
        whenJust may'oppId \oppId -> do
          forM_ h2h.sets \{ setInfo } -> do
            whenJust setInfo.id \setId -> do
              let oppResult = if setInfo.won then Neg else Pos
              _'insert (setId /\ oppResult) oppId
  let allSeasons = obj'vals legacyBlob.seasons
  let usedSeasons = arr'remove ((==) 0 <<< _.seasonId) allSeasons
  eventIdBySlug <- _'exec_ @(X'HashMap String Int) do
    forM_ (obj'vals legacyBlob.players) \playerSeasons -> do
      forM_ (obj'vals playerSeasons) \playerSeason -> do
        forM_ playerSeason.events \event -> do
          _'insert event.event.slug event.event.eventId
    forM_ (obj'vals legacyBlob.events) \{ eventId, slug } -> do
      _'insert slug eventId
  forM_ usedSeasons \season -> do
    let { seasonId } = season
    seasonEvents <- _'exec_ @(X'HashMap SorN ClmSeason'Event) do
      let
        insertEvent = \event -> do
          let trueEventId = sOrN event.eventId
          date <- e'unwrap (ClmStE.InvalidLegacyDate event.slug event.date)
            $ dateTime'fromMS'Int (event.date * 1000)
          eventName <- (event.eventName >|> event.name) #
            e'unwrap (ClmStE.MissingLegacyEventName seasonId event.slug)
          _'insert trueEventId
            { eventId: trueEventId
            , numEntrants: event.numEntrants
            , source: H2h.startggSource event.slug
            , eventName
            , tournamentName: event.tournamentName
            , imageUrl: event.imageUrl
            , prEligible: event.prEligible
            , date
            }
      forM_ (obj'vals season.events) \{ eventId } -> do
        whenJust (obj'lookup (show eventId) legacyBlob.events) insertEvent
      forM_ (obj'vals legacyBlob.players) \playerSeasons -> do
        case (obj'lookup (show seasonId) playerSeasons) of
          Nothing -> pass
          Just playerSeason ->
            forM_ (playerSeason.events <#> _.event) insertEvent
    forM_ (obj'vals season.players) \player -> do
      let gamerTag = may'or player.name player.tag
      x'consAt @"usedTags" player.clmId gamerTag
      x'insert @"clmPlayers" player.clmId
        { clmId: player.clmId
        , gamerTag
        , prefix: player.prefix
        , pronouns: player.pronouns
        , realName: player.realName
        , socials: hm'fromFoldable $ arr'concatMaybes
            [ player.twitter <#> (/\) H2h.Twitter
            , player.twitch <#> (/\) H2h.Twitch
            ]
        , image: Just player.image
        }
    rawRatingData /\ clmIdByRank <- _'run_ @(X'HashMap Int Int) do
      forM season.ranks \r -> do
        clmId <- e'unwrap (ClmStE.LegacyMissingClmId seasonId r.playerIdent)
          $ (obj'lookup r.playerIdent season.players <#> _.clmId)
        _'insert r.rank clmId
        pure
          { prEvents: r.prEvents
          , raw: r.conservativeRating
          , chars: []
          , clmId
          , wins: r.wins
          , losses: r.losses
          , lastEvent: { id: sOrN r.eventId, placing: r.placing }
          }
    addRanks seasonId rawRatingData
    forM_ rawRatingData \{ clmId } -> do
      whenJust (lookupPlayerSeason clmId seasonId) \playerSeason -> do
        res /\ sets <- x'run_ @"sets" @(X'HashMap SorN ClmPlayer'Season'Set) do
          h2hs <- _'exec_ @(X'Vector ClmPlayer'Season'H2h) do
            forM_ (arr'withInd playerSeason.h2hs) \(h2hInd /\ h2h) -> do
              opponent <- x'withReturn \x'return -> do
                forM_ h2h.sets \setData -> do
                  let { setInfo } = setData
                  whenJust setInfo.id \setId -> do
                    let oppResult = if setInfo.won then Neg else Pos
                    let may'idByResult = hm'lookup setId clmIdBySetResult
                    let may'oppId = may'idByResult >>= hm'lookup oppResult
                    whenJust (may'oppId <#> Right) x'return
                    whenJust (hm'lookup h2h.rank clmIdByRank <#> Right) x'return
                pure $ Left h2h.opponent
              let
                ignoredSlug = str'joinWith "/"
                  [ "tournament"
                  , "esports-arena-chicago-thursday-3-9-event-ft-squad-strike"
                  , "event"
                  , "melee-singles"
                  ]
              let usedSets = arr'remove ((==) ignoredSlug <<< _.slug) h2h.sets
              setIds <- forM (arr'withInd usedSets) \(setInd /\ setData) -> do
                let { setInfo, slug } = setData
                eventId <- hm'lookup slug eventIdBySlug
                  # e'unwrap (ClmStE.MissingLegacyEventId seasonId slug)
                let nSetId = "h2h" <+> seasonId <+> clmId <+> h2hInd <+> setInd
                let setId = sOrN $ may'or nSetId setInfo.id
                x'insert @"sets" setId
                  { id: setId
                  , eventId: sOrN eventId
                  , dq: setInfo.dq
                  , isOverridden: false
                  , won: setInfo.won
                  , round: setInfo.round
                  , opponentId: hush opponent
                  , oppo:
                      { gamerTag: may'or h2h.opponent setInfo.opponentName
                      , prefix: Nothing
                      , score: setInfo.lostGames
                      }
                  , self:
                      { gamerTag: playerSeason.pid
                      , prefix: Nothing
                      , score: setInfo.wonGames
                      }
                  }
                pure setId
              _'cons { opponent, setIds }
          setByEventAndRound <- x'extract @"sets" <#>
            (keyBy (\s -> s.eventId /\ s.round) <<< hm'vals)
          events <- _'exec_ @(X'Vector ClmPlayer'Season'Event) do
            forM_ (arr'withInd playerSeason.events) \(eventInd /\ event) -> do
              let eventSetSumsWithInd = arr'withInd event.setSummaries
              setIds <- forM eventSetSumsWithInd \(setInd /\ setSummary) -> do
                setId <- x'withReturn \x'return -> do
                  whenJust (setSummary.id <#> sOrN) x'return
                  let setKey = sOrN event.event.eventId /\ setSummary.round
                  let currSetId = hm'lookup setKey setByEventAndRound <#> _.id
                  whenJust currSetId x'return
                  pure $ sOrN
                    ("event" <+> seasonId <+> clmId <+> eventInd <+> setInd)
                isStored <- x'lookup @"sets" setId <#> isJust
                whenNot isStored do
                  x'insert @"sets" setId
                    { id: setId
                    , eventId: sOrN event.event.eventId
                    , dq: setSummary.dq
                    , isOverridden: false
                    , won: setSummary.won
                    , round: setSummary.round
                    , opponentId: Nothing
                    , oppo:
                        { gamerTag: ""
                        , prefix: Nothing
                        , score: setSummary.lostGames
                        }
                    , self:
                        { gamerTag: playerSeason.pid
                        , prefix: Nothing
                        , score: setSummary.wonGames
                        }
                    }
                pure setId
              _'cons
                { eventId: sOrN event.event.eventId
                , didDQ: event."DQ"
                , numWins: event.numWins
                , numLosses: event.numLosses
                , placingString: event.placingString
                , setIds
                }
          pure { events, h2hs }
        let { events, h2hs } = res
        x'insert @"clmPlayerSeasons" (seasonId /\ clmId) { events, h2hs, sets }
    x'assignAt @"clmEvents" seasonId seasonEvents
  pure unit

buildSeasonJson
  :: forall x
   . Array Int
  -> HashMap Int (Array Int)
  -> Array (Int /\ Array String)
  -> Int
  -> ClmV () x @@> ClmSeason'Json
buildSeasonJson allSeasonIds allPlayerSeasons otherPlayerTags seasonId = do
  ranks <- x'lookup @"clmRanks" seasonId <#> may'or []
  events <- x'extractAt @"clmEvents" seasonId <#> hm'vals
  players <- forM ranks \{ clmId } -> do
    player <- x'lookup @"clmPlayers" clmId >>=
      e'unwrap (ClmStE.MissingPlayer clmId)
    socials <- _'exec @(X'HashMap H2h.SocialType String) player.socials do
      whenJust (hm'lookup clmId luckyStatsIdByClmId) \luckyStatsId ->
        _'insert H2h.LuckyStats luckyStatsId
    pure $ rec'set @"socials" socials player
  let removeCurrSeasonId = arr'remove ((==) seasonId)
  pure
    { ranks
    , events
    , players
    , otherPlayerTags
    , otherPlayerSeasons: hm'entries $ allPlayerSeasons <#> removeCurrSeasonId
    , otherSeasons: removeCurrSeasonId allSeasonIds
    }

buildPlayerSeasonJson
  :: forall x. Int -> Int -> ClmV () x @@> ClmPlayerSeason'Json
buildPlayerSeasonJson seasonId clmId = do
  playerSeason <- x'lookup @"clmPlayerSeasons" (seasonId /\ clmId) >>=
    e'unwrap (ClmStE.MissingPlayerSeason seasonId clmId)
  pure
    { sets: hm'vals playerSeason.sets
    , events: playerSeason.events
    , h2hs: playerSeason.h2hs
    }

buildClm
  :: forall x
   . Array Act.PureAction
  -> EA' JsError x @@> Result ClmStW.T ClmStE.T Unit
buildClm newActions = runClm do
  actionData <- getActions newActions false
  let spec = Act.buildSpec $ actionData.manual <> actionData.auto
  forM_ (hm'entries spec.clmIdByPlayerId) \(clmId /\ playerId) ->
    addPlayerIdToClmId clmId playerId
  buildLegacySeasons
  buildCurrentSeasons spec
  clmSeasonRanks <- x'extract @"clmRanks" <#> hm'entries
  let filtered = arr'filter ((<) 1 <<< arr'size <<< snd) clmSeasonRanks
  let allSeasonIds = arr'sort $ map fst filtered
  allPlayerSeasons <- map hs'vals <$> _'exec_ @(X'HashSet2D Int Int) do
    forM_ filtered \(seasonId /\ ranks) -> forM_ ranks \{ clmId } -> do
      _'consAt clmId seasonId
  let arr'entries = hm'entries <<< map hs'vals
  otherPlayerTags <- arr'entries <$> _'exec_ @(X'HashSet2D Int String) do
    forM_ (hm'keys allPlayerSeasons) \clmId -> do
      mainTag <- x'lookup @"clmPlayers" clmId <#> map _.gamerTag
      usedTags <- x'extractAt @"usedTags" clmId <#> hs'vals
      forM_ usedTags \tag -> when (Just tag /= mainTag) do _'consAt clmId tag
  git'ensureCleanLatest GitClient'Pages
  git'ensureCleanLatest GitClient'App
  docsPath <- r'ask <#> \r -> r.pagesCOPath /./ "docs"
  let dbPath = docsPath /./ "_db"
  let seasonPath = \id -> dbPath /./ "seasons" /./ (show id <> ".json")
  let
    playerSeasonPath = \seasonId clmId ->
      dbPath /./ "players" /./ show clmId /./ (show seasonId <> ".json")
  e'map ClmStE.RimrafDocs $ x'rimraf dbPath
  x'out "Saving Seasons"
  forM_ allSeasonIds \seasonId -> do
    seasonJson <-
      buildSeasonJson allSeasonIds allPlayerSeasons otherPlayerTags seasonId
    e'map (ClmStE.WriteSeasonJson seasonId) $
      x'encodeTextFileP (seasonPath seasonId) seasonJson
  x'out "Saving Players"
  forM_ (hm'entries allPlayerSeasons) \(clmId /\ seasonIds) -> do
    forM_ seasonIds \seasonId -> do
      playerSeasonJson <-
        buildPlayerSeasonJson seasonId clmId
      e'map (ClmStE.WritePlayerSeasonJson seasonId clmId) $
        x'encodeTextFileP (playerSeasonPath seasonId clmId) playerSeasonJson
  x'out "Managing Git For Database"
  hasDBChanges <- usePagesGit git'isClean <#> not
  nowS <- r'ask <#> \r -> floor (r.nowMS'number / 1000.0)
  let gitMsg'ts = "ts@" <:> nowS
  let gitMsg'mkParts l p = arr'concat [ [ "clm-stats", l, "|" ], p, [ "|" ] ]
  let gitMsg'mk label pieces = str'joinWith " " $ gitMsg'mkParts label pieces
  dbHash <-
    if not hasDBChanges then do
      x'out "-- No Database Changes"
      r'ask <#> gmOr_ @"lastBuildMeta?!.outputs.dbHash" (show nowS)
    else do
      gitMsg'auto <- get'nowMS'Rounded <#> \n -> "autoActions:ts@" <:> n
      let gitMsg'new = "newActions[" <:> arr'size newActions <:> "]"
      let gitMsg = gitMsg'mk "db" [ gitMsg'ts, gitMsg'auto, gitMsg'new ]
      x'out "-- Database Changes Committed"
      x'out $ "--  -- " <> gitMsg
      usePagesGit git'addAll
      usePagesGit $ git'commitM gitMsg
      usePagesGit $ git'push pass
      usePagesGit git'lastCommitShort
  x'out { dbHash }
  let
    clmTimeline =
      { defaultSeasonId: spec.defaultSeasonId
      , preDefault: arr'filter ((>) spec.defaultSeasonId) allSeasonIds
      , postDefault: arr'filter ((<) spec.defaultSeasonId) allSeasonIds
      }
    buildOutputs = { timeline: clmTimeline, dbHash }
  e'map ClmStE.RimrafDB $ x'rimraf docsPath
  buildMetaPath <- r'ask <#> _.buildMetaPath
  x'encodeTextFileP buildMetaPath (clmBuildMeta actionData buildOutputs) !!-
    ClmStE.WriteBuildMeta
  let configPath = docsPath /./ "_config.yml"
  ClmStE.WriteJekyllConfig -!! x'writeTextFileP configPath $ w'str'nl \w -> do
    w $ "collections:"
    w $ "  db:"
    w $ "    output: true"
    w $ "    permalink: /db/" <> dbHash <> "/:path"
    w $ ""
  usePagesGit $ git'checkout $ w'say $ pathStr dbPath
  hasDocsChanges <- usePagesGit git'isClean <#> not
  x'out "Managing Git For Pages App"
  if not hasDocsChanges then do
    x'out "-- No Pages App Changes"
  else do
    appHash <- useAppGit git'lastCommitShort
    let gitMsg'db = "db#" <> dbHash
    let gitMsg'app = "app#" <> appHash
    let gitMsg = gitMsg'mk "docs" [ gitMsg'ts, gitMsg'db, gitMsg'app ]
    x'out "-- App Changes Changes Committed"
    x'out $ "--  -- " <> gitMsg
    usePagesGit git'addAll
    usePagesGit $ git'commitM gitMsg
    usePagesGit $ git'push pass
    deployHash <- usePagesGit git'lastCommitShort
    x'out { deployHash }
