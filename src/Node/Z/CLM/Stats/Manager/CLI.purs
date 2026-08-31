module Node.Z.CLM.Stats.Manager.CLI where

import Node.Z.Prelude

import Node.Z.CLM.Stats.Manager.Action as Act
import Node.Z.CLM.Stats.Manager.Error as ClmStE
import Node.Z.CLM.Stats.Manager.Spec as Spec
import Node.Z.CLM.Stats.Manager.Warning as ClmStW
import Node.Z.CLM.Stats.Queries as Q
import Node.Z.Gql as Gql
import Node.Z.H2h as H2h
import Node.Z.H2h.Startgg.All as All

wrapH2hWE
  :: forall x a
   . Run (WaE H2h.Warning H2h.Error $ WaE ClmStW.T ClmStE.T x) a
  -> Run (WaE ClmStW.T ClmStE.T x) a
wrapH2hWE = we'map ClmStW.H2h ClmStE.H2h

type CLMStatsLegacyBlob'SetSummary =
  { id :: Maybe String
  , won :: Boolean
  , dq :: Boolean
  , round :: String
  , wonGames :: String
  , lostGames :: String
  , opponentName :: Maybe String
  , winnerName :: Maybe String
  , loserName :: Maybe String
  }

type CLMStatsLegacyBlob =
  { nameDataByPlayerId :: Object { name :: String, ident :: String }
  , nextIdTry :: Int
  , "IDENT_CLM_IDS" :: Object Int
  , timeline ::
      Array
        { seasonId :: Int
        , title :: String
        , timelineInd :: Int
        , season :: String
        }
  , events ::
      Object
        { eventName :: String
        , numEntrants :: Int
        , date :: Int
        , slug :: String
        , prEligible :: Boolean
        , tournamentName :: String
        , imageUrl :: String
        , eventId :: Int
        }
  , players ::
      Object $ Object
        { pid :: String
        , clmId :: Maybe Int
        , events ::
            Array
              { event :: { eventId :: Int }
              , placingString :: String
              , setSummaries :: Array CLMStatsLegacyBlob'SetSummary
              , numWins :: Int
              , numLosses :: Int
              , losses :: Array String
              , "DQ" :: Boolean
              }
        , h2hs ::
            Array
              { opponent :: String
              , rank :: Int
              , sets ::
                  Array
                    { setInfo :: CLMStatsLegacyBlob'SetSummary
                    , tournamentName :: String
                    , date :: String
                    , slug :: String
                    }
              }
        }
  , seasons ::
      Object
        { seasonId :: Int
        , title :: String
        , isAll :: Boolean
        , others :: Object Int
        , events :: Object { eventId :: Int }
        , players ::
            Object
              { playerId :: Int
              , image :: String
              , name :: String
              , realName :: Maybe String
              , id :: Int
              , clmId :: Int
              }
        , ranks ::
            Array
              { rank :: Int
              , winrate :: Maybe Number
              , placing :: Int
              , placingString :: String
              , wins :: Int
              , losses :: Int
              , prEvents :: Int
              , rating :: Number
              , conservativeRating :: Number
              , playerIdent :: String
              , eventId :: Int
              }
        }
  }

type EnvR r =
  { isDevEnv :: String
  , ggAuth :: String
  , dataRoot :: String
  , appCOPath :: String
  , pagesCOPath :: String
  , client :: Gql.Client
  , legacyBlob :: CLMStatsLegacyBlob
  | r
  }

type ClmEvent =
  { eventName :: String
  , numEntrants :: Int
  , date :: DateTime
  , slug :: String
  , prEligible :: Boolean
  , tournamentName :: String
  , imageUrl :: String
  , eventId :: SorN
  }

type ClmPlayerStub =
  { image :: String
  , name :: String
  , realName :: Maybe String
  , id :: Int
  }

type ClmSeason r =
  { seasonId :: Int
  , title :: String
  , isAll :: Boolean
  , ranks ::
      Array
        { playerId :: Int
        , rating :: Number
        , clmRank :: Int
        , placing :: Int
        , wins :: Int
        , losses :: Int
        , prEvents :: Int
        , eventId :: SorN
        }
  | r
  }

type ClmPlayerSeason =
  { sets ::
      HashMap SorN
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
  , events ::
      Array
        { eventId :: SorN
        , placingString :: String
        , setIds :: Array SorN
        , numWins :: Int
        , numLosses :: Int
        , losses :: Array String
        , didDQ :: Boolean
        }
  , h2hs ::
      Array
        { opponent :: String
        , rank :: Int
        , setIds :: Array SorN
        }
  }

type ClmBaseSeason = ClmSeason ()

type ClmFullSeason = ClmSeason
  (players :: HashMap Int ClmPlayerSeason, events :: HashMap SorN ClmEvent)

type ClmData = { season :: ClmFullSeason, players :: HashMap Int ClmPlayerStub }

type ClmV r x =
  ( RWaEA (EnvR r) ClmStW.T ClmStE.T
      ( seasons :: XHM'R "seasons" Int ClmBaseSeason
      , players :: XHM'R "players" Int ClmPlayerStub
      , playerSeasons :: XHM2d'R "playerSeasons" Int Int ClmPlayerSeason
      , seasonEvents :: XHM2d'R "seasonEvents" Int SorN ClmEvent
      , nextIdTry :: S' Int
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
  [ Act.AddEvent $ ggD "the-botlane-show-5-yolk-grunker" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-6-jess-dang3r" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-7-kadence" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-8-dz" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-9-unsure" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-10-jair-the-creator" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-11-jisp" "melee-singles" {}
  , Act.AddEvent $ ggD "the-botlane-show-12-fluid-lucinasd" "melee-singles" {}
  , Act.AddEvent { slug: "fgzs2x09" }
  , Act.MarkChallonge { slug: "fgzs2x09" }
  , Act.MarkDoneUpdating $ ggD "bracket-at-the-emporium-9" "melee-singles" {}
  , Act.MarkDoneUpdating $ ggD "the-bunker-11" "crazy-doubles" {}
  , Act.MarkDoneUpdating $ ggD "the-bunker-12" "crazy-doubles" {}
  , Act.MarkDoneUpdating $ ggD "the-bunker-13" "crazy-doubles" {}
  , Act.MarkDoneUpdating $ ggD lastFudds "melee-singles" {}
  , Act.MarkDoneUpdating $ ggD lastFudds "melee-amateur-bracket" {}
  ]
  where
  lastFudds = "fudds-house-17-last-fudds"
  ggD t e r = rec'insert @"slug" ("tournament/" <> t <> "/event/" <> e) r

getActions :: forall r x. PureActions -> Boolean -> ClmV r x #> ActionData
getActions newActions usePrevAuto = do
  buildDataPath <- r'ask <#> \r -> r.pagesCOPath /./ "build.json"
  baseRes <- xDecodeTextFile @ActionData buildDataPath # e'try <#> case _ of
    Left _ -> { manual: initialManual <> newActions, auto: [] }
    Right d -> { manual: d.manual <> newActions, auto: d.auto }
  if usePrevAuto then pure baseRes
  else do
    { client } <- r'ask
    before <- xNowMS <#>
      \n -> (60 * 60 * floor (n / 1000.0 / 60.0 / 60.0)) + (24 * 60 * 60)
    let after = 1767225600
    let pSpecs = [ All.ggPageSpec (__ @"page") (__ @"tournaments") ]
    let initVars = { after, before, page: 0 }
    clmEvents <- wrapH2hWE $
      All.ggQueryAll Q.clmEvents initVars pSpecs client Gql.CacheFirst
    let slugs = clmEvents.tournaments.nodes <#> _.events # arr'concat <#> _.slug
    newAuto <- pure do
      slug <- slugs
      let pieces = str'split (Pattern "/") slug
      case pieces of
        [ "tournament", tslug, "event", eslug ] -> do
          tparse /\ eparse /\ mkAction <- autoActionDefs
          case (runParser tslug tparse /\ runParser eslug eparse) of
            (Right _ /\ Right _) -> pure $ mkAction slug
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
  -> ClmV r x #> Array (Int /\ HashMap String H2h.Event)
getH2hData spec allowRefetch = xhm2d'eval @"seasonEvents" do
  { client } <- r'ask
  xInfo { numEvents: hs'size spec.eventSlugs }
  forM_ (hs'vals spec.eventSlugs) \slug -> do
    let
      isChallonge = hs'has slug spec.challongeSlugs
      sourceFn = if isChallonge then H2h.challongeSource else H2h.startggSource
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
    let shouldRefetchEvent = eventData'.state /= "COMPLETED" && canRefetchEvent
    eventData <-
      if shouldRefetchEvent then getEventData Gql.ForceFetch
      else pure eventData'
    let seasonId = seasonIdByDate eventData.tournament.date
    xhm2d'insert @"seasonEvents" seasonId slug eventData
  xhm2d'entries @"seasonEvents"

type XSeason x =
  ( wins :: XHM'R "wins" Int Int
  , losses :: XHM'R "losses" Int Int
  , eventIds :: XHS2d'R "eventIds" Int SorN
  , eventWins :: XHM'R "eventWins" (Int /\ SorN) Int
  , eventLosses :: XHM'R "eventLosses" (Int /\ SorN) Int
  , eventSetIds :: XHS2d'R "eventSetIds" (Int /\ SorN) SorN
  , eventBeaters :: XHS2d'R "eventBeaters" (Int /\ SorN) Int
  | x
  )

runSeason :: forall x a. Run (XSeason x) a -> Run x a
runSeason = id
  <<< xhm'eval @"wins"
  <<< xhm'eval @"losses"
  <<< xhs2d'eval @"eventIds"
  <<< xhm'eval @"eventWins"
  <<< xhm'eval @"eventLosses"
  <<< xhs2d'eval @"eventSetIds"
  <<< xhs2d'eval @"eventBeaters"

runClm
  :: forall x a
   . ClmV () (E JsError x) ##> a
  -> EA JsError x ##> Result ClmStW.T ClmStE.T a
runClm m = do
  let
    getEnv s = xLookupEnv s >>= e'unwrap (jsError "Required Env Var Missing" s)
    wrappedM = r'ask >>= \{ legacyBlob } -> do
      m
  isDevEnv <- getEnv "CLM_STATS_IS_DEV"
  ggAuth <- getEnv "CLM_STATS_GG_AUTH"
  dataRoot <- getEnv "CLM_STATS_DATA_DIR"
  appCOPath <- getEnv "CLM_STATS_APP_CO"
  pagesCOPath <- getEnv "CLM_STATS_PAGES_CO"
  client <- pure $ H2h.mkClient do
    s'sets @"authToken" $ Just ggAuth
    let cachePath = dataRoot /./ "cache" /./ "startgg.gqlCache"
    s'sets @"cachePath" $ Just $ pathStr $ cachePath
  let tryDecode = xDecodeTextFile $ dataRoot /./ "FULL_LEGACY.json"
  legacyBlob <- e'try tryDecode >>= e'ok <<< constL (jsError "legacy read" "")
  clmIdByPlayerId <- hm'fromFoldable <$> forM
    (obj'entries legacyBlob.nameDataByPlayerId)
    \(ggIdS /\ { ident }) -> do
      ggId <- e'ok $ constL (jsError "invalid ggId" "") $
        runParser ggIdS parseInt
      clmId <- e'unwrap (jsError "unfound clmId" "") $ obj'lookup ident
        legacyBlob."IDENT_CLM_IDS"
      pure $ ggId /\ clmId
  xOut clmIdByPlayerId
  let
    playerIdsByClmId =
      hs2d'fromFoldable $ tup'flip <$> hm'entries clmIdByPlayerId
  xOut playerIdsByClmId
  we'runResult
    $ xhm'eval @"seasons"
    $ xhm'eval @"players"
    $ xhm2d'eval @"seasonEvents"
    $ xhm2d'eval @"playerSeasons"
    $ g1 @XEvalS @"nextIdTry" legacyBlob.nextIdTry
    $ flip r'run wrappedM
        { isDevEnv
        , ggAuth
        , dataRoot
        , appCOPath
        , pagesCOPath
        , client
        , legacyBlob
        }

xRun :: forall x. Array String -> EA JsError x ##> Unit
xRun args = do
  xInfo args
  res <- runClm do
    actionData <- getActions [] false
    let spec = Act.buildSpec $ actionData.manual <> actionData.auto
    seasonEvents <- getH2hData spec true
    forM_ seasonEvents \(seasonId /\ events) -> runSeason do
      xOut { seasonId }
      xOut events
      let eventList = arr'sortWith (\e -> e.tournament.date) $ hm'vals events
      forM_ eventList \event -> x'withContinue \xContinue -> do
        isSingles <- x'withReturn \xReturn -> do
          forM_ (map'vals event.entrants) \entrant -> do
            when (arr'size entrant.participants > 1) $ xReturn false
          pure true
        when (not isSingles) xContinue
        forM_ (map'vals event.entrants) \entrant -> do
          xInfo entrant
        xhm2d'insert @"seasonEvents" seasonId event.id
          { eventName: event.name
          , numEntrants: map'size event.entrants
          , date: event.tournament.date
          , slug: event.slug
          , prEligible: true
          , tournamentName: event.tournament.name
          , imageUrl: ""
          , eventId: event.id
          }
        xOut { event }
      pure unit
  xInfo $ case res.v of
    Left e -> encode e
    Right _ -> "data intake: success"
