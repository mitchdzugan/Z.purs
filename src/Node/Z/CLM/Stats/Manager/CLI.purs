module Node.Z.CLM.Stats.Manager.CLI where

import Node.Z.Prelude

import Data.Array (concat)
import Debug (traceM)
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
        { periodId :: Int
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
  , periods ::
      Object
        { periodId :: Int
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
              , winrate :: Maybe Int
              , placing :: Int
              , placingString :: String
              , wins :: Int
              , losses :: Int
              , prEvents :: Int
              , rating :: Int
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
  | r
  }

type ClmV r x = (RWaEA (EnvR r) ClmStW.T ClmStE.T x)

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

getActions :: forall r x. PureActions -> Boolean -> ClmV r x #> ActionData
getActions newActions usePrevAuto = do
  buildDataPath <- r'ask <#> \r -> r.pagesCOPath /./ "build.json"
  baseRes <- xDecodeTextFile @ActionData buildDataPath # e'try <#> case _ of
    Left _ -> { manual: newActions, auto: [] }
    Right d -> { manual: d.manual <> newActions, auto: d.auto }
  if usePrevAuto then pure baseRes
  else do
    { ggAuth, dataRoot } <- r'ask
    before <- xNowMS <#>
      \n -> (60 * 60 * floor (n / 1000.0 / 60.0 / 60.0)) + (24 * 60 * 60)
    let after = 1767225600
    let pSpecs = [ All.ggPageSpec (__ @"page") (__ @"tournaments") ]
    let initVars = { after, before, page: 0 }
    client <- pure $ H2h.mkClient do
      s'sets @"authToken" (Just ggAuth)
      s'sets @"cachePath" (Just $ pathStr $ dataRoot /./ "startgg.gqlCache")
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

getH2hData :: forall r x. Spec.Spec -> Boolean -> ClmV r x #> Unit
getH2hData spec allowRefetch = do
  forM_ (arr'fromFoldable spec.eventSlugs) \slug -> do
    let
      isDone = set'has slug spec.doneUpdating
      needsRefetch = set'has slug spec.eventsToRefetch
      wantsRefetch = not isDone || needsRefetch
      shouldRefetchEvent = allowRefetch && wantsRefetch
      networkControl = case (isDone /\ needsRefetch) of
        (true /\ _) -> Gql.CacheOnly
        (_ /\ true) -> Gql.ForceFetch
        _ -> default
    xOut $ { slug, shouldRefetchEvent, networkControl }
    pure unit

xRun :: forall x. Array String -> EA JsError x ##> Unit
xRun args = do
  xInfo args
  isDevEnv <- getEnv "CLM_STATS_IS_DEV"
  ggAuth <- getEnv "CLM_STATS_GG_AUTH"
  dataRoot <- getEnv "CLM_STATS_DATA_DIR"
  appCOPath <- getEnv "CLM_STATS_APP_CO"
  pagesCOPath <- getEnv "CLM_STATS_PAGES_CO"
  let env = { isDevEnv, ggAuth, dataRoot, appCOPath, pagesCOPath }
  res <- we'runResult $ r'run env do
    actionData <- getActions [] false
    let spec = Act.buildSpec $ actionData.manual <> actionData.auto
    getH2hData spec true
  xInfo res
  where
  getEnv s = xLookupEnv s >>= e'unwrap (jsError "Required Env Var Missing" s)
