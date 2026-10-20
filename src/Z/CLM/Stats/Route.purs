module Z.CLM.Stats.Route
  ( PlayerTab(..)
  , Route(..)
  , RoutePage(..)
  , route'gotoPlayer
  , route'gotoStats
  , route'isH2hPage
  , route'isPlayerPage
  , route'isSamePage
  , route'isStatsPage
  , route'mk
  , route'playerPageId
  , route'playerPageTab
  , route'urlEdits
  , route'urlParse
  ) where

import Z.Prelude

import Data.Eq.Generic (genericEq)
import Z.CLM.Stats.Season (seasonId'slug)
import Z.CLM.Stats.Types (ClmBuildOutputs)

data PlayerTab = PlayerTab'H2hs | PlayerTab'Tournaments

derive instance Generic PlayerTab _

instance Eq PlayerTab where
  eq = genericEq

instance Show PlayerTab where
  show = genericShow

data RoutePage = Stats | Player PlayerTab | Compare | H2hGrid

derive instance Generic RoutePage _

instance Eq RoutePage where
  eq = genericEq

instance Show RoutePage where
  show = genericShow

newtype Route = Route
  { page :: RoutePage, seasonId :: Int, clmIds :: Array Int }

derive instance Newtype Route _

derive instance Generic Route _

instance Show Route where
  show = genericShow

route'mk :: Int -> RoutePage -> Array Int -> Route
route'mk seasonId page clmIds = Route { page, seasonId, clmIds }

route'urlEdits :: Int -> Route -> Edit UrlParts
route'urlEdits defaultSeasonId (Route { page, seasonId, clmIds }) = do
  s'set'b @"hash" (getHash clmIds)
  s'set'b @"path" path
  s'set'b @"query" query
  where
  idCount = arr'size clmIds
  defaultPage = case idCount of
    0 -> Stats
    1 -> Player PlayerTab'H2hs
    2 -> Compare
    _ -> H2hGrid
  isValidPage = case page of
    Stats -> idCount == 0
    Player _ -> idCount < 2
    Compare -> idCount < 3
    _ -> true
  actualPage = if isValidPage then page else defaultPage
  path = sync'x $ _'exec_ @(X'Vector String) do
    when (seasonId /= defaultSeasonId) do
      _'cons $ seasonId'slug seasonId
    case actualPage of
      Player _ -> _'cons "-"
      Compare -> _'cons "compare"
      H2hGrid -> _'cons "h2h"
      _ -> pure unit
  query = sync'x $ _'exec_ @(X'HashMap String (Array String)) do
    case actualPage of
      Player PlayerTab'Tournaments -> _'insert "tab" [ "tournaments" ]
      _ -> pure unit
  getHash [] = Nothing
  getHash ids = Just $ str'joinWith "~" $ ids <#> show

urlParse'impl :: Int -> HashMap String Int -> URL -> Either String Route
urlParse'impl defaultSeasonId seasonIdBySlug u =
  sync'x $ e'try $ s'exec (route'mk defaultSeasonId Stats []) do
    processClmIds $ url'hash u
    processRouteList false $ list'fromFoldable $ url'pathSegments u
    processQuery $ url'query u
  where
  processClmIds (Just hashStr) = do
    clmIds <- forM (str'split (Pattern "~") hashStr) \idStr -> do
      case (tryParseInt idStr) of
        Just clmId -> pure clmId
        _ -> e'fail $ "clmIds must be integers separated by `~`: " <> hashStr
    s'set'b @"!.clmIds" clmIds
  processClmIds _ = pure unit
  processRouteList _ Nil = pure unit
  processRouteList _ (Cons "-" Nil) = s'set'b @"!.page" $ Player PlayerTab'H2hs
  processRouteList _ (Cons "compare" Nil) = s'set'b @"!.page" Compare
  processRouteList _ (Cons "h2h" Nil) = s'set'b @"!.page" H2hGrid
  processRouteList false (Cons slug rest) =
    case (hm'lookup slug seasonIdBySlug) of
      Just seasonId -> do
        s'set'b @"!.seasonId" seasonId
        processRouteList true rest
      _ -> e'fail $ "Unknown Slug: " <> slug
  processRouteList _ _ = e'fail "Invalid Route"
  processQuery queryParams = do
    isPlayerPage <- s'get <#> route'isPlayerPage
    let isTournamentsTab = hm'lookup "tab" queryParams == Just [ "tournaments" ]
    when (isPlayerPage && isTournamentsTab) do
      s'set'b @"!.page" $ Player PlayerTab'Tournaments

route'urlParse :: ClmBuildOutputs -> URL -> Either String Route
route'urlParse { timeline } = urlParse'impl defaultSeasonId seasonIdBySlug
  where
  defaultSeasonId = timeline.defaultSeasonId
  allSeasonIds = arr'concat
    [ [ defaultSeasonId ], timeline.preDefault, timeline.postDefault ]
  seasonIdBySlug =
    hm'fromFoldable $ allSeasonIds <#> \id -> seasonId'slug id /\ id

route'isH2hPage :: Route -> Boolean
route'isH2hPage (Route { page: H2hGrid }) = true
route'isH2hPage _ = false

route'isStatsPage :: Route -> Boolean
route'isStatsPage (Route { page: Stats }) = true
route'isStatsPage _ = false

route'isPlayerPage :: Route -> Boolean
route'isPlayerPage (Route { page: Player _ }) = true
route'isPlayerPage _ = false

route'playerPageId :: Route -> Maybe (Maybe Int)
route'playerPageId (Route { page: Player _, clmIds }) = Just $ arr'first clmIds
route'playerPageId _ = Nothing

route'playerPageTab :: Route -> Maybe PlayerTab
route'playerPageTab (Route { page: Player tab }) = Just tab
route'playerPageTab _ = Nothing

route'gotoStats :: Edit Route
route'gotoStats = do
  isStatsPage <- s'get <#> route'isStatsPage
  whenNot isStatsPage do
    s'set'b @"!.page" $ Stats

route'gotoPlayer :: Edit Route
route'gotoPlayer = do
  isPlayerPage <- s'get <#> route'isPlayerPage
  whenNot isPlayerPage do
    s'set'b @"!.page" $ Player PlayerTab'H2hs

route'isSamePage :: Route -> Route -> Boolean
route'isSamePage r1 r2 = g_ @"!.page" r1 == g_ @"!.page" r2