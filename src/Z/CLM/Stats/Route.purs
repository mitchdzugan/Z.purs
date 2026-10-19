module Z.CLM.Stats.Route
  ( Route(..)
  , RoutePage(..)
  , route'isH2hPage
  , route'isStatsPage
  , route'mk
  , route'urlEdits
  , route'urlParse
  ) where

import Z.Prelude

import Z.CLM.Stats.Season (seasonId'slug)
import Z.CLM.Stats.Types (ClmBuildOutputs)

data RoutePage = Stats | Player | Compare | H2hGrid

derive instance Generic RoutePage _

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
  where
  idCount = arr'size clmIds
  defaultPage = case idCount of
    0 -> Stats
    1 -> Player
    2 -> Compare
    _ -> H2hGrid
  isValidPage = case page of
    Stats -> idCount == 0
    Player -> idCount < 2
    Compare -> idCount < 3
    _ -> true
  actualPage = if isValidPage then page else defaultPage
  path = sync'x $ _'exec_ @(X'Vector String) do
    when (seasonId /= defaultSeasonId) do
      _'cons $ seasonId'slug seasonId
    case actualPage of
      Player -> _'cons "-"
      Compare -> _'cons "compare"
      H2hGrid -> _'cons "h2h"
      _ -> pure unit
  getHash [] = Nothing
  getHash ids = Just $ str'joinWith "~" $ ids <#> show

urlParse'impl :: Int -> HashMap String Int -> URL -> Either String Route
urlParse'impl defaultSeasonId seasonIdBySlug u =
  sync'x $ e'try $ s'exec (route'mk defaultSeasonId Stats []) do
    x'out { u, segs: url'pathSegments u }
    processClmIds $ url'hash u
    processRouteList false $ list'fromFoldable $ url'pathSegments u
  where
  processClmIds (Just hashStr) = do
    clmIds <- forM (str'split (Pattern "~") hashStr) \idStr -> do
      case (tryParseInt idStr) of
        Just clmId -> pure clmId
        _ -> e'fail $ "clmIds must be integers separated by `~`: " <> hashStr
    s'set'b @"!.clmIds" clmIds
  processClmIds _ = pure unit
  processRouteList _ Nil = pure unit
  processRouteList _ (Cons "-" Nil) = s'set'b @"!.page" Player
  processRouteList _ (Cons "compare" Nil) = s'set'b @"!.page" Compare
  processRouteList _ (Cons "h2h" Nil) = s'set'b @"!.page" H2hGrid
  processRouteList false (Cons slug rest) =
    case (hm'lookup slug seasonIdBySlug) of
      Just seasonId -> do
        s'set'b @"!.seasonId" seasonId
        processRouteList true rest
      _ -> e'fail $ "Unknown Slug: " <> slug
  processRouteList _ _ = e'fail "Invalid Route"

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
