module Z.CLM.Stats.Route
  ( Filters
  , Filters'Key(..)
  , Filters'rt
  , PlayerTab(..)
  , Route(..)
  , RoutePage(..)
  , Sort
  , SortBy(..)
  , filters'pairs
  , page'key
  , page'sort
  , route'gotoPlayer
  , route'gotoSeason
  , route'gotoStats
  , route'isH2hPage
  , route'isPlayerPage
  , route'isSamePage
  , route'isStatsPage
  , route'mk
  , route'mkDefault
  , route'pageKey
  , route'playerPageId
  , route'playerPageTab
  , route'setSort
  , route'sort
  , route'toggleFilter
  , route'urlEdits
  , route'urlParse
  , sortBy'defaultDir
  , sortBy'title
  ) where

import Z.Prelude

import Z.CLM.Stats.Season (seasonId'active, seasonId'slug)
import Z.CLM.Stats.Types (ClmBuildOutputs)

data PlayerTab = PlayerTab'H2hs | PlayerTab'Tournaments

derive instance Generic PlayerTab _

derive instance Eq PlayerTab

instance Show PlayerTab where
  show = genericShow

data SortBy
  = SortBy'Name
  | SortBy'Qual
  | SortBy'Acc
  | SortBy'Att
  | SortBy'Mru

derive instance Generic SortBy _

derive instance Eq SortBy

instance Show SortBy where
  show SortBy'Name = "name"
  show SortBy'Qual = "qual"
  show SortBy'Acc = "acc"
  show SortBy'Att = "att"
  show SortBy'Mru = "mru"

instance Identable SortBy where
  ident'get = id'of <<< show

sortBy'title :: SortBy -> String
sortBy'title SortBy'Name = "Player"
sortBy'title SortBy'Qual = "Rating"
sortBy'title SortBy'Acc = "W - L"
sortBy'title SortBy'Att = "PR Events"
sortBy'title SortBy'Mru = "Last Event"

sortBy'vals :: Array SortBy
sortBy'vals = [ SortBy'Name, SortBy'Qual, SortBy'Acc, SortBy'Att, SortBy'Mru ]

sortBy'byKey :: HashMap String SortBy
sortBy'byKey = hm'fromFoldable $ sortBy'vals <#> \v -> show v /\ v

sortBy'parse :: String -> SortBy
sortBy'parse key = may'or defaultSort.by $ hm'lookup key sortBy'byKey

sortBy'defaultDir :: SortBy -> PairKey
sortBy'defaultDir SortBy'Name = Pos
sortBy'defaultDir _ = Neg

sortDir'parse :: SortBy -> String -> PairKey
sortDir'parse _ "asc" = Pos
sortDir'parse _ "desc" = Neg
sortDir'parse by _ = sortBy'defaultDir by

type Sort = { by :: SortBy, dir :: PairKey }

defaultSort :: Sort
defaultSort = { by: SortBy'Qual, dir: Neg }

type Filters'rt = (req'prEvents :: Boolean)
type Filters = Record Filters'rt

data Filters'Key = Filters'att

instance Show Filters'Key where
  show Filters'att = "att"

instance Identable Filters'Key where
  ident'get = id'of <<< show

filters'keys :: Array Filters'Key
filters'keys = [ Filters'att ]

filters'pairs :: Filters -> Array (Filters'Key /\ Boolean)
filters'pairs f = filters'keys <#> \k -> k /\ filters'get f k

filters'get :: Filters -> Filters'Key -> Boolean
filters'get f Filters'att = f.req'prEvents

filters'set :: Filters -> Filters'Key -> Boolean -> Filters
filters'set f Filters'att v = rec'set @"req'prEvents" v f

filters'toggle :: Filters -> Filters'Key -> Filters
filters'toggle f k = filters'set f k $ not $ filters'get f k

x'defaultFilters :: forall x. Int -> x @@> Filters
x'defaultFilters seasonId = do
  activeSeasonId <- seasonId'active
  pure { req'prEvents: activeSeasonId /= seasonId }

data RoutePage = Stats Sort Filters | Player PlayerTab | Compare | H2hGrid

derive instance Generic RoutePage _

derive instance Eq RoutePage

instance Show RoutePage where
  show = genericShow

page'key :: RoutePage -> String
page'key (Stats _ _) = "stats"
page'key (Player _) = "player"
page'key Compare = "compare"
page'key H2hGrid = "h2h"

page'sort :: RoutePage -> Sort
page'sort (Stats sort _) = sort
page'sort _ = defaultSort

x'defaultStatsPage :: forall x. Int -> x @@> RoutePage
x'defaultStatsPage seasonId = do
  defaultFilters <- x'defaultFilters seasonId
  pure $ Stats defaultSort defaultFilters

newtype Route = Route
  { page :: RoutePage, seasonId :: Int, clmIds :: Array Int }

derive instance Newtype Route _

derive instance Generic Route _

instance Show Route where
  show = genericShow

route'mk :: Int -> RoutePage -> Array Int -> Route
route'mk seasonId page clmIds = Route { page, seasonId, clmIds }

getActualPage :: forall x. Route -> x @@> RoutePage
getActualPage (Route { page, seasonId, clmIds }) =
  if isValidPage then pure page else getDefaultPage
  where
  idCount = arr'size clmIds
  getDefaultPage = case idCount of
    0 -> x'defaultStatsPage seasonId
    1 -> pure $ Player PlayerTab'H2hs
    2 -> pure Compare
    _ -> pure H2hGrid
  isValidPage = case page of
    Stats _ _ -> idCount == 0
    Player _ -> idCount < 2
    Compare -> idCount < 3
    _ -> true

sortDir'param :: PairKey -> String
sortDir'param Pos = "asc"
sortDir'param Neg = "desc"

route'urlEdits :: Int -> Route -> Edit UrlParts
route'urlEdits defaultSeasonId route@(Route { seasonId, clmIds }) = do
  s'set'b @"hash" (getHash clmIds)
  s'set'b @"path" path
  s'set'b @"query" query
  where
  path = sync'x $ _'exec_ @(X'Vector String) do
    whenNot (seasonId == defaultSeasonId) do _'cons $ seasonId'slug seasonId
    getActualPage route >>= case _ of
      Player _ -> _'cons "-"
      Compare -> _'cons "compare"
      H2hGrid -> _'cons "h2h"
      _ -> pure unit
  query = sync'x $ _'exec_ @(X'HashMap String (Array String)) do
    getActualPage route >>= case _ of
      Stats sort filters -> do
        defaultFilters <- x'defaultFilters seasonId
        forM_ (filters'pairs filters) \(filterKey /\ filterVal) -> do
          when (filterVal /= filters'get defaultFilters filterKey) do
            _'insert (show filterKey) [ if filterVal then "y" else "n" ]
        when (sort.by /= defaultSort.by) do
          _'insert "by" [ show sort.by ]
        when (sort.dir /= sortBy'defaultDir sort.by) do
          _'insert "dir" [ sortDir'param sort.dir ]
      Player PlayerTab'Tournaments -> _'insert "tab" [ "tournaments" ]
      _ -> pure unit
  getHash [] = Nothing
  getHash ids = Just $ str'joinWith "~" $ ids <#> show

route'mkDefault :: Int -> Route
route'mkDefault defaultSeasonId = route'mk defaultSeasonId defaultStatsPage []
  where
  defaultStatsPage = sync'x $ x'defaultStatsPage defaultSeasonId

urlParse'impl :: Int -> HashMap String Int -> URL -> Either String Route
urlParse'impl defaultSeasonId seasonIdBySlug u =
  sync'x $ e'try $ s'exec (route'mkDefault defaultSeasonId) do
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
        defaultFilters <- x'defaultFilters seasonId
        x'out { seasonId, defaultFilters }
        route'setFilters defaultFilters
        processRouteList true rest
      _ -> e'fail $ "Unknown Slug: " <> slug
  processRouteList _ _ = e'fail "Invalid Route"
  processQuery queryParams = do
    forM_ filters'keys \filterKey -> do
      case hm'lookup (show filterKey) queryParams of
        Just [ "y" ] -> route'setFilter filterKey true
        Just [ "n" ] -> route'setFilter filterKey false
        _ -> pure unit
    let sortByS = may'or "" $ arr'first =<< hm'lookup "by" queryParams
    let sortDirS = may'or "" $ arr'first =<< hm'lookup "dir" queryParams
    let sortBy = sortBy'parse sortByS
    let sortDir = sortDir'parse sortBy sortDirS
    route'setSort { by: sortBy, dir: sortDir }
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
route'isStatsPage (Route { page: Stats _ _ }) = true
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
  seasonId <- s'view'b @"!.seasonId"
  isStatsPage <- s'get <#> route'isStatsPage
  whenNot isStatsPage do
    s'set'b @"!.page" =<< x'defaultStatsPage seasonId
  s'set'b @"!.clmIds" []

route'gotoPlayer :: Edit Route
route'gotoPlayer = do
  isPlayerPage <- s'get <#> route'isPlayerPage
  whenNot isPlayerPage do
    s'set'b @"!.page" $ Player PlayerTab'H2hs
  s'over'b @"!.clmIds" $ arr'take 1

route'gotoSeason :: Int -> Edit Route
route'gotoSeason seasonId = do
  currSeasonId <- s'view'b @"!.seasonId"
  s'view'b @"!.page" >>= case _ of
    Stats _ filters -> do
      currDefaultFilters <- x'defaultFilters currSeasonId
      let isChanged (fk /\ fv) = fv /= filters'get currDefaultFilters fk
      let changed = arr'filter isChanged $ filters'pairs filters
      defaultFilters <- x'defaultFilters seasonId
      route'setFilters defaultFilters
      forM_ changed \(fk /\ fv) -> route'setFilter fk fv
    _ -> pure unit
  s'set'b @"!.seasonId" seasonId

route'isSamePage :: Route -> Route -> Boolean
route'isSamePage r1 r2 = route'pageKey r1 == route'pageKey r2

route'toggleFilter :: Filters'Key -> Edit Route
route'toggleFilter filtersKey = s'view'b @"!.page" >>= case _ of
  Stats sort filters ->
    s'set'b @"!.page" $ Stats sort $ filters'toggle filters filtersKey
  _ -> pure unit

route'setFilter :: forall x. Filters'Key -> Boolean -> Run (S' Route x) Unit
route'setFilter filtersKey filtersVal = s'view'b @"!.page" >>= case _ of
  Stats sort filters ->
    s'set'b @"!.page" $ Stats sort $ filters'set filters filtersKey filtersVal
  _ -> pure unit

route'setFilters :: forall x. Filters -> Run (S' Route x) Unit
route'setFilters filters = s'view'b @"!.page" >>= case _ of
  Stats sort _ ->
    s'set'b @"!.page" $ Stats sort filters
  _ -> pure unit

route'setSort :: forall x. Sort -> Run (S' Route x) Unit
route'setSort sort = s'view'b @"!.page" >>= case _ of
  Stats _ filters ->
    s'set'b @"!.page" $ Stats sort filters
  _ -> pure unit

route'pageKey :: Route -> String
route'pageKey (Route { page }) = page'key page

route'sort :: Route -> Sort
route'sort (Route { page }) = page'sort page