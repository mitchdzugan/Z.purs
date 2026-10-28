module Z.CLM.Stats.Route2
  ( Filter'Row
  , Filter(..)
  , IsH2hTab(..)
  , ManualFilters
  , Route
  , Route'SortByAndFilters
  , RoutePage(..)
  , Sort
  , SortBy'Row
  , SortBy(..)
  , filter'Pr
  , page'Compare
  , page'H2h
  , route'anyChangedFilters
  , route'getFilter
  , route'gotoPlayer
  , route'gotoSeason
  , route'gotoStats
  , route'isH2hPage
  , route'isPlayerPage
  , route'isSamePage
  , route'isSortByQual
  , route'isStatsPage
  , route'mkDefault
  , route'pageKey
  , route'playerPageId
  , route'playerTab
  , route'setSort
  , route'sort
  , route'sortByAndFilters
  , route'toggleFilter
  , route'urlEdits
  , route'urlParse
  , sortBy'Acc
  , sortBy'Att
  , sortBy'Mru
  , sortBy'Name
  , sortBy'Qual
  , sortBy'defaultDir
  , sortBy'title
  ) where

import Z.Prelude

import Data.Variant (Variant, onMatch)
import Z.CLM.Stats.Season (seasonId'active, seasonId'slug)
import Z.CLM.Stats.Types (ClmBuildOutputs)

newtype IsH2hTab = IsH2hTab Boolean

derive instance Newtype IsH2hTab _
derive newtype instance Eq IsH2hTab
derive newtype instance Show IsH2hTab

--------

newtype RoutePage = RoutePage
  ( Variant
      ( stats :: Unit
      , player :: IsH2hTab
      , compare :: Unit
      , h2hGrid :: Unit
      )
  )

derive instance Newtype RoutePage _
derive newtype instance Show RoutePage
derive newtype instance Eq RoutePage

page'playerTab :: RoutePage -> Maybe IsH2hTab
page'playerTab = unwrap >>> onMatch { player: Just } (const Nothing)

page'playerLimit :: RoutePage -> Maybe Int
page'playerLimit = unwrap >>> var'match
  { stats: const $ Just 0
  , player: const $ Just 1
  , compare: const $ Just 2
  , h2hGrid: const Nothing
  }

page'conformIds :: RoutePage -> Array Int -> Array Int
page'conformIds page = may'or id $ page'playerLimit page <#> arr'take

page'Player :: Boolean -> RoutePage
page'Player isH2hTab = wrap $ var'inj @"player" $ wrap isH2hTab

page'Stats :: RoutePage
page'Stats = wrap $ var'inj @"stats" unit

page'Compare :: RoutePage
page'Compare = wrap $ var'inj @"compare" unit

page'H2h :: RoutePage
page'H2h = wrap $ var'inj @"h2hGrid" unit

page'firstLegal :: Int -> RoutePage
page'firstLegal 1 = page'Player true
page'firstLegal 2 = wrap $ var'inj @"compare" unit
page'firstLegal _ = wrap $ var'inj @"h2hGrid" unit

page'key :: RoutePage -> String
page'key = unwrap >>> var'match
  { stats: const "stats"
  , player: const "player"
  , compare: const "compare"
  , h2hGrid: const "h2h"
  }

--------

type SortBy'Row =
  ( name :: Unit
  , qual :: Unit
  , acc :: Unit
  , att :: Unit
  , mru :: Unit
  )

newtype SortBy = SortBy (Variant SortBy'Row)

derive instance Newtype SortBy _
derive newtype instance Eq SortBy
instance Show SortBy where
  show = unwrap >>> var'match
    { name: const "name"
    , qual: const "qual"
    , acc: const "acc"
    , att: const "att"
    , mru: const "mru"
    }

instance Identable SortBy where
  ident'get = id'of <<< show

sortBy'mk :: forall @p x'. ConsSymbol p Unit x' SortBy'Row => SortBy
sortBy'mk = wrap $ var'inj @p unit

defaultSortBy :: SortBy
defaultSortBy = sortBy'Qual

sortBy'allVariants :: Array SortBy
sortBy'allVariants =
  [ sortBy'Name, sortBy'Qual, sortBy'Acc, sortBy'Att, sortBy'Mru ]

sortBy'Name :: SortBy
sortBy'Name = sortBy'mk @"name"

sortBy'Qual :: SortBy
sortBy'Qual = sortBy'mk @"qual"

sortBy'Acc :: SortBy
sortBy'Acc = sortBy'mk @"acc"

sortBy'Att :: SortBy
sortBy'Att = sortBy'mk @"att"

sortBy'Mru :: SortBy
sortBy'Mru = sortBy'mk @"mru"

sortBy'byKey :: HashMap String SortBy
sortBy'byKey = hm'fromFoldable $ sortBy'allVariants <#> \v -> show v /\ v

sortBy'parse :: String -> SortBy
sortBy'parse key = may'or defaultSortBy $ hm'lookup key sortBy'byKey

sortBy'defaultDir :: SortBy -> PairKey
sortBy'defaultDir = unwrap >>> onMatch { name: const pos } (const neg)

sortDir'parse :: SortBy -> String -> PairKey
sortDir'parse _ "asc" = pos
sortDir'parse _ "desc" = neg
sortDir'parse by _ = sortBy'defaultDir by

sortBy'title :: SortBy -> String
sortBy'title = unwrap >>> var'match
  { name: const "Player"
  , qual: const "Rating"
  , acc: const "W - L"
  , att: const "PR Events"
  , mru: const "Last Event"
  }

type Sort = { by :: SortBy, dir :: PairKey }

defaultSort :: Sort
defaultSort = { by: defaultSortBy, dir: sortBy'defaultDir defaultSortBy }

--------

type Filter'Row =
  ( pr :: Unit
  )

newtype Filter = Filter (Variant Filter'Row)

derive instance Newtype Filter _
derive newtype instance Eq Filter
instance Show Filter where
  show = unwrap >>> var'match { pr: const "pr" }

instance Identable Filter where
  ident'get = id'of <<< show

filter'defaultState :: Boolean -> Filter -> Boolean
filter'defaultState isActiveSeason =
  unwrap >>> var'match { pr: const $ not isActiveSeason }

filter'isActive :: Boolean -> ManualFilters -> Filter -> Boolean
filter'isActive isActiveSeason manuals filter = hm'lookup filter manuals
  # may'or (filter'defaultState isActiveSeason filter)

filter'allVariants :: Array Filter
filter'allVariants = [ filter'Pr ]

filter'Pr :: Filter
filter'Pr = wrap $ var'inj @"pr" unit

type ManualFilters = HashMap Filter Boolean

filters'active :: Boolean -> ManualFilters -> Array Filter
filters'active isActiveSeason manuals =
  arr'filter (filter'isActive isActiveSeason manuals) filter'allVariants

--------
activeSeasonId'static :: Int
activeSeasonId'static = sync'x seasonId'active

--------

type Route =
  { page :: RoutePage
  , seasonId :: Int
  , clmIds :: Array Int
  , sort :: Sort
  , manualFilters :: ManualFilters
  }

type Route'SortByAndFilters = SortBy /\ Array Filter

route'sortByAndFilters :: Route -> Route'SortByAndFilters
route'sortByAndFilters { sort, manualFilters, seasonId } =
  sort.by /\ filters'active (seasonId == activeSeasonId'static) manualFilters

route'pageKey :: Route -> String
route'pageKey = _.page >>> page'key

route'playerTab :: Route -> Maybe IsH2hTab
route'playerTab = _.page >>> page'playerTab

route'isPlayerPage :: Route -> Boolean
route'isPlayerPage = isJust <<< route'playerTab

route'isH2hPage :: Route -> Boolean
route'isH2hPage =
  _.page >>> unwrap >>> onMatch { h2hGrid: const true } (const false)

route'isStatsPage :: Route -> Boolean
route'isStatsPage =
  _.page >>> unwrap >>> onMatch { stats: const true } (const false)

route'isSortByQual :: Route -> Boolean
route'isSortByQual =
  _.sort.by >>> unwrap >>> onMatch { qual: const true } (const false)

route'playerPageId :: Route -> Maybe (Maybe Int)
route'playerPageId { page, clmIds } = unwrap page #
  onMatch { player: \_ -> Just $ arr'first clmIds } (const Nothing)

route'sort :: Route -> Sort
route'sort = _.sort

getActualPage :: Route -> RoutePage
getActualPage { page, clmIds } =
  if isValidPage then page else getDefaultPage
  where
  idCount = arr'size clmIds
  getDefaultPage = page'firstLegal idCount
  isValidPage = may'or true $ page'playerLimit page <#> (<=) idCount

getActualClmIds :: Route -> Array Int
getActualClmIds route@{ clmIds } =
  page'conformIds (getActualPage route) clmIds

sortDir'param :: PairKey -> String
sortDir'param pk = if pk == pos then "asc" else "desc"

route'urlEdits :: Int -> Route -> Edit UrlParts
route'urlEdits defaultSeasonId route@{ seasonId, sort, manualFilters } = do
  s'set'b @"hash" (getHash $ getActualClmIds route)
  s'set'b @"path" path
  s'set'b @"query" query
  where
  path = sync'x $ _'exec_ @(X'Vector String) do
    whenNot (seasonId == defaultSeasonId) do _'cons $ seasonId'slug seasonId
    flip (flip onMatch $ const $ pure unit) (unwrap $ getActualPage route)
      { player: \isH2hTab ->
          _'cons "-" *> whenNot (unwrap isH2hTab) (_'cons "events")
      , compare: \_ -> _'cons "compare"
      , h2hGrid: \_ -> _'cons "h2h"
      }
  query = sync'x $ _'exec_ @(X'HashMap String (Array String)) do
    forM_ (hm'entries manualFilters) \(filter /\ isActive) -> do
      _'insert (show filter) [ if isActive then "y" else "n" ]
    when (sort.by /= defaultSort.by) do
      _'insert "by" [ show sort.by ]
    when (sort.dir /= sortBy'defaultDir sort.by) do
      _'insert "dir" [ sortDir'param sort.dir ]
    pure unit
  getHash [] = Nothing
  getHash ids = Just $ str'joinWith "~" $ ids <#> show

route'mkDefault :: Int -> Route
route'mkDefault defaultSeasonId =
  { page: page'Stats
  , seasonId: defaultSeasonId
  , clmIds: []
  , sort: defaultSort
  , manualFilters: hm'empty
  }

urlParse'impl :: Int -> HashMap String Int -> URL -> Either String Route
urlParse'impl defaultSeasonId seasonIdBySlug u =
  sync'x $ e'try $ s'exec (route'mkDefault defaultSeasonId) do
    processRouteList false $ list'fromFoldable $ url'pathSegments u
    processQuery $ url'query u
    processClmIds $ url'hash u
  where
  readClmIds (Just hashStr) = do
    clmIds <- forM (str'split (Pattern "~") hashStr) \idStr -> do
      case (tryParseInt idStr) of
        Just clmId -> pure clmId
        _ -> e'fail $ "clmIds must be integers separated by `~`: " <> hashStr
    pure clmIds
  readClmIds _ = pure []
  processClmIds hash = do
    page <- s'view'b @"page"
    (=<<) (s'set'b @"clmIds") $ readClmIds hash <#> page'conformIds page
  processRouteList _ Nil = pure unit
  processRouteList _ (Cons "-" Nil) = s'set'b @"page" $ page'Player true
  processRouteList _ (Cons "-" (Cons "events" Nil)) =
    s'set'b @"page" $ page'Player false
  processRouteList _ (Cons "compare" Nil) = s'set'b @"page" page'Compare
  processRouteList _ (Cons "h2h" Nil) = s'set'b @"page" page'H2h
  processRouteList false (Cons slug rest) =
    case (hm'lookup slug seasonIdBySlug) of
      Just seasonId -> do
        s'set'b @"seasonId" seasonId
        processRouteList true rest
      _ -> e'fail $ "Unknown Slug: " <> slug
  processRouteList _ _ = e'fail "Invalid Route"
  processQuery queryParams = do
    manualFilters <- _'exec_ @(X'HashMap Filter Boolean) do
      forM_ filter'allVariants \filter -> do
        case hm'lookup (show filter) queryParams of
          Just [ "y" ] -> _'insert filter true
          Just [ "n" ] -> _'insert filter false
          _ -> pure unit
    s'set'b @"manualFilters" manualFilters
    let sortByS = may'or "" $ arr'first =<< hm'lookup "by" queryParams
    let sortDirS = may'or "" $ arr'first =<< hm'lookup "dir" queryParams
    let sortBy = sortBy'parse sortByS
    let sortDir = sortDir'parse sortBy sortDirS
    s'set'b @"sort" { by: sortBy, dir: sortDir }

route'urlParse :: ClmBuildOutputs -> URL -> Either String Route
route'urlParse { timeline } = urlParse'impl defaultSeasonId seasonIdBySlug
  where
  defaultSeasonId = timeline.defaultSeasonId
  allSeasonIds = arr'concat
    [ [ defaultSeasonId ], timeline.preDefault, timeline.postDefault ]
  seasonIdBySlug =
    hm'fromFoldable $ allSeasonIds <#> \id -> seasonId'slug id /\ id

route'gotoStats :: Edit Route
route'gotoStats = s'set'b @"page" page'Stats

route'gotoPlayer :: Edit Route
route'gotoPlayer = do
  isH2hTab <- s'get <#> route'playerTab <#> map unwrap <#> may'or true
  s'set'b @"page" (page'Player isH2hTab)

route'gotoSeason :: Int -> Edit Route
route'gotoSeason seasonId = do
  s'set'b @"seasonId" seasonId

route'isSamePage :: Route -> Route -> Boolean
route'isSamePage r1 r2 = route'pageKey r1 == route'pageKey r2

route'getFilter :: Filter -> Route -> Boolean
route'getFilter filter { manualFilters, seasonId } =
  filter'isActive (seasonId == activeSeasonId'static) manualFilters filter

route'toggleFilter :: Filter -> Edit Route
route'toggleFilter filter = do
  nextVal <- s'get <#> route'getFilter filter <#> not
  s'over'b @"manualFilters" $ hm'insert filter nextVal

route'anyChangedFilters :: Route -> Boolean
route'anyChangedFilters r = getActives r.manualFilters /= getActives hm'empty
  where
  isActiveSeason = r.seasonId == activeSeasonId'static
  getActives = filters'active isActiveSeason

route'setSort :: forall x. Sort -> Run (S' Route x) Unit
route'setSort = s'set'b @"sort"