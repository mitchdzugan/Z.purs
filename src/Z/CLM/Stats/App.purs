module Z.CLM.Stats.App
  ( app'mk
  ) where

import Z.XDom.Prelude

import Z.CLM.Stats.Route
  ( Route
  , RoutePage(..)
  , route'isH2hPage
  , route'isStatsPage
  , route'mk
  , route'urlEdits
  , route'urlParse
  )
import Z.CLM.Stats.Season (seasonId'title)
import Z.CLM.Stats.Types
  ( ClmBuildOutputs
  , ClmPlayer
  , ClmSeason'Event
  , ClmSeason'Json
  )
import Z.XDom.UrlState as UrlSt
import Z.Z.X.Responds (Responds'Const, responds'run)

-----------------------------------------------------------------------

foreign import data ClmSeason'Tag :: Type

type ClmSeason'Responds = VariantF
  ( events :: Responds'Const (HashMap SorN ClmSeason'Event)
  , players :: Responds'Const (HashMap Int ClmPlayer)
  )

instance Boxable ClmSeason'Tag ClmSeason'Json ClmSeason'Responds where
  boxable'mkResponds b = varf'match
    { events: responds'run \_ -> keyBy _.eventId $ (boxed'v b).events
    , players: responds'run \_ -> keyBy _.clmId $ (boxed'v b).players
    }

type ClmSeason'Boxed = Boxed ClmSeason'Tag

-----------------------------------------------------------------------

type ClmBuildOutputs'X x = (buildOutputs :: X'R ClmBuildOutputs | x)

type ClmDom'X x = Router' String Route $ ClmBuildOutputs'X
  ( clmSeasons :: X'FetchCache Int ClmSeason'Boxed
  , route :: X'R Route
  , routeErr :: X'R (Maybe String)
  , editRoute :: X'R (Edit Route -> Route)
  | x
  )

type ClmDom dr x a = XDom dr (ClmDom'X x) a

app'body :: forall dr x. ClmDom dr x Unit
app'body = do
  route <- x'extract @"route"
  editRoute <- x'extract @"editRoute"
  let seasonId = g_ @"!.seasonId" route
  let title = seasonId'title seasonId
  seasonOr_ <- domFetchCache'lookup @"clmSeasons" seasonId
  whenJust seasonOr_ \season -> do
    let seasonEvents = boxed'call_ @"events" season
    x'out { season }
    x'out { seasonEvents }
  let isH2hPage = route'isH2hPage route
  let isStatsPage = route'isStatsPage route
  dom'div do
    el'cnW \w -> do
      w "container overflow-hidden max-w-290 rounded-none min-h-screen mx-auto"
      w "px-0 card bg-base-100 shadow-xl m-4 my-0"
    dom'div do
      el'cnW \w -> do
        w "w-auto max-w-screen min-h-screen max-h-screen"
        whenNot isH2hPage $ w "overflow-scroll"
      dom'div do
        el'cnW \w -> do
          w "flex max-w-screen flex-col self-stretch sticky z-750 left-0 top-0"
        dom'div do
          el'id "navbar"
          el'cnW \w -> do
            w "navbar h-15 border-b-2 border-gray-300 dark:border-gray-700"
            w "relative z-50 p-2 py-0 bg-base-200 shadow-sm"
          dom'div do
            el'cn "navbar-start relative w-auto gap-1 lg:gap-2"
          dom'div do
            el'cnW \w -> do
              w "lg:hidden border-gray-100 dark:border-gray-900 rounded-md"
              w "absolute -top-px left-0 w-27"
              w $ if isStatsPage then "border-0" else "border-1"
          whenNot isStatsPage do
            dom'a do
              router'setHref $ editRoute $ s'set'b @"!.page" Stats
              el'roleButton
              el'targetBlank
              el'cnW \w -> do
                w "btn btn-soft btn-outline px-0 w-full"
                -- TODO animateInCn
                w "border-gray-300 dark:border-gray-700 whitespace-nowrap"
              dom'pre $ dom'text "⬑"
              dom'span $ el'cn "text-xs" *> dom'text title

app'mk :: forall dr x. ClmBuildOutputs -> UrlSt.XProvider dr x -> XDom dr x Unit
app'mk buildOutputs = router'run urlEdits parseUrl mkTitleOr_ do
  route <- router'routeOrE <#> hush <#> may'or defaultRoute
  routeErr <- router'routeOrE <#> blush
  let editRoute = edit route
  id
    $ domR'run'' @"buildOutputs" buildOutputs
    $ domR'run'' @"route" route
    $ domR'run'' @"routeErr" routeErr
    $ domR'run'' @"editRoute" editRoute
    $ domFetchCache'run_ @"clmSeasons" mkClmSeasonFetchUrl getClmSeasonIds
    $ app'body
  where
  mkClmSeasonFetchUrl id = "/db/" <:> dbHash <:> "/seasons/" <:> id <:> ".json"
  getClmSeasonIds = x'extract @"route" <#> g_ @"!.seasonId" <#> pure
  mkTitleOr_ = Just <<< url'toString <<< _.url
  dbHash = buildOutputs.dbHash
  defaultSeasonId = buildOutputs.timeline.defaultSeasonId
  parseUrl = route'urlParse buildOutputs
  urlEdits = route'urlEdits defaultSeasonId
  defaultRoute = route'mk defaultSeasonId Stats []
