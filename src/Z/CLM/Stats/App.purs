module Z.CLM.Stats.App
  ( app'mk
  ) where

import Z.XDom.Prelude

import Z.CLM.Stats.Icon
  ( icon'chartColumn
  , icon'tableCells
  , icon'user
  , icon'userGroup
  )
import Z.CLM.Stats.Route
  ( Route
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
  )
import Z.CLM.Stats.Season (seasonId'title)
import Z.CLM.Stats.Types
  ( ClmBuildOutputs
  , ClmPlayer
  , ClmPlayerSeason'Json
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

foreign import data ClmPlayerSeason'Tag :: Type

type ClmPlayerSeason'Responds = VariantF
  ()

instance
  Boxable
    ClmPlayerSeason'Tag
    ClmPlayerSeason'Json
    ClmPlayerSeason'Responds where
  boxable'mkResponds b = varf'match
    {}

type ClmPlayerSeason'Boxed = Boxed ClmPlayerSeason'Tag

-----------------------------------------------------------------------

type ClmBuildOutputs'X x = (buildOutputs :: X'R ClmBuildOutputs | x)

type ClmDom'X x = Router' String Route $ ClmBuildOutputs'X
  ( clmSeasons :: X'FetchCache Int ClmSeason'Boxed
  , clmPlayerSeasons :: X'FetchCache (Int /\ Int) ClmPlayerSeason'Boxed
  , route :: X'R Route
  , routeErr :: X'R (Maybe String)
  , editRoute :: X'R (Edit Route -> Route)
  , isInitials ::
      X'S
        { isInitialPage :: Boolean
        , isInitialTab :: Boolean
        , isInitialPlayer :: Boolean
        , isInitialSeason :: Boolean
        , initialRoute :: Route
        }
  | x
  )

type ClmDom dr x a = XDom dr (ClmDom'X x) a

getAnimateInCn :: forall dr x. Boolean -> String -> ClmDom dr x String
getAnimateInCn shouldForce animationCn = do
  { isInitialPage } <- x'extract @"isInitials"
  let shouldUse = not isInitialPage || shouldForce
  pure $ if shouldUse then "animate animate-once " <> animationCn else ""

getAnimateInCn_ :: forall dr x. ClmDom dr x String
getAnimateInCn_ = getAnimateInCn false "animate-jump-in"

borderG3 :: String
borderG3 = "border-gray-300 dark:border-gray-700"

w'borderG3 :: forall x. Wa' String x @@> Unit
w'borderG3 = w'say borderG3

borderG1 :: String
borderG1 = "border-gray-100 dark:border-gray-900"

w'borderG1 :: forall x. Wa' String x @@> Unit
w'borderG1 = w'say borderG1

menuCn :: Boolean -> String
menuCn isActive = w'str'sp \w -> do
  w "active:bg-primary/25 active:dark:bg-primary-content/25"
  w "active:text-base-content"
  when isActive do
    w "active bg-primary/25 dark:bg-primary-content/25"

periodMenu :: forall dx x. String -> ClmDom dx x Unit
periodMenu ulCn = do
  dom'ul do
    el'tabIndex (-1)
    el'cnW \w -> w "menu" *> w ulCn
    liEl route'gotoStats icon'chartColumn "Stats"
    liEl route'gotoPlayer icon'user "Players"
    liEl (s'set'b @"!.page" Compare) icon'userGroup "Compare"
    liEl (s'set'b @"!.page" H2hGrid) icon'tableCells "H2h"
  where
  liEl edits icon text = do
    currRoute <- x'extract @"route"
    editRoute <- x'extract @"editRoute"
    let linkedRoute = editRoute edits
    let isActive = route'isSamePage currRoute linkedRoute
    dom'li do
      el'key text
      dom'a do
        el'cn $ menuCn isActive
        router'setHref linkedRoute
        icon pass
        dom'text $ "\x00A0" <> text

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
  animateInCn <- getAnimateInCn_
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
            w "navbar h-15 border-b-2" *> w'borderG3
            w "relative z-50 p-2 py-0 bg-base-200 shadow-sm"
          dom'div do
            el'cn "navbar-start relative w-auto gap-1 lg:gap-2"
            dom'div do
              el'cnW \w -> do
                w "lg:hidden rounded-md" *> w'borderG1
                w "absolute -top-px left-0 w-27"
                w $ if isStatsPage then "border-0" else "border-1"
              whenNot isStatsPage do
                dom'a do
                  router'setHref $ editRoute $ s'set'b @"!.page" Stats
                  el'roleButton
                  el'targetBlank
                  el'tabIndex0
                  el'cnW \w -> do
                    w $ "btn btn-soft btn-outline px-0 w-full" <> animateInCn
                    w "whitespace-nowrap" *> w'borderG3
                  dom'pre %% "⬑"
                  dom'span $ el'cn "text-xs" *%% title
            dom'a do
              el'href "https://chicagomelee.com/"
              el'targetBlank
              el'roleButton
              el'cnW \w -> do
                w "btn btn-ghost px-1 transition transition-transform"
                w if isStatsPage then "translate-x-0" else "translate-x-29"
                w "lg:translate-x-0"
              dom'img $ el'cn "w-8 h-8" *> el'src "/favicon.ico"
              dom'span $ el'cn "hidden lg:inline" *%% "CLM"
            dom'span $ el'cn
              ("w-27 lg:hidden " <> if isStatsPage then "hidden" else "inline")
            periodMenu "menu-horizontal hidden lg:flex px-1 gap-2"
            dom'div $ el'cn "w-1"
          dom'div do
            el'cnW \w -> do
              w "navbar-end flex-1 gap-4 overflow-hidden rounded-r-md"
              w "hover:overflow-visible focus-within:overflow-visible"
            dom'div do
              el'cnW \w -> do
                w "rounded-md flex items-stretch border-1"
                w "bg-gray-300 dark:bg-gray-700 border-white dark:border-black"
              dom'div do
                el'cnW \w -> do
                  w "group overflow-hidden relative rounded-l-sm border-1"
                  w "bg-base-300 focus-within:overflow-visible"
                  w "border-gray-300 dark:border-gray-700 border-r-transparent"
                  when isStatsPage $ w "hover:overflow-visible"
                dom'div do
                  el'cnW \w -> do
                    w "absolute top-0 left-0 h-full w-full"
                    whenNot isStatsPage $ w "group-focus-within:overflow-hidden"
                  dom'div do
                    el'cn "absolute z-15 right-[-2px] top-0 h-full"
                  -- TODO ActivePlayerIcons
                  dom'a do
                    dom'text "i"

app'mk :: forall dr x. ClmBuildOutputs -> UrlSt.XProvider dr x -> XDom dr x Unit
app'mk buildOutputs = router'run urlEdits parseUrl mkTitleOr_ do
  route <- router'routeOrE <#> hush <#> may'or defaultRoute
  routeErr <- router'routeOrE <#> blush
  let
    editRoute = edit route
    isPlayerPage = route'isPlayerPage route
    initIsInitials =
      { isInitialPage: true
      , isInitialSeason: true
      , isInitialPlayer: isPlayerPage
      , isInitialTab: isPlayerPage
      , initialRoute: route
      }
  domx'eval @"isInitials" initIsInitials do
    { initialRoute
    , isInitialPage
    , isInitialSeason
    , isInitialPlayer
    , isInitialTab
    } <- x'extract @"isInitials"
    let
      isStillInitialSeason = isInitialSeason &&
        (g_ @"!.seasonId" route == g_ @"!.seasonId" initialRoute)
    x'assign @"isInitials"
      { initialRoute
      , isInitialPage: isStillInitialSeason && isInitialPage &&
          (g_ @"!.page" route == g_ @"!.page" initialRoute)
      , isInitialSeason: isStillInitialSeason && isStillInitialSeason
      , isInitialPlayer: isStillInitialSeason && isInitialPlayer &&
          (route'playerPageId route == route'playerPageId route)
      , isInitialTab: isStillInitialSeason && isInitialTab &&
          (route'playerPageTab route == route'playerPageTab route)
      }
    domR'run'' @"buildOutputs" buildOutputs
      $ domR'run'' @"route" route
      $ domR'run'' @"routeErr" routeErr
      $ domR'run'' @"editRoute" editRoute
      $ domFetchCache'run_ @"clmSeasons" mkClmSeasonFetchUrl getClmSeasonIds
      $ domFetchCache'run_ @"clmPlayerSeasons"
          mkClmPlayerSeasonFetchUrl
          getClmPlayerSeasonIds
      $ app'body
  where
  mkClmSeasonFetchUrl id = "/db/" <:> dbHash <:> "/seasons/" <:> id <:> ".json"
  mkClmPlayerSeasonFetchUrl (clmId /\ seasonId) =
    "/db/" <:> dbHash <:> "/players/" <:> clmId <:> "/" <:> seasonId <:> ".json"
  getClmSeasonIds = x'extract @"route" <#> g_ @"!.seasonId" <#> pure
  getClmPlayerSeasonIds = x'extract @"route" <#> \r ->
    g_ @"!.clmIds" r <#> flip (/\) (g_ @"!.seasonId" r)
  mkTitleOr_ = Just <<< url'toString <<< _.url
  dbHash = buildOutputs.dbHash
  defaultSeasonId = buildOutputs.timeline.defaultSeasonId
  parseUrl = route'urlParse buildOutputs
  urlEdits = route'urlEdits defaultSeasonId
  defaultRoute = route'mk defaultSeasonId Stats []
