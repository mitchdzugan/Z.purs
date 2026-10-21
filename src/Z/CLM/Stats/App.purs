module Z.CLM.Stats.App
  ( app'mk
  ) where

import Z.XDom.Prelude

import Z.CLM.Stats.Icon
  ( icon'bars
  , icon'calendar
  , icon'chartColumn
  , icon'magnifyingGlass
  , icon'minus
  , icon'sort
  , icon'sortDown
  , icon'sortUp
  , icon'tableCells
  , icon'turnDown
  , icon'user
  , icon'userGroup
  , icon'xmark
  )
import Z.CLM.Stats.Route
  ( Filters
  , PlayerTab
  , Route
  , RoutePage(..)
  , Sort
  , SortBy(..)
  , route'gotoPlayer
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
  x'out { isInitialPage }
  let shouldUse = (not isInitialPage) || shouldForce
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

nbsp :: String
nbsp = "\x00A0"

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
        dom'text $ nbsp <> text

playerSearch :: forall dx x. ClmDom dx x Unit
playerSearch = do
  route <- x'extract @"route"
  let isStatsPage = route'isStatsPage route
  dom'div do
    el'cnW \w -> do
      w "dropdown"
    dom'label do
      el'cnW \w -> do
        w "group input h-9 border-0 px-2 rounded-none"
        w "transition transition-transform duration-300 shadow-none"
        w "relative outline-hidden focus-within:outline-solid"
        if isStatsPage then w "translate-x-8" else w "translate-x-0"
      el'style $ obj'fromFoldable
        [ "outlineOffset" /\ "-6px"
        , "outlineWidth" /\ "2px"
        ]
      icon'magnifyingGlass $ el'cn "size-4"
      dom'input do
        el'tabIndex0
        el'type "text"
      dom'div do
        el'key "outline-spacer"
        el'cnW \w -> do
          w "hidden absolute top-[5px] right-[calc(2rem_-_1px)]"
          w "w-[5px] h-[calc(100%_-_10px)]"
          when isStatsPage $ w "group-focus-within:block"
        el'style $ obj'fromFoldable
          [ "outlineStyle" /\ "inherit"
          , "outlineOffset" /\ "0px"
          , "outlineWidth" /\ "0px"
          ]
        dom'div do
          el'cnW \w -> do
            w "absolute w-0 h-full top-0 -right-px"
            w "outline-white dark:outline-black"
          el'style $ obj'fromFoldable
            [ "outlineStyle" /\ "inherit"
            , "outlineOffset" /\ "0px"
            , "outlineWidth" /\ "5px"
            ]
        dom'div do
          el'cnW \w -> do
            w "absolute w-0 h-full top-0 left-px"
            w "outline-black dark:outline-white"
          el'style $ obj'fromFoldable
            [ "outlineStyle" /\ "inherit"
            , "outlineOffset" /\ "0px"
            , "outlineWidth" /\ "1px"
            ]
    dom'div do
      pure unit

type TableCell dx x = MDom dx (ATTREL $ ATTREL $ ATTREL $ ATTREL x) Unit

type TableCells dx x = Array (TableCell dx x)

pageTableRow
  :: forall dx x. String -> String -> TableCells dx x -> MDom dx x Unit
pageTableRow rowKey rowCn cells = do
  dom'div do
    el'key rowKey
    el'cn $ "table text-base w-[100%] min-w-240 " <> rowCn
    dom'div do
      el'cn "table-row"
      dom'div $ el'cn "relative table-cell w-22" *> cell 0
      dom'div $ el'cn "relative table-cell min-w-80 w-auto" *> cell 1
      dom'div $ el'cn "relative table-cell w-28" *> cell 2
      dom'div $ el'cn "relative table-cell w-28" *> cell 3
      dom'div $ el'cn "relative table-cell w-28" *> cell 4
      dom'div do
        el'style $ obj'fromFoldable [ "width" /\ "max(16rem, 18vw)" ]
        el'cn "relative table-cell"
        cell 5
  where
  cell i = do
    let isStart = i == 1 || i == 5
    let mcell = may'or pass $ nth cells i
    dom'div do
      el'cnW \w -> do
        w "left-0 top-0 absolute h-full w-full flex items-center"
        if isStart then w "justify-start" else w "justify-center"
        when (i == 5) do w "px-1"
      mcell

page'stats :: forall dx x. Sort -> Filters -> ClmDom dx x Unit
page'stats sort filters = do
  animateInCn <- getAnimateInCn false "animate-flip-down"
  dom'div do
    el'cn "sticky z-30 top-16"
    pageTableRow
      "headers"
      ("h-10 rounded-none border-b-2 bg-base-300" <+> borderG3 <+> animateInCn)
      [ dom'span $ el'cn "font-bold" *%% "#"
      , tableHead SortBy'Name
      , tableHead SortBy'Qual
      , tableHead SortBy'Acc
      , tableHead SortBy'Att
      , tableHead SortBy'Mru
      ]
  dom'div do
    el'cn "relative"
  where
  tableHead sortBy = do
    editRoute <- x'extract @"editRoute"
    let s4 = "size-4"
    let isCurr = sort.by == sortBy
    let isPos = sort.dir == Pos
    let dir = if isCurr then pairKey'inv sort.dir else sortBy'defaultDir sortBy
    let targetSort = { dir, by: sortBy }
    dom'a do
      router'setHref $ editRoute $ route'setSort targetSort
      el'cnW \w -> do
        w "group transition ease-in-out duration-300 whitespace-nowrap"
        w "flex font-bold hover:underline hover:text-primary items-center"
        when isCurr $ w "text-primary underline"
      dom'text $ sortBy'title sortBy <> nbsp
      dom'span do
        el'cn "relative"
        dom'span do
          el'cn $ isCurr ? "group-hover:opacity-25" $ "group-hover:opacity-0"
          if isCurr then
            isPos ? icon'sortUp (el'cn s4) $ icon'sortDown (el'cn s4)
          else icon'sort (el'cn s4)

page'player :: forall dx x. PlayerTab -> ClmDom dx x Unit
page'player tab = do
  pure unit

page'compare :: forall dx x. ClmDom dx x Unit
page'compare = do
  pure unit

page'h2h :: forall dx x. ClmDom dx x Unit
page'h2h = do
  pure unit

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
                  router'setHref $ editRoute route'gotoStats
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
                    router'setHref $ editRoute $
                      route'toggleFilter @"insufficientAttendance"
                    el'data "tip" "test tooltip" -- TODO tooltip
                    dom'button do
                      el'tabIndex0
                      el'roleButton
                      el'cnW \w -> do
                        w "btn btn-sm h-full btn-soft rounded-r-none"
                        w "rounded-l-xs p-px w-8 border-0 hover:bg-base-200"
                        w "transition transition-colors duration-500 relative"
                      dom'div do
                        el'cn "relative"
                        dom'div do
                          el'cnW \w -> do
                            w "absolute -bottom-3 left-0 w-full h-full"
                            w "flex justify-center align-center"
                          dom'label do
                            el'cn "swap swap-rotate h-3 w-3"
                            dom'input do
                              el'type "checkbox"
                            dom'div do
                              el'cn "swap-on text-accent"
                              icon'minus $ el'cn "h-3 w-3 opacity-50"
                            dom'div do
                              el'cn "swap-off text-secondary"
                              icon'xmark $ el'cn "h-3 w-3"
                        icon'turnDown do
                          el'cn
                            "w-4 h-4 relative -top-1 border-b-0 text-warning"
                playerSearch
              dom'div do
                el'key $ route'pageKey route
                el'cnW \w -> do
                  w "lg:hidden dropdown dropdown-hover dropdown-end"
                  w "join-item border" *> w'borderG3
                dom'button do
                  el'tabIndex0
                  el'roleButton
                  el'cn "btn px-1 btn-sm h-full btn-soft rounded-none border-0"
                  icon'bars pass
                pure unit -- TODO periodMenu
              dom'div do
                el'key $ show $ g_ @"!.seasonId" route
                el'cnW \w -> do
                  w "dropdown dropdown-hover dropdown-end"
                  w "join-item border rounded-r-md" *> w'borderG3
                dom'button do
                  el'tabIndex0
                  el'roleButton
                  el'cnW \w -> do
                    w "btn px-1 btn-sm h-full btn-soft"
                    w "rounded-none border-0 rounded-r-sm"
                  icon'calendar pass
                pure unit -- TODO periodMenu
      case g_ @"!.page" route of
        Stats sort filters -> page'stats sort filters
        Player tab -> page'player tab
        Compare -> page'compare
        H2hGrid -> page'h2h

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
      isStillInitialPage = isStillInitialSeason && isInitialPage &&
        (g_ @"!.page" route == g_ @"!.page" initialRoute)
    x'out { isStillInitialPage }
    x'out $ show route
    x'assign @"isInitials"
      { initialRoute
      , isInitialPage: isStillInitialPage
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
  defaultRoute = route'mkDefault defaultSeasonId
