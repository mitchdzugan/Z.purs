module Z.CLM.Stats.App
  ( app'mk
  ) where

import Z.XDom.Prelude

import Z.CLM.Stats.Icon
  ( IconSize(..)
  , icon'bars
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
  , iconBadge'activity
  )
import Z.CLM.Stats.Route
  ( Filters
  , Filters'Key(..)
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
  , route'mkDefault
  , route'pageKey
  , route'playerPageId
  , route'playerPageTab
  , route'setSort
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
  , ClmSeason'Player
  , ClmSeason'Player'Full
  , clm'hasRatingConfidence
  , clm'hasReq'prEvents
  )
import Z.H2h.Module as H2h
import Z.XDom.UrlState as UrlSt
import Z.Z.X.Responds (Responds, Responds'Const, responds'run)

-----------------------------------------------------------------------

foreign import data ClmSeason'Tag :: Type

data ClmSeason'Error
  = ClmSeason'Error'MissingPlayer Int
  | ClmSeason'Error'MissingEvent SorN

type Or'ClmSeason'Error t = Either ClmSeason'Error t

type ClmSeason'Players'Full = Array ClmSeason'Player'Full

type ClmSeason'Responds = VariantF
  ( events :: Responds'Const (HashMap SorN ClmSeason'Event)
  , players :: Responds'Const (HashMap Int ClmPlayer)
  , fullPlayers :: Responds'Const (HashMap Int ClmSeason'Player'Full)
  , playerRanks :: Responds'Const (HashMap Int ClmSeason'Player)
  , getIsInUse :: Responds Int (Filters -> Boolean)
  , ranksOrE ::
      Responds'Const (Or'ClmSeason'Error ClmSeason'Players'Full)
  , ranksOrESortedBy ::
      Responds SortBy (Or'ClmSeason'Error ClmSeason'Players'Full)
  )

instance Boxable ClmSeason'Tag ClmSeason'Json ClmSeason'Responds where
  boxable'mkResponds b = varf'match
    { events: responds'run \_ -> keyBy _.eventId $ (boxed'v b).events
    , players: responds'run \_ -> keyBy _.clmId $ (boxed'v b).players
    , fullPlayers: responds'run \_ -> case (boxed'call_ @"ranksOrE" b) of
        Left _ -> hm'empty
        Right ranks -> keyBy _.clmId ranks
    , getIsInUse: responds'run \clmId -> \filters ->
        case (hm'lookup clmId $ boxed'call_ @"fullPlayers" b) of
          Nothing -> false
          Just p ->
            p.hasReq'prEvents || not filters.req'prEvents
    , playerRanks: responds'run \_ -> keyBy _.clmId $ (boxed'v b).ranks
    , ranksOrE: responds'run \_ -> sync'x do
        x'out "calculating ranks"
        let eventsById = boxed'call_ @"events" b
        let playersById = boxed'call_ @"players" b
        e'try $ forM (boxed'v b).ranks \rank -> do
          let playerErr = ClmSeason'Error'MissingPlayer rank.clmId
          let eventErr = ClmSeason'Error'MissingEvent rank.lastEvent.id
          player <- e'unwrap playerErr $ hm'lookup rank.clmId playersById
          evData <- e'unwrap eventErr $ hm'lookup rank.lastEvent.id eventsById
          let sets = rank.wins + rank.losses
          let numWs = toNumber rank.wins
          let numTot = toNumber sets
          let winrate = if (sets == 0) then Nothing else Just (numWs / numTot)
          pure
            $ rec'insert @"player" player
            $ rec'insert @"sets" sets
            $ rec'insert @"winrate" winrate
            $ rec'insert @"hasReq'prEvents" (clm'hasReq'prEvents rank)
            $ rec'insert @"hasRatingConfidence" (clm'hasRatingConfidence rank)
            $ rec'modify @"lastEvent" (\ev -> rec'insert @"data" evData ev)
            $ rank
    , ranksOrESortedBy: responds'run \sortBy -> do
        ranks <- boxed'call_ @"ranksOrE" b
        pure $ flip multisort ranks $ case sortBy of
          SortBy'Name -> [ sorter _.player.gamerTag ]
          SortBy'Qual -> []
          SortBy'Acc -> [ sorter _.winrate, sorter _.sets ]
          SortBy'Att -> [ sorter _.prEvents, sorter _.sets ]
          SortBy'Mru -> [ sorter _.lastEvent.data.date ]
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
      XDom'Ref
        { isInitialPage :: Boolean
        , isInitialTab :: Boolean
        , isInitialPlayer :: Boolean
        , isInitialSeason :: Boolean
        , initialRoute :: Route
        }
  | x
  )

type ClmDom dx x a = XDom dx (ClmDom'X x) a

data ClmDom'Err
  = ClmDom'Err'LoadSeason Int JsError
  | ClmDom'Err'FullSeason Int ClmSeason'Error

type ClmDomE dx x a = XDom (DomE' ClmDom'Err dx) (ClmDom'X x) a

getAnimateInCn :: forall dx x. Boolean -> String -> ClmDom dx x String
getAnimateInCn shouldForce animationCn = do
  { isInitialPage } <- domRef'get'' @"isInitials"
  let shouldUse = (not isInitialPage) || shouldForce
  pure $ if shouldUse then "animate animate-once " <> animationCn else ""

getAnimateInCn_ :: forall dx x. ClmDom dx x String
getAnimateInCn_ = getAnimateInCn false "animate-jump-in"

borderG3 :: String
borderG3 = "border-gray-300 dark:border-gray-700"

w'BorderG3 :: forall dx x. MDomEl dx x Unit
w'BorderG3 = el'AddCn borderG3

borderG1 :: String
borderG1 = "border-gray-100 dark:border-gray-900"

w'BorderG1 :: forall dx x. MDomEl dx x Unit
w'BorderG1 = el'AddCn borderG1

menuCn :: Boolean -> String
menuCn isActive = w'str'sp \w -> do
  w "active:bg-primary/25 active:dark:bg-primary-content/25"
  w "active:text-base-content"
  when isActive do w "active bg-primary/25 dark:bg-primary-content/25"

nbsp :: String
nbsp = "\x00A0"

periodMenu :: forall dx x. String -> ClmDom dx x Unit
periodMenu ulCn = do
  dom'Ul do
    el'TabIndex (-1)
    el'AddCn "menu"
    el'AddCn ulCn
    pure do
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
    dom'Li do
      el'Key text
      pure $ dom'A do
        el'Cn $ menuCn isActive
        router'setHref linkedRoute
        pure do icon pass *> do dom'text $ nbsp <> text

playerSearch :: forall dx x. ClmDom dx x Unit
playerSearch = do
  route <- x'extract @"route"
  let isStatsPage = route'isStatsPage route
  dom'Div do
    el'AddCn "dropdown"
    pure do
      dom'Label do
        el'AddCn "group input h-9 border-0 px-2 rounded-none"
        el'AddCn "transition transition-transform duration-300 shadow-none"
        el'AddCn "relative outline-hidden focus-within:outline-solid"
        el'AddCn $ if isStatsPage then "translate-x-8" else "translate-x-0"
        el'SetStyles
          [ "outlineOffset" /\ "-6px"
          , "outlineWidth" /\ "2px"
          ]
        pure do
          icon'magnifyingGlass $ s'put Size4
          dom'Input_ do
            el'TabIndex0
            el'Type "text"
          dom'Div do
            el'Key "outline-spacer"
            el'AddCn "hidden absolute top-[5px] right-[calc(2rem_-_1px)]"
            el'AddCn "w-[5px] h-[calc(100%_-_10px)]"
            when isStatsPage $ el'AddCn "group-focus-within:block"
            el'SetStyles
              [ "outlineStyle" /\ "inherit"
              , "outlineOffset" /\ "0px"
              , "outlineWidth" /\ "0px"
              ]
            pure do
              dom'Div_ do
                el'AddCn "absolute w-0 h-full top-0 -right-px"
                el'AddCn "outline-white dark:outline-black"
                el'SetStyles
                  [ "outlineStyle" /\ "inherit"
                  , "outlineOffset" /\ "0px"
                  , "outlineWidth" /\ "5px"
                  ]
              dom'Div_ do
                el'AddCn "absolute w-0 h-full top-0 left-px"
                el'AddCn "outline-black dark:outline-white"
                el'SetStyles
                  [ "outlineStyle" /\ "inherit"
                  , "outlineOffset" /\ "0px"
                  , "outlineWidth" /\ "1px"
                  ]
      dom'Div_ do pure unit

type TableCell dx x = MDom dx x Unit

type TableCells dx x = Array (TableCell dx x)

pageTableRow
  :: forall dx x. String -> String -> TableCells dx x -> MDom dx x Unit
pageTableRow rowKey rowCn cells = do
  dom'Div do
    el'Key rowKey
    el'Cn $ "table text-base w-[100%] min-w-240 " <> rowCn
    pure $ dom'Div do
      el'Cn "table-row"
      pure do
        dom'Div $ el'Cn "relative table-cell w-22" *> cell 0
        dom'Div $ el'Cn "relative table-cell min-w-80 w-auto" *> cell 1
        dom'Div $ el'Cn "relative table-cell w-28" *> cell 2
        dom'Div $ el'Cn "relative table-cell w-28" *> cell 3
        dom'Div $ el'Cn "relative table-cell w-28" *> cell 4
        dom'Div do
          el'SetStyles [ "width" /\ "max(16rem, 18vw)" ]
          el'Cn "relative table-cell"
          cell 5
  where
  cell i = do
    let isStart = i == 1 || i == 5
    let mcell = may'or pass $ nth cells i
    pure $ dom'Div do
      el'AddCn "left-0 top-0 absolute h-full w-full flex items-center"
      el'AddCn $ if isStart then "justify-start" else "justify-center"
      when (i == 5) do el'AddCn "px-1"
      pure mcell

type TableRowProps dx x =
  { key :: String
  , cn :: String
  , ord :: ClmDom dx x Unit
  , name :: ClmDom dx x Unit
  , profileImage :: ClmDom dx x Unit
  , pronouns :: Maybe $ ClmDom dx x Unit
  , characters :: Maybe $ ClmDom dx x Unit
  , qual :: ClmDom dx x Unit
  , acc :: ClmDom dx x Unit
  , att :: ClmDom dx x Unit
  , attTooltip :: Maybe $ ClmDom dx x Unit
  , mruLink :: ClmDom dx x Unit
  , mruPlacing :: ClmDom dx x Unit
  , mruDate :: ClmDom dx x Unit
  }

tableContentRow
  :: forall dx x. Maybe Int -> TableRowProps dx x -> ClmDom dx x Unit
tableContentRow clmIdOr_ props = do
  let baseCn = "h-25 rounded-none" <+> borderG3
  let fullCn = baseCn <+> props.cn
  pageTableRow props.key fullCn
    [ dom'Div do
        el'Cn "font-extrabold text-4xl"
        pure props.ord
    , do
        editRoute <- x'extract @"editRoute"
        let
          topCn = w'str'sp \w -> do
            w "flex items-center justify-start flex-1"
            when (isJust clmIdOr_) $ w "group"
          useBody tag m = tag $ el'Cn topCn *> m *> pure do
            dom'Div_ $ el'Cn "w-2"
            dom'Div do
              el'AddCn "h-13 w-13 rounded-full overflow-hidden shadow-md"
              el'AddCn "transition transition-shadow transition-colors"
              el'AddCn "duration-300 group-hover:shadow-primary/33"
              pure props.profileImage
            dom'Div_ $ el'Cn "w-4"
            dom'Div do
              el'Cn
                "flex flex-col justify-center flex-1 relative overflow-hidden"
              pure do
                dom'Div do
                  el'AddCn "whitespace-nowrap text-xl font-bold"
                  el'AddCn "text-primary group-hover:underline"
                  pure props.name
                whenJust props.pronouns \pronouns -> dom'Div do
                  el'Cn "text-sm"
                  pure pronouns
                whenJust props.characters \characters -> dom'Div do
                  pure characters
        case clmIdOr_ of
          Just clmId -> do
            useBody dom'A $ router'setHref
              (editRoute $ route'gotoPlayer *> s'setPure'b @"!.clmIds" clmId)
          _ -> useBody dom'Div $ pure unit
    , dom'Div $ pure props.qual
    , dom'Div $ pure props.acc
    , dom'Div do
        el'Cn "h-full w-full flex justify-center items-center"
        when (isJust props.attTooltip) do
          el'Cn "dropdown dropdown-hover dropdown-bottom dropdown-end"
        pure do
          props.att
          whenJust props.attTooltip \attTooltip -> do
            dom'Div do
              el'AddCn "dropdown-content rounded shadow-sm"
              el'AddCn "p-2 w-76 bg-base-200 z-50"
              pure attTooltip
    , dom'Div do
        el'Cn "flex flex-col overflow-hidden"
        pure do
          dom'Div do
            el'Cn "overflow-hidden whitespace-nowrap text-ellipsis"
            pure props.mruLink
          dom'Div do
            el'Cn "overflow-hidden whitespace-nowrap text-sm text-ellipsis"
            pure props.mruPlacing
          dom'Div do
            el'Cn "overflow-hidden whitespace-nowrap text-sm text-ellipsis"
            el'AddCn "opacity-75"
            pure props.mruDate
    ]

getClmRng :: forall dx x. String -> ClmDom dx x (Array String -> Number)
getClmRng key = do
  { clmIds, seasonId } <- x'extract @"route" <#> unwrap
  let rngEls a = arr'concat [ a, [ key, show seasonId ], map show clmIds ]
  pure $ pseudoRng <<< rngEls

rngSkel :: forall dx x. String -> String -> Number -> Number -> ClmDom dx x Unit
rngSkel key cn l h = do
  clmRng <- getClmRng key
  let trunc3 n = flip (/) 1000.0 $ toNumber $ floor (n * 1000.0)
  let skelRng = clmRng [ cn, show l, show h ]
  let skelSize = show $ trunc3 $ l + ((h - l) * skelRng)
  let skelWidth = skelSize <> "rem"
  let skelStyle = [ "width" /\ skelWidth ]
  dom'Div_ $ el'AddCn "skeleton" *> el'AddCn cn *> el'SetStyles skelStyle

tableSkelRow :: forall dx x. Int -> ClmDom dx x Unit
tableSkelRow id = do
  let key = "skelRow" <-> id
  clmRng <- getClmRng key
  let rngGate s lim m = if clmRng [ s ] > lim then Nothing else Just m
  tableContentRow Nothing
    { key
    , cn: "h-25 rounded-none" <+> borderG3
    , ord: dom'Div_ $ el'Cn "skeleton h-6 w-10"
    , profileImage: dom'Div_ $ el'Cn "skeleton h-full w-full"
    , name: rngSkel key "h-4" 5.0 7.5
    , pronouns: rngGate "pronouns" 0.7 $ rngSkel key "h-3" 2.0 2.5
    , characters: rngGate "char" 0.8 $ dom'Div_ $ el'Cn "skeleton h-3 w-3"
    , qual: rngSkel key "h-4" 2.75 3.25
    , acc: dom'Div $ el'Cn "flex gap-1" *> pure do
        rngSkel key "h-4" 1.201 1.799
        rngSkel key "h-4" 1.199 1.801
    , att: rngSkel key "h-4" 2.0 2.55
    , attTooltip: Nothing
    , mruLink: rngSkel key "h-4" 4.25 7.25
    , mruPlacing: rngSkel key "h-3" 1.85 2.15
    , mruDate: rngSkel key "h-3" 1.35 1.65
    }

eventUrl :: ClmSeason'Event -> String
eventUrl { source: { site, slug } } = case site of
  H2h.Startgg -> "https://start.gg/" <> slug
  H2h.Challonge -> "https://challonge.com/" <> slug

tablePlayerRow :: forall dx x. ClmSeason'Player'Full -> ClmDom dx x Unit
tablePlayerRow p = do
  tableContentRow (Just p.clmId)
    { key: "player." <:> p.clmId
    , cn: ""
    , ord: dom'text $ may'or "-" $ p.clmRank <#> show
    , profileImage: dom'Div_ $ el'Cn "h-full w-full"
    , name: dom'text $ p.player.gamerTag
    , pronouns: Just $ dom'text $ "xD"
    , characters: Just $ dom'text $ "xD"
    , qual: dom'text $ p.rating
    , acc: dom'text $ p.wins <+> "-" <+> p.losses
    , att: dom'Div do
        el'Cn "flex items-center"
        pure do
          dom'text $ p.prEvents
          dom'Div $ (*>) (el'Cn "relative") $ pure $ dom'Div do
            el'Cn "absolute top-0 left-0 flex items-center"
            pure $ whenNot p.hasReq'prEvents do
              dom'Span do
                el'Cn "flex items-center relative bottom-4"
                pure $ dom'text nbsp *> iconBadge'activity (s'put Size4)
    , attTooltip: Nothing
    , mruLink: dom'A do
        el'Cn "nowrap text-primary hover:underline"
        el'TargetBlank
        el'Href $ eventUrl p.lastEvent.data
        pure $ dom'text p.lastEvent.data.tournamentName
    , mruPlacing: dom'text $
        (ordinal p.lastEvent.placing <+> "of" <+> p.lastEvent.data.numEntrants)
    , mruDate: dom'text $ "xD"
    }

page'stats :: forall dx x. Sort -> Filters -> ClmDomE dx x Unit
page'stats sort filters = do
  seasonId <- x'extract @"route" <#> g_ @"!.seasonId"
  seasonOr_ <- domFetchCache'lookup @"clmSeasons" seasonId
  animateInCn <- getAnimateInCn false "animate-flip-down"
  dom'Div do
    el'Cn "sticky z-30 top-16"
    pure $ pageTableRow
      "headers"
      ("h-10 rounded-none border-b-2 bg-base-300" <+> borderG3 <+> animateInCn)
      [ dom'Span $ el'Cn "font-bold" *> do pure $ dom'text "#"
      , tableHead SortBy'Name
      , tableHead SortBy'Qual
      , tableHead SortBy'Acc
      , tableHead SortBy'Att
      , tableHead SortBy'Mru
      ]
  let mkRankKey s rank = s <> "." <:> rank.clmId
  dom'Div do
    el'Cn "relative"
    pure case seasonOr_ of
      FetchCache'ValState'Loading _ -> do
        forM_ (arr'range 1 17) \id -> tableSkelRow id
      FetchCache'ValState'Error err _ -> do
        domError'fail $ ClmDom'Err'LoadSeason seasonId err
      FetchCache'ValState'Val season -> do
        let ranksOrE = boxed'call @"ranksOrESortedBy" season sort.by
        ranks <- domError'ok $ mapL (ClmDom'Err'FullSeason seasonId) ranksOrE
        forM_ ranks \rank -> dom'Div_ do
          el'Key $ mkRankKey "rel" rank
          el'Cn "h-25 w-[100%] min-w-240 border-b-1 rounded-none" *> w'BorderG3
        forM_ (arr'withInd ranks) \(rankInd /\ rank) -> do
          let isInUse = boxed'call @"getIsInUse" season rank.clmId filters
          dom'withKey (mkRankKey "abs" rank) do
            dom'withNewRef Nothing \getRef setRef -> do
              domEff'every'lead do
                x'do getRef >>= flip whenJust \ref -> do
                  domEff'el'classList'rm ref "hidden"
                  domEff'el'classList'rm ref "animate-jump-out"
                  pure unit
              dom'Div do
                let offset = 6.25 * toNumber rankInd
                el'AddCn "w-[100%] h-[calc(6.25rem_-_1px)]"
                el'AddCn "min-w-240 absolute top-0 left-0"
                whenNot isInUse $ el'AddCn "pointer-events-none"
                el'AddStyle "transform" $ "translateY(" <:> offset <:> "rem)"
                pure $ dom'Div do
                  el'Ref setRef
                  el'AddCn "w-[100%] h-[100%] absolute top-0 left-0"
                  whenNot isInUse $ el'AddCn "pointer-events-none"
                  pure $ tablePlayerRow rank

  where
  tableHead sortBy = do
    editRoute <- x'extract @"editRoute"
    let s4 = s'put Size4
    let isCurr = sort.by == sortBy
    let isPos = sort.dir == Pos
    let dir = if isCurr then pairKey'inv sort.dir else sortBy'defaultDir sortBy
    let targetSort = { dir, by: sortBy }
    dom'A do
      router'setHref $ editRoute $ route'setSort targetSort
      el'AddCn "group transition ease-in-out duration-300 whitespace-nowrap"
      el'AddCn "flex font-bold hover:underline hover:text-primary items-center"
      when isCurr $ el'AddCn "text-primary underline"
      pure do
        dom'text $ sortBy'title sortBy <> nbsp
        dom'Span $ el'Cn "relative" *> pure do
          dom'Span do
            el'Cn $ isCurr ? "group-hover:opacity-25" $ "group-hover:opacity-0"
            pure do
              if isCurr then isPos ? icon'sortUp s4 $ icon'sortDown s4
              else icon'sort s4
          dom'Div do
            el'AddCn "absolute top-0 left-0 w-full h-full"
            el'AddCn "flex justify-center items-center"
            el'AddCn "hidden group-hover:block"
            pure do
              dir == Pos ? icon'sortUp s4 $ icon'sortDown s4

page'player :: forall dx x. PlayerTab -> ClmDom dx x Unit
page'player tab = do
  pure unit

page'compare :: forall dx x. ClmDom dx x Unit
page'compare = do
  pure unit

page'h2h :: forall dx x. ClmDom dx x Unit
page'h2h = do
  pure unit

app'body :: forall dx x. ClmDom dx x Unit
app'body = do
  route <- x'extract @"route"
  editRoute <- x'extract @"editRoute"
  let seasonId = g_ @"!.seasonId" route
  let title = seasonId'title seasonId
  let isH2hPage = route'isH2hPage route
  let isStatsPage = route'isStatsPage route
  animateInCn <- getAnimateInCn_
  dom'Div do
    el'AddCn "container overflow-hidden max-w-290 rounded-none min-h-screen"
    el'AddCn "mx-auto px-0 card bg-base-100 shadow-xl m-4 my-0"
    pure $ dom'Div do
      el'AddCn "w-auto max-w-screen min-h-screen max-h-screen"
      whenNot isH2hPage $ el'AddCn "overflow-scroll"
      pure do
        dom'Div do
          el'AddCn "flex flex-col self-stretch"
          el'AddCn "max-w-screen sticky z-750 left-0 top-0"
          pure $ dom'Div do
            el'Id "navbar"
            el'AddCn "navbar h-15 border-b-2" *> w'BorderG3
            el'AddCn "relative z-50 p-2 py-0 bg-base-200 shadow-sm"
            pure do
              dom'Div do
                el'Cn "navbar-start relative w-auto gap-1 lg:gap-2"
                pure do
                  dom'Div do
                    el'AddCn "lg:hidden rounded-md" *> w'BorderG1
                    el'AddCn "absolute -top-px left-0 w-27"
                    el'AddCn $ if isStatsPage then "border-0" else "border-1"
                    pure $ whenNot isStatsPage $ dom'A do
                      router'setHref $ editRoute route'gotoStats
                      el'RoleButton
                      el'TargetBlank
                      el'TabIndex0
                      el'AddCn $ "btn btn-soft btn-outline" <> animateInCn
                      el'AddCn "px-0 w-full whitespace-nowrap" *> w'BorderG3
                      pure do
                        dom'Pre do pure (dom'text "⬑")
                        dom'Span do el'Cn "text-xs" *> pure (dom'text title)
                  dom'A do
                    el'Href "https://chicagomelee.com/"
                    el'TargetBlank
                    el'RoleButton
                    el'AddCn
                      "btn btn-ghost px-1 transition transition-transform"
                    el'AddCn
                      if isStatsPage then "translate-x-0" else "translate-x-29"
                    el'AddCn "lg:translate-x-0"
                    pure do
                      dom'Img_ $ el'Cn "w-8 h-8" *> el'Src "/favicon.ico"
                      dom'Span do
                        el'Cn "hidden lg:inline" *> pure (dom'text "CLM")
                  dom'Span_ do
                    el'AddCn "w-27 lg:hidden"
                    el'AddCn if isStatsPage then "hidden" else "inline"
                  periodMenu "menu-horizontal hidden lg:flex px-1 gap-2"
                  dom'Div_ $ el'Cn "w-1"
              dom'Div do
                el'AddCn "navbar-end flex-1 gap-4 overflow-hidden rounded-r-md"
                el'AddCn "hover:overflow-visible focus-within:overflow-visible"
                pure $ dom'Div do
                  el'AddCn "rounded-md flex items-stretch border-1 border-white"
                  el'AddCn "bg-gray-300 dark:bg-gray-700 dark:border-black"
                  pure do
                    dom'Div do
                      el'AddCn "group overflow-hidden relative rounded-l-sm"
                      el'AddCn "focus-within:overflow-visible bg-base-300"
                      el'AddCn "border-1 border-r-transparent" *> w'BorderG3
                      when isStatsPage $ el'AddCn "hover:overflow-visible"
                      pure do
                        dom'Div do
                          el'AddCn "absolute top-0 left-0 h-full w-full"
                          whenNot isStatsPage $
                            el'AddCn "group-focus-within:overflow-hidden"
                          pure do
                            dom'Div do
                              el'Cn "absolute z-15 right-[-2px] top-0 h-full"
                              -- TODO ActivePlayerIcons
                              pure $ pure unit
                            dom'A do
                              router'setHref $ editRoute $
                                route'toggleFilter Filters'att
                              el'Data "tip" "test tooltip" -- TODO tooltip
                              pure $ dom'Button do
                                el'TabIndex0
                                el'RoleButton
                                el'AddCn "relative btn-sm border-0 btn-soft"
                                el'AddCn "duration-500 h-full transition"
                                el'AddCn
                                  "rounded-l-xs p-px w-8 hover:bg-base-200"
                                el'AddCn "transition-colors rounded-r-none btn"
                                pure $ dom'Div do
                                  el'Cn "relative"
                                  pure do
                                    dom'Div do
                                      el'AddCn "absolute -bottom-3 left-0"
                                      el'AddCn "w-full h-full"
                                      el'AddCn
                                        "flex justify-center align-center"
                                      pure $ dom'Label do
                                        el'Cn "swap swap-rotate h-3 w-3"
                                        pure do
                                          dom'Input_ do el'Type "checkbox"
                                          dom'Div do
                                            el'Cn "swap-on text-accent"
                                            pure $ icon'minus do
                                              w'say "h-3 w-3 opacity-50"
                                          dom'Div do
                                            el'Cn "swap-off text-secondary"
                                            pure do icon'xmark $ w'say "h-3 w-3"
                                    icon'turnDown do
                                      w'say "w-4 h-4 relative -top-1"
                                      w'say "border-b-0 text-warning"
                        playerSearch
                    dom'Div do
                      el'Key $ route'pageKey route
                      el'AddCn "lg:hidden dropdown dropdown-hover dropdown-end"
                      el'AddCn "join-item border" *> w'BorderG3
                      pure $ do
                        dom'Button do
                          el'TabIndex0
                          el'RoleButton
                          el'AddCn "btn px-1 btn-sm h-full"
                          el'AddCn "btn-soft rounded-none border-0"
                          pure $ icon'bars pass
                        pure unit -- TODO periodMenu
                    dom'Div do
                      el'Key $ show $ g_ @"!.seasonId" route
                      el'AddCn "dropdown dropdown-hover dropdown-end"
                      el'AddCn "join-item border rounded-r-md" *> w'BorderG3
                      pure do
                        dom'Button do
                          el'TabIndex0
                          el'RoleButton
                          el'AddCn "btn px-1 btn-sm h-full btn-soft"
                          el'AddCn "rounded-none border-0 rounded-r-sm"
                          pure $ icon'calendar pass
                        pure unit -- TODO periodMenu
        domError'bind (const $ pure unit) do
          case g_ @"!.page" route of
            Stats sort filters -> page'stats sort filters
            Player tab -> page'player tab
            Compare -> page'compare
            H2hGrid -> page'h2h

app'mk :: forall dx x. ClmBuildOutputs -> UrlSt.XProvider dx x -> XDom dx x Unit
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
  domRef'run'' @"isInitials" initIsInitials do
    { initialRoute
    , isInitialPage
    , isInitialSeason
    , isInitialPlayer
    , isInitialTab
    } <- domRef'get'' @"isInitials"
    let
      isStillInitialSeason = isInitialSeason &&
        (g_ @"!.seasonId" route == g_ @"!.seasonId" initialRoute)
      isStillInitialPage = isStillInitialSeason && isInitialPage &&
        (g_ @"!.page" route == g_ @"!.page" initialRoute)
    domRef'set'' @"isInitials"
      { initialRoute
      , isInitialPage: isStillInitialPage
      , isInitialSeason: isStillInitialSeason && isStillInitialSeason
      , isInitialPlayer: isStillInitialSeason && isInitialPlayer &&
          (route'playerPageId route == route'playerPageId route)
      , isInitialTab: isStillInitialSeason && isInitialTab &&
          (route'playerPageTab route == route'playerPageTab route)
      }
    dom'runReader @"buildOutputs" buildOutputs
      $ dom'runReader @"route" route
      $ dom'runReader @"routeErr" routeErr
      $ dom'runReader @"editRoute" editRoute
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
