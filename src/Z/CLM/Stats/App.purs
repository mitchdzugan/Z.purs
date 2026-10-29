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
  , filters'pairs
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
  , route'playerPageTab
  , route'setSort
  , route'toggleFilter
  , route'urlEdits
  , route'urlParse
  , sortBy'defaultDir
  , sortBy'title
  )
import Z.CLM.Stats.Season (seasonId'title, tz'chicago)
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

newtype ClmSeason'Data = ClmSeason'Data ClmSeason'Json

derive instance Newtype ClmSeason'Data _
derive newtype instance Eq ClmSeason'Data
derive newtype instance EncodeJson ClmSeason'Data
derive newtype instance DecodeJson ClmSeason'Data

type ClmSeason'Boxed = Boxed ClmSeason'Data

data ClmSeason'Error
  = ClmSeason'Error'MissingPlayer Int
  | ClmSeason'Error'MissingEvent SorN

type Or'ClmSeason'Error t = Either ClmSeason'Error t

type ClmSeason'Players'Full = Array ClmSeason'Player'Full

data SandF = SandF SortBy PairKey Filters

instance Identable SandF where
  ident'get (SandF by dir filters) = id'of
    [ ident'key by
    , ident'key dir
    , ident'key $ filters'pairs filters
    ]

type OutOfSeasonPlayer =
  { clmId :: Int
  , gamerTag :: String
  , otherTags :: Array String
  , latestSeason :: Int
  , otherSeasons :: Array Int
  }

type AnyPlayer = Either OutOfSeasonPlayer ClmSeason'Player'Full

anyPlayer'gamerTag :: AnyPlayer -> String
anyPlayer'gamerTag (Left { gamerTag }) = gamerTag
anyPlayer'gamerTag (Right { player }) = player.gamerTag

anyPlayer'otherTags :: AnyPlayer -> Array String
anyPlayer'otherTags (Left { otherTags }) = otherTags
anyPlayer'otherTags (Right { otherTags }) = otherTags

anyPlayer'otherTags'key :: AnyPlayer -> String
anyPlayer'otherTags'key = str'joinWith " " <<< anyPlayer'otherTags

anyPlayer'tag'keys :: Array (AnyPlayer -> String)
anyPlayer'tag'keys = [ anyPlayer'gamerTag, anyPlayer'otherTags'key ]

sortBy'sorters :: SortBy -> Array (Sorter ClmSeason'Player'Full)
sortBy'sorters SortBy'Name = [ sorter _.player.gamerTag ]
sortBy'sorters SortBy'Qual = []
sortBy'sorters SortBy'Acc = [ sorter'rev _.winrate, sorter'rev _.sets ]
sortBy'sorters SortBy'Att = [ sorter'rev _.prEvents, sorter'rev _.sets ]
sortBy'sorters SortBy'Mru =
  [ sorter'rev _.lastEvent.data.date, sorter'rev _.prEvents ]

foreign import data M'events :: Type
foreign import data M'players :: Type
foreign import data M'otherPlayerTags :: Type
foreign import data M'otherPlayerSeasons :: Type
foreign import data M'ranksOrE :: Type
foreign import data M'allPlayers :: Type
foreign import data M'allPlayersSnapshot :: Type
foreign import data M'searchedPlayers :: Type
foreign import data M'ranksOrESorted :: Type

instance
  Method M'events ClmSeason'Boxed Unit (HashMap SorN ClmSeason'Event) where
  call b _ = keyBy _.eventId $ _.events $ unwrap $ boxed'v b

instance Method M'players ClmSeason'Boxed Unit (HashMap Int ClmPlayer) where
  call b _ = keyBy _.clmId $ _.players $ unwrap $ boxed'v b

instance
  Method
    M'otherPlayerTags
    ClmSeason'Boxed
    Unit
    (HashMap Int (Array String)) where
  call b _ = hm'fromFoldable $ _.otherPlayerTags $ unwrap $ boxed'v b

instance
  Method
    M'otherPlayerSeasons
    ClmSeason'Boxed
    Unit
    (HashMap Int (Array Int)) where
  call b _ = hm'fromFoldable $ _.otherPlayerSeasons $ unwrap $ boxed'v b

instance
  Method M'ranksOrE
    ClmSeason'Boxed
    Unit
    (Or'ClmSeason'Error ClmSeason'Players'Full) where
  call b _ = sync'x do
    let otherPlayerTags = boxed'call_ @M'otherPlayerTags b
    let otherPlayerSeasons = boxed'call_ @M'otherPlayerSeasons b
    let eventsById = boxed'call_ @M'events b
    let playersById = boxed'call_ @M'players b
    e'try $ forM (unwrap $ boxed'v b).ranks \rank -> do
      let playerErr = ClmSeason'Error'MissingPlayer rank.clmId
      let eventErr = ClmSeason'Error'MissingEvent rank.lastEvent.id
      player <- e'unwrap playerErr $ hm'lookup rank.clmId playersById
      evData <- e'unwrap eventErr $ hm'lookup rank.lastEvent.id eventsById
      let sets = rank.wins + rank.losses
      let numWs = toNumber rank.wins
      let numTot = toNumber sets
      let otherTags = may'or [] $ hm'lookup rank.clmId otherPlayerTags
      let otherSeasons = may'or [] $ hm'lookup rank.clmId otherPlayerSeasons
      let winrate = if (sets == 0) then Nothing else Just (numWs / numTot)
      pure $ rank
        # rec'insert @"player" player
        # rec'insert @"sets" sets
        # rec'insert @"winrate" winrate
        # rec'insert @"sortAndFilteredRank" 0
        # rec'insert @"otherTags" otherTags
        # rec'insert @"otherSeasons" otherSeasons
        # rec'insert @"hasReq'prEvents" (clm'hasReq'prEvents rank)
        # rec'insert @"hasRatingConfidence" (clm'hasRatingConfidence rank)
        # rec'modify @"lastEvent" (\ev -> rec'insert @"data" evData ev)

instance Method M'allPlayers ClmSeason'Boxed Unit (Array AnyPlayer) where
  call b _ = sync'x do
    let ranks = may'or [] $ hush $ boxed'call_ @M'ranksOrE b
    let ranksById = keyBy _.clmId ranks
    let otherPlayerTags = boxed'call_ @M'otherPlayerTags b
    let otherPlayerSeasons = boxed'call_ @M'otherPlayerSeasons b
    outOfSeasonPlayers <- _'exec_ @(X'Vector OutOfSeasonPlayer) do
      forM_ (hm'entries otherPlayerTags) \(clmId /\ allOtherTags) -> do
        when (isNothing $ hm'lookup clmId ranksById) do
          whenJust (arr'splitHead allOtherTags) \splitHeadTags -> do
            let (gamerTag /\ otherTags) = splitHeadTags
            let allSeasons = may'or [] $ hm'lookup clmId otherPlayerSeasons
            whenJust (arr'splitTail allSeasons) \splitHeadSeasons -> do
              let (latestSeason /\ otherSeasons) = splitHeadSeasons
              _'cons
                { clmId, gamerTag, otherTags, latestSeason, otherSeasons }
    pure $ arr'concat [ ranks <#> Right, outOfSeasonPlayers <#> Left ]

instance
  Method
    M'allPlayersSnapshot
    ClmSeason'Boxed
    Unit
    (FuzzySnapshot AnyPlayer) where
  call b _ = fuzzy'mkSnapshot (boxed'call_ @M'allPlayers b) anyPlayer'tag'keys

instance Method M'searchedPlayers ClmSeason'Boxed String (Array AnyPlayer) where
  call b srch =
    if srch == "" then boxed'call_ @M'allPlayers b
    else _.obj <$>
      fuzzy'searchSnapshot (boxed'call_ @M'allPlayersSnapshot b) srch pass

instance
  Method M'ranksOrESorted
    ClmSeason'Boxed
    SandF
    (Or'ClmSeason'Error ClmSeason'Players'Full) where
  call b (SandF sortBy _ filters) = do
    let sorter'isInUse = sorter'rev (clm'isInUse filters)
    let sorters = arr'concat [ [ sorter'isInUse ], sortBy'sorters sortBy ]
    boxed'call_ @M'ranksOrE b
      <#> arr'filter (\r -> r.prEvents > 0)
      <#> withMultisortInd sorters
      <#> map (\(ind /\ r) -> rec'set @"sortAndFilteredRank" ind r)

-----------------------------------------------------------------------

newtype ClmPlayerSeason'Data = ClmPlayerSeason'Data ClmPlayerSeason'Json

derive instance Newtype ClmPlayerSeason'Data _
derive newtype instance Eq ClmPlayerSeason'Data
derive newtype instance EncodeJson ClmPlayerSeason'Data
derive newtype instance DecodeJson ClmPlayerSeason'Data

type ClmPlayerSeason'Boxed = Boxed ClmPlayerSeason'Data

-----------------------------------------------------------------------

type ClmBuildOutputs'X x = (buildOutputs :: X'R ClmBuildOutputs | x)

type ClmDom'X x = Router' String Route $ ClmBuildOutputs'X
  ( clmSeasons :: X'FetchCache Int ClmSeason'Boxed
  , clmPlayerSeasons :: X'FetchCache (Int /\ Int) ClmPlayerSeason'Boxed
  , route :: X'R Route
  , routeErr :: X'R (Maybe String)
  , editRoute :: X'R (Edit Route -> Route)
  , pxPerRemOr_ :: XDom'Ref (Maybe Number)
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

type ClmDomEff x a = XDomEff (ClmDom'X x) a

data ClmDom'Err
  = ClmDom'Err'LoadSeason Int JsError
  | ClmDom'Err'FullSeason Int ClmSeason'Error

type ClmDomE dx x a = XDom (DomE' ClmDom'Err dx) (ClmDom'X x) a

getPxPerRem :: forall x. ClmDomEff x Number
getPxPerRem = domRef'get'' @"pxPerRemOr_" >>= case _ of
  Just pxPerRem -> pure pxPerRem
  Nothing -> do
    pxPerRem <- domEff'doc'pxPerRem
    domRef'set'' @"pxPerRemOr_" $ Just pxPerRem
    pure pxPerRem

getRemPx :: forall x. Number -> ClmDomEff x Number
getRemPx rem = getPxPerRem <#> (*) rem

getPxRem :: forall x. Number -> ClmDomEff x Number
getPxRem px = getPxPerRem <#> (/) px

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
w'BorderG3 = el'addCn borderG3

borderG1 :: String
borderG1 = "border-gray-100 dark:border-gray-900"

w'BorderG1 :: forall dx x. MDomEl dx x Unit
w'BorderG1 = el'addCn borderG1

menuCn :: Boolean -> String
menuCn isActive = w'str'sp \w -> do
  w "active:bg-primary/25 active:dark:bg-primary-content/25"
  w "active:text-base-content"
  when isActive do w "active bg-primary/25 dark:bg-primary-content/25"

nbsp :: String
nbsp = "\x00A0"

periodMenu :: forall dx x. String -> ClmDom dx x Unit
periodMenu ulCn = do
  dom'ul do
    el'tabIndex (-1)
    el'addCn "menu"
    el'addCn ulCn
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
    dom'li do
      el'key text
      pure $ dom'a do
        el'cn $ menuCn isActive
        router'setHref linkedRoute
        pure do icon pass *> do dom'text $ nbsp <> text

type PlayerSearchProps =
  { dropdownStart :: Boolean
  , dropdownTop :: Boolean
  , outsideNav :: Boolean
  }

defaultPlayerSearchProps :: PlayerSearchProps
defaultPlayerSearchProps =
  { dropdownStart: false, dropdownTop: false, outsideNav: false }

getSearchedPlayers :: forall dx x. String -> ClmDom dx x (Array AnyPlayer)
getSearchedPlayers searchString = do
  seasonId <- x'extract @"route" <#> g_ @"!.seasonId"
  domFetchCache'lookup @"clmSeasons" seasonId <#> case _ of
    FetchCache'ValState'Val season ->
      boxed'call @M'searchedPlayers season searchString
    _ -> []

playerSearch :: forall dx x. Edit PlayerSearchProps -> ClmDom dx x Unit
playerSearch edits = do
  let props = edit defaultPlayerSearchProps edits
  let { dropdownStart, dropdownTop, outsideNav } = props
  route <- x'extract @"route"
  let isStatsPage = route'isStatsPage route
  let initialState = { input: "", highlightedInd: 0 }
  dom'withNewRef Nothing \getEl setEl -> do
    dom'withNewState initialState \state setState -> do
      searchedPlayers <- getSearchedPlayers state.input
      domEff'every'lead do
        x'do getEl >>= flip whenJust \el -> do
          domEff'el'querySelector el "[data-is-highlighted=\"true\"]" >>=
            flip whenJust \hlEl -> do
              x'out { el, hlEl }
              domEff'el'parentElement hlEl >>=
                flip whenJust \hlElP1 -> domEff'el'parentElement hlElP1 >>=
                  flip whenJust \hlElP2 -> domEff'el'parentElement hlElP2 >>=
                    flip whenJust \hlElP3 -> do
                      x'out { el, hlEl, hlElP1, hlElP2, hlElP3 }
      x'out { searchedPlayers }
      x'out state
      dom'div do
        el'setElRef setEl
        el'addCn "dropdown"
        whenNot dropdownStart $ el'addCn "dropdown-end"
        when dropdownTop $ el'addCn "dropdown-top"
        pure do
          dom'label do
            let outlineO o = (/\) "outlineOffset" $ o <:> "px"
            let outlineW w = (/\) "outlineWidth" $ w <:> "px"
            let outlineInh = "outlineStyle" /\ "inherit"
            let outlineOW o w = [ outlineO o, outlineW w ]
            let outlineOWInh o w = [ outlineInh ] <> outlineOW o w
            let mkSetOutline f o w = el'setStyles $ f o w
            let setOutlineStylesOW = mkSetOutline outlineOW
            let setOutlineStylesOWInh = mkSetOutline outlineOWInh
            el'addCn "group input h-9 border-0 px-2 rounded-none"
            el'addCn "transition transition-transform duration-300 shadow-none"
            el'addCn "relative outline-hidden focus-within:outline-solid"
            el'addCn $ if isStatsPage then "translate-x-8" else "translate-x-0"
            setOutlineStylesOW (-6) 2
            pure do
              icon'magnifyingGlass $ s'put Size4
              dom'input_ do
                el'tabIndex0
                el'type "text"
                el'placeholder "Search Players..."
                el'onInput \e -> do
                  input <- domEff'event'currentTargetValue e
                  x'do $ setState $ rec'set @"input" input state
                el'value state.input
              dom'div do
                el'key "outline-spacer"
                el'addCn "hidden absolute top-[5px] right-[calc(2rem_-_1px)]"
                el'addCn "w-[5px] h-[calc(100%_-_10px)]"
                when isStatsPage $ el'addCn "group-focus-within:block"
                setOutlineStylesOWInh 0 0
                pure do
                  dom'div_ do
                    el'addCn "absolute w-0 h-full top-0 -right-px"
                    el'addCn "outline-white dark:outline-black"
                    setOutlineStylesOWInh 0 5
                  dom'div_ do
                    el'addCn "absolute w-0 h-full top-0 left-px"
                    el'addCn "outline-black dark:outline-white"
                    setOutlineStylesOWInh 0 1
          dom'div do
            el'cn "dropdown-content max-h-80 overflow-y-scroll"
            el'addCn $ dropdownStart
              ? (outsideNav ? "left-2" $ "-left-12")
              $ (outsideNav ? "right-2" $ "-right-12")
            el'addCn $ outsideNav ? "z-800" $ "z-1"
            el'addCn "w-full shadow-sm bg-base-200 rounded-box p-0 min-w-70"
            pure $ dom'ul do
              el'tabIndex (-1)
              el'cn "relative w-full menu"
              pure $ dom'text "Test Search Dropdown"

type TableCell dx x = MDom dx x Unit

type TableCells dx x = Array (TableCell dx x)

pageTableRow
  :: forall dx x. String -> String -> TableCells dx x -> MDom dx x Unit
pageTableRow rowKey rowCn cells = do
  dom'div do
    el'key rowKey
    el'cn $ "table text-base w-[100%] min-w-240 " <> rowCn
    pure $ dom'div do
      el'cn "table-row"
      pure do
        dom'div $ el'cn "relative table-cell w-22" *> cell 0
        dom'div $ el'cn "relative table-cell min-w-80 w-auto" *> cell 1
        dom'div $ el'cn "relative table-cell w-28" *> cell 2
        dom'div $ el'cn "relative table-cell w-28" *> cell 3
        dom'div $ el'cn "relative table-cell w-28" *> cell 4
        dom'div do
          el'setStyles [ "width" /\ "max(16rem, 18vw)" ]
          el'cn "relative table-cell"
          cell 5
  where
  cell i = do
    let isStart = i == 1 || i == 5
    let mcell = may'or pass $ nth cells i
    pure $ dom'div do
      el'addCn "left-0 top-0 absolute h-full w-full flex items-center"
      el'addCn $ if isStart then "justify-start" else "justify-center"
      when (i == 5) do el'addCn "px-1"
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
    [ dom'div do
        el'cn "font-extrabold text-4xl"
        pure props.ord
    , do
        editRoute <- x'extract @"editRoute"
        let
          topCn = w'str'sp \w -> do
            w "flex items-center justify-start flex-1"
            when (isJust clmIdOr_) $ w "group"
          useBody tag m = tag $ el'cn topCn *> m *> pure do
            dom'div_ $ el'cn "w-2"
            dom'div do
              el'addCn "h-13 w-13 rounded-full overflow-hidden shadow-md"
              el'addCn "transition transition-shadow transition-colors"
              el'addCn "duration-300 group-hover:shadow-primary/33"
              pure props.profileImage
            dom'div_ $ el'cn "w-4"
            dom'div do
              el'cn
                "flex flex-col justify-center flex-1 relative overflow-hidden"
              pure do
                dom'div do
                  el'addCn "whitespace-nowrap text-xl font-bold"
                  el'addCn "text-primary group-hover:underline"
                  pure props.name
                whenJust props.pronouns \pronouns -> dom'div do
                  el'cn "text-sm"
                  pure pronouns
                whenJust props.characters \characters -> dom'div do
                  pure characters
        case clmIdOr_ of
          Just clmId -> do
            useBody dom'a $ router'setHref
              (editRoute $ route'gotoPlayer *> s'setPure'b @"!.clmIds" clmId)
          _ -> useBody dom'div $ pure unit
    , dom'div $ pure props.qual
    , dom'div $ pure props.acc
    , dom'div do
        el'cn "h-full w-full flex justify-center items-center"
        when (isJust props.attTooltip) do
          el'addCn "dropdown dropdown-hover dropdown-bottom dropdown-end"
        pure do
          props.att
          whenJust props.attTooltip \attTooltip -> do
            dom'div do
              el'addCn "dropdown-content rounded shadow-sm"
              el'addCn "p-2 w-76 bg-base-200 z-50"
              el'addStyle "transform" "translateY(-2.25rem)"
              pure attTooltip
    , dom'div do
        el'cn "flex flex-col overflow-hidden"
        pure do
          dom'div do
            el'cn "overflow-hidden whitespace-nowrap text-ellipsis"
            pure props.mruLink
          dom'div do
            el'cn "overflow-hidden whitespace-nowrap text-sm text-ellipsis"
            pure props.mruPlacing
          dom'div do
            el'cn "overflow-hidden whitespace-nowrap text-sm text-ellipsis"
            el'addCn "opacity-75"
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
  dom'div_ $ el'addCn "skeleton" *> el'addCn cn *> el'setStyles skelStyle

tableSkelRow :: forall dx x. Int -> ClmDom dx x Unit
tableSkelRow id = do
  let key = "skelRow" <-> id
  clmRng <- getClmRng key
  let rngGate s lim m = if clmRng [ s ] > lim then Nothing else Just m
  tableContentRow Nothing
    { key
    , cn: "h-25 rounded-none" <+> borderG3
    , ord: dom'div_ $ el'cn "skeleton h-6 w-10"
    , profileImage: dom'div_ $ el'cn "skeleton h-full w-full"
    , name: rngSkel key "h-4" 5.0 7.5
    , pronouns: rngGate "pronouns" 0.7 $ rngSkel key "h-3" 2.0 2.5
    , characters: rngGate "char" 0.8 $ dom'div_ $ el'cn "skeleton h-3 w-3"
    , qual: rngSkel key "h-4" 2.75 3.25
    , acc: dom'div $ el'cn "flex gap-1" *> pure do
        rngSkel key "h-4" 1.201 1.799
        rngSkel key "h-4" 1.199 1.801
    , att: rngSkel key "h-4" 2.0 2.55
    , attTooltip: Nothing
    , mruLink: rngSkel key "h-4" 4.25 7.25
    , mruPlacing: rngSkel key "h-3" 1.85 2.15
    , mruDate: rngSkel key "h-3" 1.35 1.65
    }

defaultProfileImage :: String
defaultProfileImage = "/img/CLM_Logo_Avatar_Placeholder.png"

type ProfileImageProps =
  { player :: ClmPlayer
  , cn :: String
  , lazyLoad :: Boolean
  , alt :: Maybe String
  }

profileImage :: forall dx x. ProfileImageProps -> ClmDom dx x Unit
profileImage props = do
  let attemptedSrc = may'or defaultProfileImage props.player.image
  dom'withNewState false \hasError setHasError -> do
    let src = if hasError then defaultProfileImage else attemptedSrc
    let isDefault = src == defaultProfileImage
    dom'img_ do
      el'addCn props.cn
      el'addCn "text-transparent bg-base-300"
      when isDefault $ el'addCn "dark:filter-[brightness(55%)_contrast(1000%)]"
      el'src src
      when props.lazyLoad $ el'loading "lazy"
      whenJust props.alt el'alt

eventUrl :: ClmSeason'Event -> String
eventUrl { source: { site, slug } } = case site of
  H2h.Startgg -> "https://start.gg/" <> slug
  H2h.Challonge -> "https://challonge.com/" <> slug

eventDateString :: ClmSeason'Event -> String
eventDateString { date } = str'joinWith "/" $ (#) date <$>
  [ flip dateTime'month'2dig tz'chicago
  , flip dateTime'day'2dig tz'chicago
  , show <<< flip dateTime'year tz'chicago
  ]

getSocialHref :: H2h.SocialType -> String -> Maybe String
getSocialHref H2h.Twitter s = Just $ "https://x.com/" <> s
getSocialHref H2h.Twitch s = Just $ "https://www.twitch.tv/" <> s
getSocialHref H2h.LuckyStats s = Just $ "https://luckystats.gg/player/" <> s
getSocialHref _ _ = Nothing

getSocialImgSrc :: H2h.SocialType -> Maybe String
getSocialImgSrc H2h.Twitter = Just $ "/links/twitter.png"
getSocialImgSrc H2h.Twitch = Just $ "/links/twitch.png"
getSocialImgSrc H2h.LuckyStats = Just $ "/links/lucky_stats.png"
getSocialImgSrc _ = Nothing

type SocialProps = { imgSrc :: String, href :: String }

getSocialProps :: H2h.SocialType /\ String -> Maybe SocialProps
getSocialProps (socialType /\ socialVal) = do
  imgSrc <- getSocialImgSrc socialType
  href <- getSocialHref socialType socialVal
  pure { imgSrc, href }

getSocialsProps :: HashMap H2h.SocialType String -> Array SocialProps
getSocialsProps = arr'concatMaybes <<< map getSocialProps <<< hm'entries

socialIcons :: forall dx x. ClmPlayer -> ClmDom dx x Unit
socialIcons { socials } = forM_ (getSocialsProps socials) \{ imgSrc, href } ->
  dom'a do
    el'cn "mr-1" *> el'targetBlank *> el'href href
    pure $ dom'img_ $ el'cn "h-4 w-4" *> el'src imgSrc

charImages :: forall dx x. ClmSeason'Player'Full -> String -> ClmDom dx x Unit
charImages { chars: [] } _ = pure unit
charImages { chars } cnExtra = do
  dom'div do
    el'cn "inline-flex flex-row items-center gap-1"
    el'addCn cnExtra
    pure $ forM_ chars \{ charId, color, usage } -> do
      dom'img_ do
        let mkUsageStr n i = if n < 0.01 then "<1" else show i
        let usageStr = mkUsageStr usage $ round $ 100.0 * usage
        el'cn "h-5 w-5"
        el'title $ usageStr <> "% usage in sets with reported character data"
        el'src $ "/chars/" <:> charId <:> "/" <:> color <:> "/stock.png"

tablePlayerRow :: forall dx x. Int -> ClmSeason'Player'Full -> ClmDom dx x Unit
tablePlayerRow sortedInd p = do
  isSortByQual <- x'extract @"route" <#> route'isSortByQual
  let canShowQual = p.hasRatingConfidence
  let canShowOrd = canShowQual || not isSortByQual
  tableContentRow (Just p.clmId)
    { key: "player." <:> p.clmId
    , cn: ""
    , ord: dom'text $ if canShowOrd then show sortedInd else "-"
    , profileImage: profileImage
        { alt: Just "start.gg profile image"
        , lazyLoad: true
        , player: p.player
        , cn: "object-cover w-full h-full"
        }
    , name: dom'text $ p.player.gamerTag
    , pronouns: Just $ dom'span do
        el'cn "inline-flex flex-row items-center"
        pure do
          whenJust p.player.pronouns \pronouns -> do
            dom'span $ el'cn "mr-1" *> pure (dom'text pronouns)
          socialIcons p.player
    , characters: Just $ charImages p ""
    , qual: dom'text $ if canShowQual then show p.rating else "-"
    , acc: dom'text $ p.wins <+> "-" <+> p.losses
    , att: dom'div do
        el'cn "flex items-center"
        pure do
          dom'text $ p.prEvents
          dom'div $ (*>) (el'cn "relative") $ pure $ dom'div do
            el'cn "absolute top-0 left-0 flex items-center"
            pure $ whenNot p.hasReq'prEvents do
              dom'span do
                el'cn "flex items-center relative bottom-4"
                pure $ dom'text nbsp *> iconBadge'activity (s'put Size4)
    , attTooltip:
        (if p.hasReq'prEvents then Nothing else Just unit) <#> \_ -> do
          dom'ul do
            el'cn "flex flex-col gap-1"
            pure $ dom'li do
              el'cn "flex items-center whitespace-nowrap"
              pure do
                iconBadge'activity $ s'put Size4
                dom'text nbsp
                dom'text "has not met activity requirements"
    , mruLink: dom'a do
        el'cn "nowrap text-primary hover:underline"
        el'targetBlank
        el'href $ eventUrl p.lastEvent.data
        pure $ dom'text p.lastEvent.data.tournamentName
    , mruPlacing: dom'text $
        (ordinal p.lastEvent.placing <+> "of" <+> p.lastEvent.data.numEntrants)
    , mruDate: dom'text $ eventDateString p.lastEvent.data
    }

clm'isInUse :: Filters -> ClmSeason'Player'Full -> Boolean
clm'isInUse f p = p.hasReq'prEvents || not f.req'prEvents

page'stats :: forall dx x. Sort -> Filters -> ClmDomE dx x Unit
page'stats sort filters = do
  seasonId <- x'extract @"route" <#> g_ @"!.seasonId"
  seasonOr_ <- domFetchCache'lookup @"clmSeasons" seasonId
  animateInCn <- getAnimateInCn false "animate-flip-down"
  dom'div do
    el'cn "sticky z-30 top-16"
    pure $ pageTableRow
      "headers"
      ("h-10 rounded-none border-b-2 bg-base-300" <+> borderG3 <+> animateInCn)
      [ dom'span $ el'cn "font-bold" *> do pure $ dom'text "#"
      , tableHead SortBy'Name
      , tableHead SortBy'Qual
      , tableHead SortBy'Acc
      , tableHead SortBy'Att
      , tableHead SortBy'Mru
      ]
  let mkRankKey s rank = s <> "." <:> seasonId <:> "." <:> rank.clmId
  dom'withKey "statsPage" $ dom'div do
    el'cn "relative"
    pure case seasonOr_ of
      FetchCache'ValState'Loading _ -> do
        forM_ (arr'range 1 17) \id -> tableSkelRow id
      FetchCache'ValState'Error err _ -> do
        domError'fail $ ClmDom'Err'LoadSeason seasonId err
      FetchCache'ValState'Val season -> do
        let sAndF = SandF sort.by sort.dir filters
        let ranksOrE = boxed'call @M'ranksOrESorted season sAndF
        ranks <- domError'ok $ mapL (ClmDom'Err'FullSeason seasonId) ranksOrE
        let filtRanks = arr'filter (clm'isInUse filters) ranks
        forM_ filtRanks \rank -> dom'div_ do
          el'key $ mkRankKey "rel" rank
          el'cn "h-25 w-[100%] min-w-240 border-b-1 rounded-none" *> w'BorderG3
        let numShown = arr'size filtRanks
        let isDefaultDir = sort.dir == sortBy'defaultDir sort.by
        forM_ ranks \(rank) -> do
          let rankInd = rank.sortAndFilteredRank
          let sortedInd = isDefaultDir ? rankInd $ numShown - rankInd - 1
          let isInUse = clm'isInUse filters rank
          dom'withKey (mkRankKey "abs" rank) do
            dom'withNewRef Nothing \getRef setRef -> do
              domEff'every'lead do
                x'do getRef >>= flip whenJust \(el /\ hasRendered) -> do
                  if isInUse then do
                    domEff'el'classList'rm el "hidden"
                    domEff'el'classList'rm el "animate-jump-out"
                    if hasRendered then do
                      domEff'el'classList'rm el "animate-once"
                    else do
                      domEff'el'classList'add el "animate-jump-in"
                      domEff'el'classList'add el "animate-once"
                    domEff'async do
                      domEffA'animationFrame
                      domEffA'sync do
                        domEff'el'parentElement el >>= flip whenJust \pEl -> do
                          when isInUse do
                            domEff'el'classList'add pEl "transition"
                            domEff'el'classList'add pEl "transition-transform"
                            domEff'el'classList'add pEl "duration-300"
                            domEff'el'classList'add pEl "ease-in-out"
                  else do
                    domEff'el'classList'rm el "animate-jump-in"
                    if hasRendered then do
                      domEff'el'classList'add el "animate-jump-out"
                      domEff'el'classList'add el "animate-once"
                    else do
                      domEff'el'classList'add el "hidden"
                    domEff'el'parentElement el >>= flip whenJust \pEl -> do
                      domEff'el'classList'rm pEl "transition"
                      domEff'el'classList'rm pEl "transition-transform"
                      domEff'el'classList'rm pEl "duration-300"
                      domEff'el'classList'rm pEl "ease-in-out"
                  x'do $ setRef $ Just $ el /\ isInUse
              dom'div do
                let offset = (*) 6.25 $ toNumber $ min sortedInd $ dec numShown
                el'addCn "w-[100%] h-[calc(6.25rem_-_1px)]"
                el'addCn "min-w-240 absolute top-0 left-0"
                whenNot isInUse $ el'addCn "pointer-events-none"
                el'addStyle "transform" $ "translateY(" <:> offset <:> "rem)"
                pure $ dom'div do
                  el'ref \el -> do
                    hasRendered <- x'do getRef <#> map snd <#> may'or false
                    x'do $ setRef $ Just $ el /\ hasRendered
                  el'onAnimationEnd \_ -> do
                    x'do getRef >>= flip whenJust \(el /\ _) -> do
                      whenNot isInUse do
                        domEff'el'classList'add el "hidden"
                      domEff'el'classList'rm el "animate-jump-in"
                      domEff'el'classList'rm el "animate-jump-out"
                      domEff'el'classList'rm el "animate-once"
                  el'addCn "w-[100%] h-[100%] absolute top-0 left-0"
                  whenNot isInUse $ el'addCn "pointer-events-none"
                  pure $ when isInUse $ tablePlayerRow (rankInd + 1) rank

  where
  tableHead sortBy = do
    editRoute <- x'extract @"editRoute"
    let s4 = s'put Size4
    let isCurr = sort.by == sortBy
    let isPos = sort.dir == pos
    let dir = if isCurr then pairKey'inv sort.dir else sortBy'defaultDir sortBy
    let targetSort = { dir, by: sortBy }
    dom'a do
      router'setHref $ editRoute $ route'setSort targetSort
      el'addCn "group transition ease-in-out duration-300 whitespace-nowrap"
      el'addCn "flex font-bold hover:underline hover:text-primary items-center"
      when isCurr $ el'addCn "text-primary underline"
      pure do
        dom'text $ sortBy'title sortBy <> nbsp
        dom'span $ el'cn "relative" *> pure do
          dom'span do
            el'cn $ isCurr ? "group-hover:opacity-25" $ "group-hover:opacity-0"
            pure do
              if isCurr then isPos ? icon'sortUp s4 $ icon'sortDown s4
              else icon'sort s4
          dom'div do
            el'addCn "absolute top-0 left-0 w-full h-full"
            el'addCn "flex justify-center items-center"
            el'addCn "hidden group-hover:block"
            pure do
              dir == pos ? icon'sortUp s4 $ icon'sortDown s4

page'player :: forall dx x. PlayerTab -> ClmDom dx x Unit
page'player tab = do
  pure unit

page'compare :: forall dx x. ClmDom dx x Unit
page'compare = do
  pure unit

page'h2h :: forall dx x. ClmDom dx x Unit
page'h2h = do
  pure unit

filterButton :: forall dx x. ClmDom dx x Unit
filterButton = do
  route <- x'extract @"route"
  editRoute <- x'extract @"editRoute"
  let isStatsPage = route'isStatsPage route
  let anyChangedFilters = route'anyChangedFilters route
  let hasFilter'att = route'getFilter Filters'att route
  let attTipPrefix = hasFilter'att ? "Show" $ "Hide"
  let attTip = attTipPrefix <+> "Insufficient Attendance"
  dom'withNewRef Nothing \getEl setEl -> do
    dom'withNewRef false \getCanColor setCanColor -> do
      domEff'once'lead $ domEff'async do
        x'timeout 100
        x'do $ setCanColor true
        domEffA'sync $ updateColor anyChangedFilters getEl getCanColor
      domEff'onNew'lead (anyChangedFilters /\ hasFilter'att) do
        updateColor anyChangedFilters getEl getCanColor
      dom'a do
        router'setHref $ editRoute $
          route'toggleFilter Filters'att
        el'addCn "absolute z-10 top-0 left-px h-full"
        el'addCn "tooltip tooltip-bottom overflow-visible"
        el'addCn "border border-0" *> w'BorderG3
        el'addCn "transition-transform transition-colors"
        el'addCn "join-item duration-300 border-r-1"
        let tx0 = "translate-x-0"
        el'addCn $ isStatsPage ? tx0 $ "-translate-x-9"
        el'data "tip" attTip
        pure $ dom'button do
          el'setElRef setEl
          el'tabIndex0
          el'roleButton
          el'addCn "relative btn-sm border-0 btn-soft"
          el'addCn "duration-500 h-full p-px transition"
          el'addCn "rounded-l-xs w-8 hover:bg-base-200"
          el'addCn "transition-colors rounded-r-none btn"
          pure $ dom'div do
            el'cn "relative"
            pure do
              dom'div do
                el'addCn "absolute -bottom-3 left-0"
                el'addCn "w-full h-full"
                el'addCn
                  "flex justify-center align-center"
                pure $ dom'label do
                  el'cn "swap swap-rotate h-3 w-3"
                  pure do
                    dom'input_ do
                      el'type "checkbox"
                      el'checked $ not hasFilter'att
                      el'onClick domEff'event'preventDefault
                    dom'div do
                      el'cn "swap-on text-accent"
                      pure $ icon'minus do
                        w'say "h-3 w-3 opacity-50"
                    dom'div do
                      el'cn "swap-off text-secondary"
                      pure do icon'xmark $ w'say "h-3 w-3"
              icon'turnDown do
                w'say "w-4 h-4 relative -top-1"
                w'say "border-b-0 text-warning"
  where
  updateColor anyChanged getEl getCanColor = do
    x'do getCanColor >>= flip when do
      x'do getEl >>= flip whenJust \el -> do
        let fn = anyChanged ? domEff'el'classList'add $ domEff'el'classList'rm
        fn el "bg-info/20"

app'body :: forall dx x. ClmDom dx x Unit
app'body = do
  allSeasonIds <- x'extract @"buildOutputs"
    <#> _.timeline
    <#> (\tl -> [ tl.preDefault, [ tl.defaultSeasonId ], tl.postDefault ])
    <#> arr'concat
    <#> arr'reverse
  route <- x'extract @"route"
  editRoute <- x'extract @"editRoute"
  let seasonId = g_ @"!.seasonId" route
  let title = seasonId'title seasonId
  let isH2hPage = route'isH2hPage route
  let isStatsPage = route'isStatsPage route
  animateInCn <- getAnimateInCn_
  dom'div do
    el'addCn "container overflow-hidden max-w-290 rounded-none min-h-screen"
    el'addCn "mx-auto px-0 card bg-base-100 shadow-xl m-4 my-0"
    pure $ dom'div do
      el'addCn "w-auto max-w-screen min-h-screen max-h-screen"
      whenNot isH2hPage $ el'addCn "overflow-scroll"
      pure do
        dom'div do
          el'addCn "flex flex-col self-stretch"
          el'addCn "max-w-screen sticky z-750 left-0 top-0"
          pure $ dom'div do
            el'id "navbar"
            el'addCn "navbar h-15 border-b-2" *> w'BorderG3
            el'addCn "relative z-50 p-2 py-0 bg-base-200 shadow-sm"
            pure do
              dom'div do
                el'cn "navbar-start relative w-auto gap-1 lg:gap-2"
                pure do
                  dom'div do
                    el'addCn "lg:hidden rounded-md" *> w'BorderG1
                    el'addCn "absolute -top-px left-0 w-27"
                    el'addCn $ if isStatsPage then "border-0" else "border-1"
                    pure $ whenNot isStatsPage $ dom'a do
                      router'setHref $ editRoute route'gotoStats
                      el'roleButton
                      el'targetBlank
                      el'tabIndex0
                      el'addCn $ "btn btn-soft btn-outline" <> animateInCn
                      el'addCn "px-0 w-full whitespace-nowrap" *> w'BorderG3
                      pure do
                        dom'pre do pure (dom'text "⬑")
                        dom'span do el'cn "text-xs" *> pure (dom'text title)
                  dom'a do
                    el'href "https://chicagomelee.com/"
                    el'targetBlank
                    el'roleButton
                    el'addCn
                      "btn btn-ghost px-1 transition transition-transform"
                    el'addCn
                      if isStatsPage then "translate-x-0" else "translate-x-29"
                    el'addCn "lg:translate-x-0"
                    pure do
                      dom'img_ $ el'cn "w-8 h-8" *> el'src "/favicon.ico"
                      dom'span do
                        el'cn "hidden lg:inline" *> pure (dom'text "CLM")
                  dom'span_ do
                    el'addCn "w-27 lg:hidden"
                    el'addCn if isStatsPage then "hidden" else "inline"
                  periodMenu "menu-horizontal hidden lg:flex px-1 gap-2"
                  dom'div_ $ el'cn "w-1"
              dom'div do
                el'addCn "navbar-end flex-1 gap-4 overflow-hidden rounded-r-md"
                el'addCn "hover:overflow-visible focus-within:overflow-visible"
                pure $ dom'div do
                  el'addCn "rounded-md flex items-stretch border-1 border-white"
                  el'addCn "bg-gray-300 dark:bg-gray-700 dark:border-black"
                  pure do
                    dom'div do
                      el'addCn "group overflow-hidden relative rounded-l-sm"
                      el'addCn "focus-within:overflow-visible bg-base-300"
                      el'addCn "border-1 border-r-transparent" *> w'BorderG3
                      when isStatsPage $ el'addCn "hover:overflow-visible"
                      pure do
                        dom'div do
                          el'addCn "absolute top-0 left-0 h-full w-full"
                          whenNot isStatsPage $
                            el'addCn "group-focus-within:overflow-hidden"
                          pure do
                            dom'div do
                              el'cn "absolute z-15 right-[-2px] top-0 h-full"
                              -- TODO ActivePlayerIcons
                              pure $ pure unit
                            filterButton
                        playerSearch pass
                    dom'div do
                      el'key $ route'pageKey route
                      el'addCn "lg:hidden dropdown dropdown-hover dropdown-end"
                      el'addCn "join-item border" *> w'BorderG3
                      pure $ do
                        dom'button do
                          el'tabIndex0
                          el'roleButton
                          el'addCn "btn px-1 btn-sm h-full"
                          el'addCn "btn-soft rounded-none border-0"
                          pure $ icon'bars pass
                        periodMenu $ w'str'sp \w -> do
                          w "menu-sm dropdown-content bg-base-200 gap-1"
                          w "rounded-box z-50 w-52 p-2 shadow"
                    dom'div do
                      el'key $ show $ g_ @"!.seasonId" route
                      el'addCn "dropdown dropdown-hover dropdown-end"
                      el'addCn "join-item border rounded-r-md" *> w'BorderG3
                      pure do
                        dom'button do
                          el'tabIndex0
                          el'roleButton
                          el'addCn "btn px-1 btn-sm h-full btn-soft"
                          el'addCn "rounded-none border-0 rounded-r-sm"
                          pure do
                            icon'calendar pass
                            dom'span do
                              el'addCn "text-base text-primary text-left"
                              el'addCn "hidden lg:inline lg:min-w-27"
                              pure $ dom'text title
                        dom'ul do
                          el'tabIndex (-1)
                          el'addCn "dropdown-content menu bg-base-200"
                          el'addCn "rounded-box z-50 w-52 p-2 shadow-sm"
                          pure $ forM_ allSeasonIds \otherSeasonId -> do
                            dom'li do
                              el'key $ show otherSeasonId
                              pure $ dom'a do
                                el'cn $ menuCn $ otherSeasonId == seasonId
                                el'addCn "flex justify-between"
                                router'setHref $ editRoute do
                                  route'gotoSeason otherSeasonId
                                pure do
                                  dom'div $ pure $ dom'text $
                                    seasonId'title otherSeasonId
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
  x'out $ "ROUTE-INFO"
  x'out $ show route
  x'out $ may'or "NO ERROR" $ routeErr <#> (<>) "ERROR: "
  x'out ""
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
        (route'isSamePage route initialRoute)
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
      $ domRef'run'' @"pxPerRemOr_" Nothing
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
