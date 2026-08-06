module Node.Z.H2h.Challonge
  ( getEventData
  ) where

import Node.Z.Prelude
import Z.Bk.Elimination.Round as Round
import Z.Gql.Warning as GqlW
import Z.Gql.Error as GqlE
import Z.H2h.Error as H2hE
import Z.H2h.Module as H2h
import Z.H2h.Warning as H2hW
import Node.Z.Gql as Gql
import Node.Z.H2h.Builder as B
import Node.Z.Puppeteer as P

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder $ mkDim @WithReturn \xReturn -> do
  { client, networkControl, slug } <- mkDim @Ask
  let { cachePath } = client
  let eCacheOnlyEmpty = H2hE.Gql GqlE.CacheOnlyEmpty
  cached <- getCached slug cachePath networkControl
  whenJust cached xReturn
  when (networkControl == Gql.CacheOnly) $ mkDim @Fail eCacheOnlyEmpty
  res <- getEventDataImpl
  writeToCache slug cachePath res
  pure res
  where
  fullPath slug path = path /./ ("CHALLONGE-" <> slug <> ".json")
  writeToCache _ Nothing _ = pure unit
  writeToCache slug (Just path) res =
    mkDim @TellMappedHush (H2hW.Gql <<< GqlW.CacheWrite) $ xEncodeTextFileP
      (fullPath slug path)
      res
  getCached _ Nothing _ = pure Nothing
  getCached _ _ Gql.ForceFetch = pure Nothing
  getCached slug (Just path) _ = mkDim @TellMappedMHush mapMDecodeErr
    $ xDecodeTextFile
    $ fullPath slug path
  mapMDecodeErr e@(DecodeError _) = [ H2hW.Gql $ GqlW.CacheDecode e ]
  mapMDecodeErr _ = []

getEventDataImpl :: forall x. H2h.Event <# B.BuildX x
getEventDataImpl = do
  P.xUseBrowser H2hE.PuppeteerBrowserResource browserOpts $ \browser -> do
    xInfo { op: "newPage" }
    page <- pDo "newPage" "" $ P.xNewPage browser
    xInfo { op: "xSetViewport" }
    pDo "xSetViewport" "1920x1080" $ P.xSetViewport page 1920 1080
    { slug } <- mkDim @Ask
    let url = "https://challonge.com/" <> slug
    xInfo { op: "xGoto", url }
    pDo "xGoto" url $ P.xGoto page url $ x' @"~waitUntil" $ Just
      P.DOMContentLoaded
    pWaitFor page ".redesigned-meta-list .item .text"
    pWaitFor page ".title #title"
    pWaitFor page ".bracket-svg .match .match--player"
    mkDim @EvalS initialState $ readPageData page
  where
  initialState =
    { isDE: false
    , nameOrE: Left $ H2hE.MissingData "event.name"
    , dateOrE: Left $ H2hE.MissingData "event.date"
    , baseSets: mapEmpty @Int @BaseSet
    , entrants: mapEmpty @SorN @H2h.Entrant
    }

  readPageData page = do
    { slug } <- mkDim @Ask
    itemEls <- pEls page ".redesigned-meta-list .item"
    forM_ itemEls $ \el -> do
      itemLabel <- pEl el ".item-label" >>= pInnerText
      itemText <- pEl el ".text" >>= pInnerText
      when (itemLabel == "Start Time" || itemLabel == "Start") do
        date <- mkDim @MapE H2hE.ParseTime $ mkDim @RunParser itemText
          parseDate
        x' @"~dateOrE" $ Right date
        pure unit
      when (itemLabel == "Game") do
        x' @"~nameOrE" $ Right itemText
      when (itemLabel == "Format") do
        x' @"~isDE" $ itemText == "Double Elimination"
    name <- x' @"-.nameOrE" >>= x' @"ok"
    date <- x' @"-.dateOrE" >>= x' @"ok"
    isDE <- x' @"-.isDE"
    tournamentName <- pEl page ".title #title" >>= pInnerText
    bracketEls <- pEls page ".bracket-svg"
    forM_ bracketEls $ \bracketEl -> do
      matchEls <- pEls bracketEl ".match"
      forM_ matchEls $ \matchEl -> x' @"-+winnerId" Nothing do
        setId <- pReadDataAttr matchEl "match" >>= \s -> mkDim @MapE
          H2hE.ParseTime
          (mkDim @RunParser s parseInt)
        playerEls <- pEls matchEl ".match--player"
        slots <- forM playerEls $ \playerEl -> do
          entrantId <- pReadIdDataAttr playerEl "participant"
          playerName <- pEl playerEl "title" >>= pInnerHtml
          scoreEl <- pEl playerEl ".match--player-score"
          scoreClass <- pGetAttribute scoreEl "class"
          scoreS <- pInnerHtml scoreEl
          score <- mkDim @MapE H2hE.ParseTime do
            mkDim @RunParser scoreS parseInt <#> H2h.mkScoreCount
          forM_ (strSplit (Pattern " ") scoreClass) $ \cn -> do
            when (cn == "-winner") $ x' @"~winnerId" $ Just entrantId
          x' @"set" (_o @"entrants" $ at entrantId) $ Just
            { id: entrantId
            , standing: { placement: 0, isFinal: false }
            , participants:
                [ { prefix: Nothing
                  , gamerTag: playerName
                  , playerOrder: 1
                  , player:
                      { id: sOrN $ "Challonge-" <> playerName <> "-playerId"
                      , gamerTag: playerName
                      , prefix: Nothing
                      , pronouns: Nothing
                      , name: Nothing
                      , socials: mapEmpty
                      , images: mapEmpty
                      }
                  }
                ]
            }
          pure { entrantId: Just entrantId, score }
        let emptySlot = { entrantId: Nothing, score: H2h.NoScore }
        let slotA = jOr emptySlot (nth slots 0)
        let slotB = jOr emptySlot (nth slots 1)
        winnerId <- x' @"-.winnerId"
        let
          winner =
            if isNothing winnerId then Nothing
            else if winnerId == slotA.entrantId then Just Pos
            else Just Neg
        let baseSet = { winner, id: setId, slots: slotA ~ slotB }
        x' @"%baseSets" (mapSet setId baseSet)
    baseSetList <- x' @"-.baseSets"
      <#> arrReverse
      <<< arrSortWith (g_ @"id")
      <<< arrFromFoldable
    isComplete <- x' @"withReturn" \xReturn -> do
      forM_ baseSetList $ \baseSet -> do
        when (isNothing baseSet.winner) (xReturn false)
      pure true
    let
      setsLoopState =
        { prev: Nothing
        , depth: 0
        , roundInd: 0
        , isDropRound: true
        , hasReset: false
        , gfEIds: setEmpty @SorN
        , nonGfEIds: setEmpty @SorN
        }
    roundSets <- mkDimAt @"setsLoop" @EvalS setsLoopState $ do
      roundSets' <- forM baseSetList $ \baseSet -> do
        { prev, isDropRound } <- x @"setsLoop" @"get"
        let prevSet = prev <#> \p -> p.base
        let prevRound = prev <#> \p -> p.round
        let wasGrands = jOrF $ prevRound <#> Round.isGrands
        let wasLosers = jOrF $ prevRound <#> Round.isLosers
        let sameSlots = (slotsKey baseSet) == mSlotsKey prevSet
        let isGrands = isNothing prev && isDE || (wasGrands && sameSlots)
        when (isGrands && wasGrands) $ x @"setsLoop" @"~hasReset" true
        forM_ (arrFromFoldable baseSet.slots) $ \slot -> do
          whenJust slot.entrantId $ \entrantId -> do
            setAdd entrantId #
              mkDimAt @"setsLoop" @Over
                (if isGrands then __ @"gfEIds" else __ @"nonGfEIds")
        { gfEIds, nonGfEIds } <- x @"setsLoop" @"get"
        seenAllGFEntrants <- x' @"withReturn" \xReturn -> do
          forM_ (arrFromFoldable gfEIds) $ \id -> do
            when (not (setHas id nonGfEIds)) (xReturn false)
          pure true
        let nowLosers = (not isGrands) && (wasGrands || wasLosers)
        let isLosers = nowLosers && not seenAllGFEntrants
        when ((not isLosers && wasLosers) || (not isGrands && wasGrands)) do
          x @"setsLoop" @"~depth" 0
          x @"setsLoop" @"~roundInd" 0
        setDepth <- x @"setsLoop" @"-.depth"
        let round = elimRound isDE setDepth isGrands isLosers isDropRound
        let slotA ~ slotB = baseSet.slots
        whenJust slotA.entrantId \eA -> whenJust slotB.entrantId \eB -> do
          whenJust baseSet.winner \w -> when isComplete do
            let
              setFinStanding = \id placement -> x' @"set"
                (_o_ @"entrants" @"standing" $ ix id)
                { placement, isFinal: true }
            let wId ~ lId = if w == Pos then eA ~ eB else eB ~ eA
            if isGrands && not wasGrands then do
              setFinStanding wId 1
              setFinStanding lId 2
            else if isLosers then do
              let p2Inc = inc $ p2 setDepth
              setFinStanding lId $ if isDropRound then p2Inc else 3 * p2Inc / 2
            else if isDE && setDepth == 0 then do
              setFinStanding wId 1
              setFinStanding lId 2
            else if isDE then do
              setFinStanding lId $ inc $ p2 setDepth
            else pure unit
        x @"setsLoop" @"%roundInd" inc
        { depth, roundInd } <- x @"setsLoop" @"get"
        when (p2 depth <= roundInd) do
          x @"setsLoop" @"~roundInd" 0
          if (isDropRound && isLosers) then do
            x @"setsLoop" @"~isDropRound" false
          else do
            x @"setsLoop" @"~isDropRound" true
            x @"setsLoop" @"%depth" inc
        x @"setsLoop" @"~prev" $ Just { base: baseSet, round }
        pure $
          { round
          , set:
              { isDQ: false
              , isBye: false
              , roundText: ""
              , overrideScoreText: Nothing
              , doesCount: isComplete
              , id: baseSet.id
              , winner: baseSet.winner
              , slots: baseSet.slots
              }
          }
      { hasReset } <- x @"setsLoop" @"get"
      if (not hasReset) then pure roundSets'
      else pure $ set (ix 0 # o_ @"round") (Round.Grands true) roundSets'
    let lSets = arrFilter (Round.isLosers <<< g_ @"round") roundSets
    let wSets = arrFilter (Round.isWinners <<< g_ @"round") roundSets
    let maxWR = jOr0 $ maximum $ wSets <#> Round.roundTypeInd <<< g_ @"round"
    let maxLR = jOr0 $ maximum $ lSets <#> Round.roundTypeInd <<< g_ @"round"
    { entrants } <- x' @"get"
    let profileImageUrl = "https://i.imgur.com/7MsdKge.jpeg"
    let
      mkSet = \rs -> rs.set `(~.) @"roundText"` roundLabel rs.round maxWR maxLR
    let mkSetT = \rs -> Tuple rs.set.id $ mkSet rs
    let sets = mapFromFoldable $ roundSets <#> mkSetT
    pure
      { id: sOrN $ "Challonge-" <> slug <> "-eventId"
      , name
      , slug
      , state: if isComplete then "COMPLETE" else "ACTIVE"
      , site: H2h.Challonge
      , phaseGroups:
          [ { id: sOrN 1
            , phase: { id: sOrN 1, name: "Bracket", phaseOrder: 1 }
            , displayIdentifier: "1"
            , sets
            }
          ]
      , entrants
      , tournament:
          { id: sOrN $ "Challonge-" <> slug <> "-tournamentId"
          , name: tournamentName
          , images: mapFromFoldable [ "profile" /\ profileImageUrl ]
          , date: date
          }
      }
  browserOpts = do
    let uaOpt = "--user-agent=" <> userAgent
    x' @"~args" [ uaOpt, "--no-sandbox", "--disable-setuid-sandbox" ]
  mSlotsKey (Just { slots: (eA ~ eB) }) =
    strJoinWith "|" $ arrSort
      [ mEntrantIdKey eA.entrantId, mEntrantIdKey eB.entrantId ]
  mSlotsKey _ = "__"
  slotsKey s = mSlotsKey $ Just s
  mEntrantIdKey (Just id) = show id
  mEntrantIdKey _ = "_"
  -- elimRound isDE depth isGrands isLosers isDropRound
  elimRound _ _ true _ _ = Round.Grands false
  elimRound isDE depth _ false _ = Round.Winners isDE depth
  elimRound _ depth _ _ isDropRound = Round.Losers isDropRound depth
  roundLabel (Round.Grands true) _ _ = "Finals (reset)"
  roundLabel (Round.Grands _) _ _ = "Finals"
  roundLabel s@(Round.Losers _ _) _ maxL = (<>) "Losers Round " $ show
    $ maxL
    - Round.roundTypeInd s
    + 1
  roundLabel (Round.Winners true 0) _ _ = "Semifinals"
  roundLabel (Round.Winners false 0) _ _ = "Finals"
  roundLabel (Round.Winners false 1) _ _ = "Semifinals"
  roundLabel s@(Round.Winners _ _) maxW _ = (<>) "Winners Round " $ show
    $ maxW
    - Round.roundTypeInd s
    + 1

  pDo
    :: forall xx a
     . String
    -> String
    -> E JsError + EA H2hE.T xx #> a
    -> EA H2hE.T xx #> a
  pDo s1 s2 m = mkDim @MapE (H2hE.Puppeteer s1 s2) m

  pDoPorE
    :: forall xx pOrE a
     . P.IsPageOrElement pOrE
    => pOrE
    -> String
    -> E JsError + EA H2hE.T xx #> a
    -> EA H2hE.T xx #> a
  pDoPorE pOrE s m = mkDim @MapE (H2hE.Puppeteer (P.context pOrE) s) m

  pEls
    :: forall xx pOrE
     . P.IsPageOrElement pOrE
    => pOrE
    -> String
    -> EA H2hE.T xx #> Array P.Element
  pEls pOrE sel = pDoPorE pOrE sel $ P.xEls pOrE sel

  pEl
    :: forall xx pOrE
     . P.IsPageOrElement pOrE
    => pOrE
    -> String
    -> EA H2hE.T xx #> P.Element
  pEl pOrE sel = pDoPorE pOrE sel $ P.xEl pOrE sel

  pInnerText pOrE = pDoPorE pOrE "innerText" $ P.xInnerText pOrE
  pInnerHtml pOrE = pDoPorE pOrE "innerHtml" $ P.xInnerHtml pOrE
  pGetAttribute pOrE attr = pDoPorE pOrE ("getAttribute('" <> attr <> "')") $
    P.xGetAttribute pOrE attr
  pReadDataAttr e n = pGetAttribute e ("data-" <> n <> "-id")
  pReadIdDataAttr e n = pReadDataAttr e n <#> sOrN
  pWaitFor page sel = pDo "waitFor" sel do
    xInfo { op: "waitFor", sel }
    P.xWaitForSelector page sel $ x' @"~timeout" $ Just 120000

userAgent :: String
userAgent =
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"

parseMonth :: forall m. ParserT String m Month
parseMonth = parseTry (parseStringAs "January" January)
  <|> parseTry (parseStringAs "February" February)
  <|> parseTry (parseStringAs "March" March)
  <|> parseTry (parseStringAs "April" April)
  <|> parseTry (parseStringAs "May" May)
  <|> parseTry (parseStringAs "June" June)
  <|> parseTry (parseStringAs "July" July)
  <|> parseTry (parseStringAs "August" August)
  <|> parseTry (parseStringAs "September" September)
  <|> parseTry (parseStringAs "October" October)
  <|> parseTry (parseStringAs "November" November)
  <|> parseTry (parseStringAs "December" December)
  <|> parseFail "Expected %Month%"

parseAMorPM :: forall m. ParserT String m Boolean
parseAMorPM = parseTry (parseStringAs "AM" false)
  <|> parseTry (parseStringAs "PM" true)
  <|> parseFail "Expected AM|PM"

parseDate :: forall m. ParserT String m DateTime
parseDate = do
  month <- parseMonth
  parseString_ " "
  day <- parseInt <#> toEnum @Day >>= mOr "invalid day"
  parseString_ ","
  parseString_ " "
  year <- parseInt <#> toEnum @Year >>= mOr "invalid year"
  parseString_ " at "
  hour <- parseInt <#> toEnum @Hour >>= mOr "invalid hour"
  let date = canonicalDate year month day
  parseString_ ":"
  m <- parseInt <#> toEnum @Minute >>= mOr "invalid minute"
  s <- pure 0 <#> toEnum @Second >>= mOr "invalid second"
  ms <- pure 0 <#> toEnum @Millisecond >>= mOr "invalid millisecond"
  let time = Time hour m s ms
  let rawDatetime = DateTime date time
  parseString_ " "
  isPM <- parseAMorPM
  let hOff = Hours $ toNumber $ if isPM then 12 else 0
  dAdjust hOff rawDatetime
  where
  mOr :: forall mm a. String -> Maybe a -> ParserT String mm a
  mOr s Nothing = parseFail s
  mOr _ (Just y) = pure y
  dAdjust d dt = mOr "invalid date adjustment" $ adjustDateTime d dt

type BaseSet =
  { id :: Int
  , winner :: Maybe PairKey
  , slots :: Pair H2h.Slot
  }
