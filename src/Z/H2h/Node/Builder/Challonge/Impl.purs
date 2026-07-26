module Z.H2h.Node.Builder.Challonge.Impl
  ( getEventData
  ) where

import Prelude

import Z as Z
import Z.Bk.Elimination.Round as Round
import Z.Gql.Node.Module as Gql
import Z.Gql.Warning as GqlW
import Z.Gql.Error as GqlE
import Z.H2h.Error as H2hE
import Z.H2h.Module as H2h
import Z.H2h.Node.Builder.API as B
import Z.H2h.Warning as H2hW
import Z.Puppeteer.Node.Module as P
import Z.Sys.Node.Module as Sys
import Z.Z.Shorthand (__, _o, _o_, g_, jOr, jOr0, jOrF, o_, (~), type (+), type (#>), type (<#))

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder $ Z.xWithRet do
  { client, networkControl, slug } <- Z.xAsk
  let { cachePath } = client
  let eCacheOnlyEmpty = H2hE.Gql GqlE.CacheOnlyEmpty
  cached <- getCached slug cachePath networkControl
  Z.whenJust cached Z.xReturn
  when (networkControl == Gql.CacheOnly) $ Z.xRetFail eCacheOnlyEmpty
  res <- Z.xRetLift getEventDataImpl
  Z.xRetLift $ writeToCache slug cachePath res
  pure res
  where
  fullPath slug path = path Sys./ ("CHALLONGE-" <> slug <> ".json")
  writeToCache _ Z.Nothing _ = pure unit
  writeToCache slug (Z.Just path) res =
    Z.xTellMappedHush (H2hW.Gql <<< GqlW.CacheWrite) $ Sys.encodeTextFileP
      (fullPath slug path)
      res
  getCached _ Z.Nothing _ = pure Z.Nothing
  getCached _ _ Gql.ForceFetch = pure Z.Nothing
  getCached slug (Z.Just path) _ = Z.xTellMappedMHush mapMDecodeErr
    $ Sys.decodeTextFile
    $ fullPath slug path
  mapMDecodeErr e@(Sys.DecodeError _) = [ H2hW.Gql $ GqlW.CacheDecode e ]
  mapMDecodeErr _ = []

getEventDataImpl :: forall x. H2h.Event <# B.BuildX x
getEventDataImpl = do
  P.useBrowser H2hE.PuppeteerBrowserResource browserOpts $ \browser -> do
    Z.xInfo { op: "newPage" }
    page <- pDo "newPage" "" $ P.newPage browser
    Z.xInfo { op: "setViewport" }
    pDo "setViewport" "1920x1080" $ P.setViewport page 1920 1080
    { slug } <- Z.xAsk
    let url = "https://challonge.com/" <> slug
    Z.xInfo { op: "goto", url }
    pDo "goto" url $ P.goto page url $ Z.xSet_ @"waitUntil" $ Z.Just
      P.DOMContentLoaded
    pWaitFor page ".redesigned-meta-list .item .text"
    pWaitFor page ".title #title"
    pWaitFor page ".bracket-svg .match .match--player"
    Z.xEvalS initialState $ readPageData page
  where
  initialState =
    { isDE: false
    , eOrName: Z.Left $ H2hE.MissingData "event.name"
    , eOrDate: Z.Left $ H2hE.MissingData "event.date"
    , baseSets: Z.mapEmpty @Int @BaseSet
    , entrants: Z.mapEmpty @Z.SorN @H2h.Entrant
    }

  readPageData page = do
    { slug } <- Z.xAsk
    itemEls <- pEls page ".redesigned-meta-list .item"
    Z.forM_ itemEls $ \el -> do
      itemLabel <- pEl el ".item-label" >>= pInnerText
      itemText <- pEl el ".text" >>= pInnerText
      when (itemLabel == "Start Time" || itemLabel == "Start") do
        date <- Z.xMapE H2hE.ParseTime $ Z.xParser itemText parseDate
        Z.xSet_ @"eOrDate" $ Z.Right date
        pure unit
      when (itemLabel == "Game") do
        Z.xSet_ @"eOrName" $ Z.Right itemText
      when (itemLabel == "Format") do
        Z.xSet_ @"isDE" $ itemText == "Double Elimination"
    name <- Z.xView_ @"eOrName" >>= Z.xOk
    date <- Z.xView_ @"eOrDate" >>= Z.xOk
    isDE <- Z.xView_ @"isDE"
    tournamentName <- pEl page ".title #title" >>= pInnerText
    bracketEls <- pEls page ".bracket-svg"
    Z.forM_ bracketEls $ \bracketEl -> do
      matchEls <- pEls bracketEl ".match"
      Z.forM_ matchEls $ \matchEl -> Z.xPlusS @"winnerId" Z.Nothing do
        setId <- pReadDataAttr matchEl "match" >>= \s -> Z.xMapE H2hE.ParseTime
          (Z.xParser s Z.parseInt)
        playerEls <- pEls matchEl ".match--player"
        slots <- Z.forM playerEls $ \playerEl -> do
          entrantId <- pReadIdDataAttr playerEl "participant"
          playerName <- pEl playerEl "title" >>= pInnerHtml
          scoreEl <- pEl playerEl ".match--player-score"
          scoreClass <- pGetAttribute scoreEl "class"
          scoreS <- pInnerHtml scoreEl
          score <- Z.xMapE H2hE.ParseTime do
            Z.xParser scoreS Z.parseInt <#> H2h.mkScoreCount
          Z.forM_ (Z.strSplit (Z.Pattern " ") scoreClass) $ \cn -> do
            when (cn == "-winner") $ Z.xSet_ @"winnerId" $ Z.Just entrantId
          Z.xSet (_o @"entrants" $ Z.at entrantId) $ Z.Just
            { id: entrantId
            , standing: { placement: 0, isFinal: false }
            , participants:
                [ { prefix: Z.Nothing
                  , gamerTag: playerName
                  , playerOrder: 1
                  , player:
                      { id: Z.sOrN $ "Challonge-" <> playerName <> "-playerId"
                      , gamerTag: playerName
                      , prefix: Z.Nothing
                      , pronouns: Z.Nothing
                      , name: Z.Nothing
                      , socials: Z.mapEmpty
                      , images: Z.mapEmpty
                      }
                  }
                ]
            }
          pure { entrantId: Z.Just entrantId, score }
        let emptySlot = { entrantId: Z.Nothing, score: H2h.NoScore }
        let slotA = jOr emptySlot (Z.nth slots 0)
        let slotB = jOr emptySlot (Z.nth slots 1)
        winnerId <- Z.xView_ @"winnerId"
        let
          winner =
            if Z.isNothing winnerId then Z.Nothing
            else if winnerId == slotA.entrantId then Z.Just Z.Pos
            else Z.Just Z.Neg
        let baseSet = { winner, id: setId, slots: slotA Z.~ slotB }
        Z.xOver_ @"baseSets" $ Z.mapSet setId baseSet
    baseSetList <- Z.xView_ @"baseSets" <#>
      Z.arrReverse <<< Z.arrSortWith (g_ @"id") <<< Z.arrFromFoldable
    isComplete <- Z.xWithRet do
      Z.forM_ baseSetList $ \baseSet -> do
        when (Z.isNothing baseSet.winner) (Z.xReturn false)
      pure true
    let
      setsLoopState =
        { prev: Z.Nothing
        , depth: 0
        , roundInd: 0
        , isDropRound: true
        , hasReset: false
        , gfEIds: Z.setEmpty @Z.SorN
        , nonGfEIds: Z.setEmpty @Z.SorN
        }
    roundSets <- Z.xMergeS setsLoopState $ do
      roundSets' <- Z.forM baseSetList $ \baseSet -> do
        { prev, isDropRound } <- Z.xGet
        let prevSet = prev <#> \p -> p.base
        let prevRound = prev <#> \p -> p.round
        let wasGrands = jOrF $ prevRound <#> Round.isGrands
        let wasLosers = jOrF $ prevRound <#> Round.isLosers
        let sameSlots = (slotsKey baseSet) == mSlotsKey prevSet
        let isGrands = Z.isNothing prev && isDE || (wasGrands && sameSlots)
        when (isGrands && wasGrands) $ Z.xSet_ @"hasReset" true
        Z.forM_ (Z.arrFromFoldable baseSet.slots) $ \slot -> do
          Z.whenJust slot.entrantId $ \entrantId -> do
            Z.setAdd entrantId #
              Z.xOver (if isGrands then __ @"gfEIds" else __ @"nonGfEIds")
        { gfEIds, nonGfEIds } <- Z.xGet
        seenAllGFEntrants <- Z.xWithRet do
          Z.forM_ (Z.arrFromFoldable gfEIds) $ \id -> do
            when (not (Z.setHas id nonGfEIds)) (Z.xReturn false)
          pure true
        let nowLosers = (not isGrands) && (wasGrands || wasLosers)
        let isLosers = nowLosers && not seenAllGFEntrants
        when ((not isLosers && wasLosers) || (not isGrands && wasGrands)) do
          Z.xSet_ @"depth" 0
          Z.xSet_ @"roundInd" 0
        setDepth <- Z.xView_ @"depth"
        let round = elimRound isDE setDepth isGrands isLosers isDropRound
        let slotA Z.~ slotB = baseSet.slots
        Z.whenJust slotA.entrantId \eA -> Z.whenJust slotB.entrantId \eB -> do
          Z.whenJust baseSet.winner \w -> when isComplete do
            let
              setFinStanding = \id placement -> Z.xSet
                (_o_ @"entrants" @"standing" $ Z.ix id)
                { placement, isFinal: true }
            let wId Z.~ lId = if w == Z.Pos then eA Z.~ eB else eB Z.~ eA
            if isGrands && not wasGrands then do
              setFinStanding wId 1
              setFinStanding lId 2
            else if isLosers then do
              let p2Inc = Z.inc $ Z.p2 setDepth
              setFinStanding lId $ if isDropRound then p2Inc else 3 * p2Inc / 2
            else if isDE && setDepth == 0 then do
              setFinStanding wId 1
              setFinStanding lId 2
            else if isDE then do
              setFinStanding lId $ Z.inc $ Z.p2 setDepth
            else pure unit
        Z.xOver_ @"roundInd" Z.inc
        { depth, roundInd } <- Z.xGet
        when (Z.p2 depth <= roundInd) do
          Z.xSet_ @"roundInd" 0
          if (isDropRound && isLosers) then do
            Z.xSet_ @"isDropRound" false
          else do
            Z.xSet_ @"isDropRound" true
            Z.xOver_ @"depth" Z.inc
        Z.xSet_ @"prev" $ Z.Just { base: baseSet, round }
        pure $
          { round
          , set:
              { isDQ: false
              , isBye: false
              , roundText: ""
              , overrideScoreText: Z.Nothing
              , doesCount: isComplete
              , id: baseSet.id
              , winner: baseSet.winner
              , slots: baseSet.slots
              }
          }
      { hasReset } <- Z.xGet
      if (not hasReset) then pure roundSets'
      else pure $ Z.set (Z.ix 0 # o_ @"round") (Round.Grands true) roundSets'
    let lSets = Z.arrFilter (Round.isLosers <<< g_ @"round") roundSets
    let wSets = Z.arrFilter (Round.isWinners <<< g_ @"round") roundSets
    let maxWR = jOr0 $ Z.maximum $ wSets <#> Round.roundTypeInd <<< g_ @"round"
    let maxLR = jOr0 $ Z.maximum $ lSets <#> Round.roundTypeInd <<< g_ @"round"
    { entrants } <- Z.xGet
    let profileImageUrl = "https://i.imgur.com/7MsdKge.jpeg"
    let mkSet = \rs -> rs.set `(~) @"roundText"` roundLabel rs.round maxWR maxLR
    let mkSetT = \rs -> Z.Tuple rs.set.id $ mkSet rs
    let sets = Z.mapFromFoldable $ roundSets <#> mkSetT
    pure
      { id: Z.sOrN $ "Challonge-" <> slug <> "-eventId"
      , name
      , slug
      , state: if isComplete then "COMPLETE" else "ACTIVE"
      , site: H2h.Challonge
      , phaseGroups:
          [ { id: Z.sOrN 1
            , phase: { id: Z.sOrN 1, name: "Bracket", phaseOrder: 1 }
            , displayIdentifier: "1"
            , sets
            }
          ]
      , entrants
      , tournament:
          { id: Z.sOrN $ "Challonge-" <> slug <> "-tournamentId"
          , name: tournamentName
          , images: Z.mapFromFoldable [ "profile" Z./\ profileImageUrl ]
          , date: date
          }
      }
  browserOpts = do
    let uaOpt = "--user-agent=" <> userAgent
    Z.xSet_ @"args" [ uaOpt, "--no-sandbox", "--disable-setuid-sandbox" ]
  mSlotsKey (Z.Just { slots: (eA Z.~ eB) }) =
    Z.strJoinWith "|" $ Z.arrSort
      [ mEntrantIdKey eA.entrantId, mEntrantIdKey eB.entrantId ]
  mSlotsKey _ = "__"
  slotsKey s = mSlotsKey $ Z.Just s
  mEntrantIdKey (Z.Just id) = show id
  mEntrantIdKey _ = "_"
  -- elimRound isDE depth isGrands isLosers isDropRound
  elimRound _ _ true _ _ = Round.Grands false
  elimRound isDE depth _ false _ = Round.Winners isDE depth
  elimRound _ depth _ _ isDropRound = Round.Losers isDropRound depth
  roundLabel (Round.Grands true) _ _ = "Finals (reset)"
  roundLabel (Round.Grands _) _ _ = "Finals"
  roundLabel s@(Round.Losers _ _) _ maxL = (<>) "Losers Round " $ show
    $ maxL - Round.roundTypeInd s + 1
  roundLabel (Round.Winners true 0) _ _ = "Semifinals"
  roundLabel (Round.Winners false 0) _ _ = "Finals"
  roundLabel (Round.Winners false 1) _ _ = "Semifinals"
  roundLabel s@(Round.Winners _ _) maxW _ = (<>) "Winners Round " $ show
    $ maxW - Round.roundTypeInd s + 1

  pDo
    :: forall xx a
     . String
    -> String
    -> Z.E Z.JsError + Z.EA H2hE.T xx #> a
    -> Z.EA H2hE.T xx #> a
  pDo s1 s2 m = Z.xMapE (H2hE.Puppeteer s1 s2) m

  pDoPorE
    :: forall xx pOrE a
     . P.IsPageOrElement pOrE
    => pOrE
    -> String
    -> Z.E Z.JsError + Z.EA H2hE.T xx #> a
    -> Z.EA H2hE.T xx #> a
  pDoPorE pOrE s m = Z.xMapE (H2hE.Puppeteer (P.context pOrE) s) m

  pEls
    :: forall xx pOrE
     . P.IsPageOrElement pOrE
    => pOrE
    -> String
    -> Z.EA H2hE.T xx #> Array P.Element
  pEls pOrE sel = pDoPorE pOrE sel $ P.els pOrE sel

  pEl
    :: forall xx pOrE
     . P.IsPageOrElement pOrE
    => pOrE
    -> String
    -> Z.EA H2hE.T xx #> P.Element
  pEl pOrE sel = pDoPorE pOrE sel $ P.el pOrE sel

  pInnerText pOrE = pDoPorE pOrE "innerText" $ P.innerText pOrE
  pInnerHtml pOrE = pDoPorE pOrE "innerHtml" $ P.innerHtml pOrE
  pGetAttribute pOrE attr = pDoPorE pOrE ("getAttribute('" <> attr <> "')") $
    P.getAttribute pOrE attr
  pReadDataAttr e n = pGetAttribute e ("data-" <> n <> "-id")
  pReadIdDataAttr e n = pReadDataAttr e n <#> Z.sOrN
  pWaitFor page sel = pDo "waitFor" sel do
    Z.xInfo { op: "waitFor", sel }
    P.waitForSelector page sel $ Z.xSet_ @"timeout" $ Z.Just 120000

userAgent :: String
userAgent =
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"

parseMonth :: forall m. Z.ParserT String m Z.Month
parseMonth = Z.parseTry (Z.parseStringAs "January" Z.January)
  Z.<|> Z.parseTry (Z.parseStringAs "February" Z.February)
  Z.<|> Z.parseTry (Z.parseStringAs "March" Z.March)
  Z.<|> Z.parseTry (Z.parseStringAs "April" Z.April)
  Z.<|> Z.parseTry (Z.parseStringAs "May" Z.May)
  Z.<|> Z.parseTry (Z.parseStringAs "June" Z.June)
  Z.<|> Z.parseTry (Z.parseStringAs "July" Z.July)
  Z.<|> Z.parseTry (Z.parseStringAs "August" Z.August)
  Z.<|> Z.parseTry (Z.parseStringAs "September" Z.September)
  Z.<|> Z.parseTry (Z.parseStringAs "October" Z.October)
  Z.<|> Z.parseTry (Z.parseStringAs "November" Z.November)
  Z.<|> Z.parseTry (Z.parseStringAs "December" Z.December)
  Z.<|> Z.parseFail "Expected %Month%"

parseAMorPM :: forall m. Z.ParserT String m Boolean
parseAMorPM = Z.parseTry (Z.parseStringAs "AM" false)
  Z.<|> Z.parseTry (Z.parseStringAs "PM" true)
  Z.<|> Z.parseFail "Expected AM|PM"

parseDate :: forall m. Z.ParserT String m Z.DateTime
parseDate = do
  month <- parseMonth
  Z.parseString_ " "
  day <- Z.parseInt <#> Z.toEnum @Z.Day >>= mOr "invalid day"
  Z.parseString_ ","
  Z.parseString_ " "
  year <- Z.parseInt <#> Z.toEnum @Z.Year >>= mOr "invalid year"
  Z.parseString_ " at "
  hour <- Z.parseInt <#> Z.toEnum @Z.Hour >>= mOr "invalid hour"
  let date = Z.canonicalDate year month day
  Z.parseString_ ":"
  m <- Z.parseInt <#> Z.toEnum @Z.Minute >>= mOr "invalid minute"
  s <- pure 0 <#> Z.toEnum @Z.Second >>= mOr "invalid second"
  ms <- pure 0 <#> Z.toEnum @Z.Millisecond >>= mOr "invalid millisecond"
  let time = Z.Time hour m s ms
  let rawDatetime = Z.DateTime date time
  Z.parseString_ " "
  isPM <- parseAMorPM
  let hOff = Z.Hours $ Z.toNumber $ if isPM then 12 else 0
  dAdjust hOff rawDatetime
  where
  mOr :: forall mm a. String -> Z.Maybe a -> Z.ParserT String mm a
  mOr s Z.Nothing = Z.parseFail s
  mOr _ (Z.Just y) = pure y
  dAdjust d dt = mOr "invalid date adjustment" $ Z.adjustDateTime d dt

type BaseSet =
  { id :: Int
  , winner :: Z.Maybe Z.PairKey
  , slots :: Z.Pair H2h.Slot
  }