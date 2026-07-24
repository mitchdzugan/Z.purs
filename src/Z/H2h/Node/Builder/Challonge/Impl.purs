module Z.H2h.Node.Builder.Challonge.Impl
  ( getEventData
  ) where

import Prelude

import Z (_o, o_)
import Z as Z
import Z.Bk.Elimination.Round as ElimRound
import Z.H2h.Error as H2hE
import Z.H2h.Module as H2h
import Z.H2h.Node.Builder.API as B
import Z.Puppeteer.Node.Module as P

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
  , winnerId :: Z.Maybe Z.SorN
  , slots :: Z.Pair H2h.Slot
  }

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder do
  P.useBrowser H2hE.PuppeteerBrowserResource browserOpts $ \browser -> do
    Z.xInfo { op: "newPage" }
    page <- mapEPupp $ P.newPage browser
    Z.xInfo { op: "setViewport" }
    mapEPupp $ P.setViewport page 1920 1080
    { slug } <- Z.xAsk
    let url = "https://challonge.com/" <> slug
    Z.xInfo { op: "goto", url }
    mapEPupp $ P.goto page url $ Z.xSet_ @"waitUntil" $ Z.Just
      P.DOMContentLoaded
    mapEPupp $ waitFor page ".redesigned-meta-list .item .text"
    mapEPupp $ waitFor page ".title #title"
    mapEPupp $ waitFor page ".bracket-svg .match .match--player"
    Z.xEvalS initialState $ readPageData page
  where
  mapEPupp
    :: forall xx a
     . Z.X (Z.E Z.JsError (Z.E H2hE.T xx)) a
    -> Z.X (Z.E H2hE.T xx) a
  mapEPupp m = Z.xMapE H2hE.UnkPupp m
  initialState =
    { isDE: false
    , eOrName: Z.Left $ H2hE.MissingData "event.name"
    , eOrDate: Z.Left $ H2hE.MissingData "event.date"
    , baseSets: Z.mapEmpty @Int @BaseSet
    , entrants: Z.mapEmpty @Z.SorN @H2h.Entrant
    }
  readPageData page = do
    { slug } <- Z.xAsk
    itemEls <- mapEPupp $ P.els page ".redesigned-meta-list .item"
    Z.forM_ itemEls $ \el -> do
      itemLabel <- mapEPupp $ P.el el ".item-label" >>= P.innerText
      itemText <- mapEPupp $ P.el el ".text" >>= P.innerText
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
    tournamentName <- mapEPupp $ P.el page ".title #title" >>= P.innerText
    bracketEls <- mapEPupp $ P.els page ".bracket-svg"
    Z.forM_ bracketEls $ \bracketEl -> do
      matchEls <- mapEPupp $ P.els bracketEl ".match"
      Z.forM_ matchEls $ \matchEl -> Z.xPlusS @"winnerId" Z.Nothing do
        setId <- readDataAttr matchEl "match" >>= \s -> Z.xMapE H2hE.ParseTime
          (Z.xParser s Z.parseInt)
        playerEls <- mapEPupp $ P.els matchEl ".match--player"
        slots <- Z.forM playerEls $ \playerEl -> do
          entrantId <- readIdDataAttr playerEl "participant"
          playerName <- mapEPupp $ (P.el playerEl "title" >>= P.innerHtml)
          scoreEl <- mapEPupp $ P.el playerEl ".match--player-score"
          scoreClass <- mapEPupp $ P.getAttribute scoreEl "class"
          scoreS <- mapEPupp $ P.innerHtml scoreEl
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
        let slotA = Z.fromMaybe emptySlot (Z.nth slots 0)
        let slotB = Z.fromMaybe emptySlot (Z.nth slots 1)
        winnerId <- Z.xView_ @"winnerId"
        let baseSet = { winnerId, id: setId, slots: slotA Z.~ slotB }
        Z.xOver_ @"baseSets" $ Z.mapSet setId baseSet

    baseSetList <- Z.xView_ @"baseSets" <#>
      Z.arrReverse <<< Z.arrSortWith (Z.lview @"id") <<< Z.arrFromFoldable

    isComplete <- Z.xWithRet do
      Z.forM_ baseSetList $ \baseSet -> do
        when (Z.isNothing baseSet.winnerId) (Z.xReturn false)
      pure true

    let
      setsLoopState =
        { prev: Z.Nothing :: Z.Maybe { base :: BaseSet, round :: ElimRound.T }
        , depth: 0
        , roundInd: 0
        , isDropRound: true
        , hasReset: false
        , gfEntrants: Z.setEmpty @Z.SorN
        , nonGfEntrants: Z.setEmpty @Z.SorN
        }
    sets <- Z.xEvalS setsLoopState $ do
      sets' <- Z.forM baseSetList $ \baseSet -> do
        { prev, isDropRound } <- Z.xGet
        let
          prevSet = prev <#> \p -> p.base
          prevRound = prev <#> \p -> p.round
          wasGrands = Z.fromMaybe false $ prevRound <#> ElimRound.isGrands
          wasLosers = Z.fromMaybe false $ prevRound <#> ElimRound.isLosers
          isGrands = Z.isNothing prev && isDE ||
            (wasGrands && (slotsKey baseSet) == mSlotsKey prevSet)
        when (isGrands && wasGrands) $ Z.xSet_ @"hasReset" true
        Z.forM_ (Z.arrFromFoldable baseSet.slots) $ \slot -> do
          Z.whenJust slot.entrantId $ \entrantId -> do
            Z.setAdd entrantId #
              ( if isGrands then Z.xOver_ @"gfEntrants"
                else Z.xOver_ @"nonGfEntrants"
              )
        { gfEntrants, nonGfEntrants } <- Z.xGet
        seenAllGFEntrants <- Z.xWithRet do
          Z.forM_ (Z.arrFromFoldable gfEntrants) $ \id -> do
            when (not (Z.setHas id nonGfEntrants)) (Z.xReturn false)
          pure true
        let nowLosers = (not isGrands) && (wasGrands || wasLosers)
        let isLosers = nowLosers && not seenAllGFEntrants
        when ((not isLosers && wasLosers) || (not isGrands && wasGrands)) do
          Z.xSet_ @"depth" 0
          Z.xSet_ @"roundInd" 0
        { depth, roundInd } <- Z.xGet
        let round = elimRound isDE depth isGrands isLosers isDropRound

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
              , winnerId: baseSet.winnerId
              , slots: baseSet.slots
              }
          }
      { hasReset } <- Z.xGet
      if (not hasReset) then pure sets'
      else pure $ Z.set (Z.ix 0 # o_ @"round") (ElimRound.Grands true) sets'

    Z.xInfo sets

    pure
      { id: Z.sOrN $ "Challonge-" <> slug <> "-eventId"
      , name
      , slug
      , state: "COMPLETE"
      , site: H2h.Challonge
      , phaseGroups: Z.arrEmpty @H2h.PhaseGroup
      , entrants: Z.mapEmpty @Z.SorN @H2h.Entrant
      , tournament:
          { id: Z.sOrN $ "Challonge-" <> slug <> "-tournamentId"
          , name: tournamentName
          , images: Z.mapEmpty
          , date: date
          }
      }
  readDataAttr e n = mapEPupp $ P.getAttribute e ("data-" <> n <> "-id")
  readIdDataAttr e n = readDataAttr e n <#> Z.sOrN
  waitFor page sel = do
    Z.xInfo { op: "waitFor", sel }
    pure unit
    P.waitForSelector page sel $ Z.xSet_ @"timeout" $ Z.Just 120000
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
  elimRound _ _ true _ _ = ElimRound.Grands false
  elimRound isDE depth _ false _ = ElimRound.Winners isDE depth
  elimRound _ depth _ _ isDropRound = ElimRound.Losers isDropRound depth