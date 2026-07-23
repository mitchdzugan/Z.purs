module Z.H2h.Node.Builder.Challonge.Impl
  ( getEventData
  ) where

import Prelude

import Z as Z
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
    mapEPupp $ P.goto page url $ Z.xlSet @"waitUntil" $ Z.Just
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
    }
  readPageData page = do
    { slug } <- Z.xAsk
    itemEls <- mapEPupp $ P.els page ".redesigned-meta-list .item"
    Z.forM_ itemEls $ \el -> do
      itemLabel <- mapEPupp $ P.el el ".item-label" >>= P.innerText
      itemText <- mapEPupp $ P.el el ".text" >>= P.innerText
      when (itemLabel == "Start Time" || itemLabel == "Start") do
        Z.xInfo { itemText }
        date <- Z.xMapE H2hE.ParseTime $ Z.xParser itemText parseDate
        Z.xlSet @"eOrDate" $ Z.Right date
        pure unit
      when (itemLabel == "Game") do
        Z.xlSet @"eOrName" $ Z.Right itemText
      when (itemLabel == "Format") do
        Z.xlSet @"isDE" $ itemText == "Double Elimination"
      Z.xInfo { itemLabel }
    name <- Z.xlView @"eOrName" >>= Z.xOk
    date <- Z.xlView @"eOrDate" >>= Z.xOk
    isDE <- Z.xlView @"isDE"
    tournamentName <- mapEPupp $ P.el page ".title #title" >>= P.innerText
    bracketEls <- mapEPupp $ P.els page ".bracket-svg"
    Z.forM_ bracketEls $ \bracketEl -> do
      matchEls <- mapEPupp $ P.els bracketEl ".match"
      Z.forM_ matchEls $ \matchEl -> do
        setId <- mapEPupp $ P.getAttribute matchEl "data-match-id"
        Z.xInfo { setId }
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
  waitFor page sel = do
    Z.xInfo { op: "waitFor", sel }
    pure unit
    P.waitForSelector page sel $ Z.xlSet @"timeout" $ Z.Just 120000
  browserOpts = do
    let uaOpt = "--user-agent=" <> userAgent
    Z.xlSet @"args" [ uaOpt, "--no-sandbox", "--disable-setuid-sandbox" ]
