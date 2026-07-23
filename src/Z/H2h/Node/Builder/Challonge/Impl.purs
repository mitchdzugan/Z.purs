module Z.H2h.Node.Builder.Challonge.Impl
  ( getEventData
  ) where

import Prelude

import Z as Z
import Z.H2h.Module as H2h
import Z.H2h.Node.Builder.API as B
import Z.Puppeteer.Node.Module as P

userAgent :: String
userAgent =
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"

getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder do
  P.useBrowser H2h.PuppeteerBrowserResource browserOpts $ \browser -> do
    Z.xInfo browser
    page <- Z.xMapE H2h.UnkPupp $ P.newPage browser
    Z.xMapE H2h.UnkPupp $ P.goto' page "https://google.com"
    pure
      { id: Z.sOrN "tourneyId"
      , name: "Melee Singles"
      , slug: "11111111"
      , state: "COMPLETE"
      , site: H2h.Challonge
      , phaseGroups: Z.arrEmpty @H2h.PhaseGroup
      , entrants: Z.mapEmpty @Z.SorN @H2h.Entrant
      , tournament:
          { id: Z.sOrN ""
          , name: ""
          , images: Z.mapEmpty
          , endAt: 0
          }

      }
  where
  browserOpts = do
    let aOpt = "--user-agent=" <> userAgent
    Z.xSet (Z.l @"args") [ aOpt, "--no-sandbox", "--disable-setuid-sandbox" ]
