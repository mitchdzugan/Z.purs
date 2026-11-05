module Z.H2h.Error
  ( T'row
  , T(..)
  , emptyEntrant
  , eventBuild
  , gql
  , missingData
  , parseCached
  , parseMatchId
  , parseScore
  , parseTime
  , puppeteer
  , puppeteerBrowserResource
  , unkPupp
  ) where

import Z.Prelude

import Z.Gql.Module as Gql
import Z.Z.Json.Decode (class XDecodeJson)

type T'row =
  ( gql :: Gql.Error
  , unkPupp :: JsError
  , puppeteer :: String /\ String /\ JsError
  , puppeteerBrowserResource :: ResourceStage /\ JsError
  , missingData :: String
  , eventBuild :: JsError
  , parseCached :: JsError
  , parseTime :: ParseError
  , parseScore :: ParseError
  , parseMatchId :: ParseError
  , emptyEntrant :: Int
  )

newtype T = T (Variant T'row)

derive instance Newtype T _

derive newtype instance XDecodeJson T

instance EncodeJson T where
  encodeJson = var'encode

gql :: Gql.Error -> T
gql = var'inj @"gql"

unkPupp :: JsError -> T
unkPupp = var'inj @"unkPupp"

puppeteer :: String -> String -> JsError -> T
puppeteer prop sel err = var'inj @"puppeteer" $ prop /\ sel /\ err

puppeteerBrowserResource :: ResourceStage -> JsError -> T
puppeteerBrowserResource stage err =
  var'inj @"puppeteerBrowserResource" $ stage /\ err

missingData :: String -> T
missingData = var'inj @"missingData"

eventBuild :: JsError -> T
eventBuild = var'inj @"eventBuild"

parseCached :: JsError -> T
parseCached = var'inj @"parseCached"

parseTime :: ParseError -> T
parseTime = var'inj @"parseTime"

parseScore :: ParseError -> T
parseScore = var'inj @"parseScore"

parseMatchId :: ParseError -> T
parseMatchId = var'inj @"parseMatchId"

emptyEntrant :: Int -> T
emptyEntrant = var'inj @"emptyEntrant"