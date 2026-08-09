module Z.H2h.Error
  ( T(..)
  ) where

import Z.Prelude
import Z.Gql.Module as Gql

data T
  = Gql Gql.Error
  | UnkPupp JsError
  | Puppeteer String String JsError
  | PuppeteerBrowserResource ResourceStage JsError
  | MissingData String
  | EventBuild JsError
  | ParseCached JsError
  | ParseTime ParseError
  | InvalidInstant Int

derive instance Generic T _

instance DecodeJson T where
  decodeJson x = genericDecodeJson x

instance EncodeJson T where
  encodeJson x = genericEncodeJson x
