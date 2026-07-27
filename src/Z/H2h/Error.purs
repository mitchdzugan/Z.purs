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

derive instance genericT :: Generic T _

instance decodeJsonT :: DecodeJson T where
  decodeJson x = genericDecodeJson x

instance encodeJsonT :: EncodeJson T where
  encodeJson x = genericEncodeJson x
