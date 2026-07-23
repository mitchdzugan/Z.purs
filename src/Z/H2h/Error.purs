module Z.H2h.Error
  ( T(..)
  ) where

import Z as Z
import Z.Gql.Module as Gql

data T
  = Gql Gql.Error
  | UnkPupp Z.JsError
  | Puppeteer String String Z.JsError
  | PuppeteerBrowserResource Z.ResourceStage Z.JsError
  | MissingData String
  | EventBuild Z.JsError
  | ParseCached Z.JsError
  | ParseTime Z.ParseError
  | InvalidInstant Int

derive instance genericT :: Z.Generic T _

instance decodeJsonT :: Z.DecodeJson T where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonT :: Z.EncodeJson T where
  encodeJson x = Z.genericEncodeJson x
