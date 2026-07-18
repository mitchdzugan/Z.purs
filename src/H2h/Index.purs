module H2h.Index
  ( BracketingSite(..)
  , Error(..)
  , Event
  , EventSource
  ) where

import Gql.Index as Gql
import Z.Z as Z

data Error
  = Fetch Gql.Error
  | AutoBrowser String String Z.JsError
  | MissingData String
  | EventBuild Z.JsError
  | ParseCached Z.JsError

derive instance genericError :: Z.Generic Error _

instance decodeJsonError :: Z.DecodeJson Error where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonError :: Z.EncodeJson Error where
  encodeJson x = Z.genericEncodeJson x

data BracketingSite = StartGG | Challonge

derive instance genericBracketingSite :: Z.Generic BracketingSite _

instance decodeJsonBracketingSite :: Z.DecodeJson BracketingSite where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonBracketingSite :: Z.EncodeJson BracketingSite where
  encodeJson x = Z.genericEncodeJson x

type Event =
  { id :: Z.StringOrNum
  , site :: BracketingSite
  , tournamentName :: String
  , name :: String
  , slug :: String
  , state :: String
  }

type EventSource =
  { site :: BracketingSite
  , slug :: String
  }