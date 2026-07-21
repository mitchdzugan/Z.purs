module Z.H2h.Module
  ( BracketingSite(..)
  , Entrant
  , Error(..)
  , Event
  , EventSource
  , PhaseGroup
  , Warning(..)
  , challongeSource
  , startggSource
  ) where

import Z.Gql.Module as Gql
import Z as Z

data Warning = GqlW Gql.Warning

derive instance genericWarning :: Z.Generic Warning _

instance decodeJsonWarning :: Z.DecodeJson Warning where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonWarning :: Z.EncodeJson Warning where
  encodeJson x = Z.genericEncodeJson x

data Error
  = GqlE Gql.Error
  | AutoBrowser String String Z.JsError
  | MissingData String
  | EventBuild Z.JsError
  | ParseCached Z.JsError

derive instance genericError :: Z.Generic Error _

instance decodeJsonError :: Z.DecodeJson Error where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonError :: Z.EncodeJson Error where
  encodeJson x = Z.genericEncodeJson x

data BracketingSite = Startgg | Challonge

derive instance genericBracketingSite :: Z.Generic BracketingSite _

instance decodeJsonBracketingSite :: Z.DecodeJson BracketingSite where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonBracketingSite :: Z.EncodeJson BracketingSite where
  encodeJson x = Z.genericEncodeJson x

type PhaseGroup =
  { id :: Z.StringOrNum
  , displayIdentifier :: String
  }

type Entrant =
  { id :: Z.StringOrNum
  }

type Event =
  { id :: Z.StringOrNum
  , site :: BracketingSite
  , tournamentName :: String
  , name :: String
  , slug :: String
  , state :: String
  , entrants :: Z.Map Z.StringOrNum Entrant
  , phaseGroups :: Array PhaseGroup
  , numEntrants :: Int
  }

type EventSource =
  { site :: BracketingSite
  , slug :: String
  }

startggSource :: String -> EventSource
startggSource slug = { slug, site: Startgg }

challongeSource :: String -> EventSource
challongeSource slug = { slug, site: Challonge }
