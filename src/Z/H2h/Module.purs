module Z.H2h.Module
  ( BracketingSite(..)
  , Entrant
  , Error(..)
  , Event
  , EventSource
  , Participant
  , PhaseGroup
  , Player
  , Standing
  , Tournament
  , Warning(..)
  , challongeSource
  , startggSource
  ) where

import Z as Z
import Z.Gql.Module as Gql

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

type Player =
  { id :: Z.StringOrNum
  , gamerTag :: String
  , prefix :: Z.Maybe String
  , pronouns :: Z.Maybe String
  , name :: Z.Maybe String
  , socials :: Z.Map String String
  , images :: Z.Map String String
  }

type Participant =
  { player :: Player
  , prefix :: Z.Maybe String
  , gamerTag :: String
  , playerOrder :: Int
  }

type Standing = { placement :: Int, isFinal :: Boolean }

type Entrant =
  { id :: Z.StringOrNum
  , participants :: Array Participant
  , standing :: Standing
  }

type Tournament =
  { id :: Z.StringOrNum
  , name :: String
  , images :: Z.Map String String
  , endAt :: Int
  }

type Event =
  { id :: Z.StringOrNum
  , site :: BracketingSite
  , name :: String
  , slug :: String
  , state :: String
  , entrants :: Z.Map Z.StringOrNum Entrant
  , phaseGroups :: Array PhaseGroup
  , tournament :: Tournament
  }

type EventSource =
  { site :: BracketingSite
  , slug :: String
  }

startggSource :: String -> EventSource
startggSource slug = { slug, site: Startgg }

challongeSource :: String -> EventSource
challongeSource slug = { slug, site: Challonge }
