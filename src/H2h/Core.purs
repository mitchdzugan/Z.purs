module H2h.Core
  ( BracketingSite(..)
  , Error
  , Event
  , EventSource
  ) where

import Prelude

import Gql.Core as Gql
import Z as Z

data Error
  = Fetch Gql.Error
  | AutoBrowser String String Z.JsError
  | MissingData String
  | EventBuild Z.JsError
  | ParseCached Z.JsError

data BracketingSite = StartGG | Challonge

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