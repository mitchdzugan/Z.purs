module H2h.Core where

import Prelude

import Data.Codec.Argonaut.Variant as CAV
import Data.Profunctor (dimap)
import Data.Variant as V
import Gql.Core as Gql
import Z as Z

data Error
  = Fetch Gql.Error
  | AutoBrowser String String Z.JsError
  | MissingData String
  | EventBuild Z.JsError
  | ParseCached Z.JsError

data BracketingSite = StartGG | Challonge

readBracketingSite :: Z.Json -> Z.Maybe BracketingSite
readBracketingSite = Z.caseJsonString Z.Nothing readBracketingSiteStr
  where
  readBracketingSiteStr "StartGG" = Z.Just StartGG
  readBracketingSiteStr "Challonge" = Z.Just Challonge
  readBracketingSiteStr _ = Z.Nothing

writeBracketingSite :: BracketingSite -> Z.Json
writeBracketingSite StartGG = Z.fromString "StartGG"
writeBracketingSite Challonge = Z.fromString "Challonge"

c_bracketingSite :: Z.JsonCodec BracketingSite
c_bracketingSite = Z.c_prismatic "BracketingSite" readBracketingSite
  writeBracketingSite
  Z.c_json

type Event =
  { id :: Z.StringOrNum
  , site :: BracketingSite
  , tournamentName :: String
  , name :: String
  , slug :: String
  , state :: String
  }

c_event :: Z.JsonCodec Event
c_event = Z.c_object "H2h|Event" $ Z.c_record
  { id: Z.c_stringOrNum
  , site: c_bracketingSite
  , tournamentName: Z.c_string
  , name: Z.c_string
  , slug: Z.c_string
  , state: Z.c_string
  }

type EventSource =
  { site :: BracketingSite
  , slug :: String
  }