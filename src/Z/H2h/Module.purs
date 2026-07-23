module Z.H2h.Module
  ( BracketingSite(..)
  , Entrant
  , Error
  , Event
  , EventSource
  , Participant
  , Phase
  , PhaseGroup
  , Player
  , Score(..)
  , Set
  , Slot
  , Standing
  , Tournament
  , Warning
  , challongeSource
  , mkScoreCount
  , mkScoreDQ
  , startggSource
  ) where

import Z as Z
import Z.H2h.Error as H2hE
import Z.H2h.Warning as H2hW

type Error = H2hE.T

type Warning = H2hW.T

data BracketingSite = Startgg | Challonge

derive instance genericBracketingSite :: Z.Generic BracketingSite _

instance decodeJsonBracketingSite :: Z.DecodeJson BracketingSite where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonBracketingSite :: Z.EncodeJson BracketingSite where
  encodeJson x = Z.genericEncodeJson x

data Score = DQ Boolean | Count Int | NoScore

mkScoreDQ :: Boolean -> Score
mkScoreDQ isDQd = DQ isDQd

mkScoreCount :: Int -> Score
mkScoreCount count = Count count

derive instance genericScore :: Z.Generic Score _

instance decodeJsonScore :: Z.DecodeJson Score where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonScore :: Z.EncodeJson Score where
  encodeJson x = Z.genericEncodeJson x

type Slot =
  { entrantId :: Z.Maybe Z.SorN
  , score :: Score
  }

type Set =
  { id :: Z.SorN
  , fullRoundText :: String
  , isDQ :: Boolean
  , isBye :: Boolean
  , displayScore :: Z.Maybe String
  , winnerId :: Z.Maybe Z.SorN
  , doesCount :: Boolean
  , isLosers :: Boolean
  , isDropRound :: Boolean
  , isGrands :: Boolean
  , depth :: Int
  , slots :: Slot Z./\ Slot
  }

type Phase =
  { id :: Z.SorN
  , name :: String
  , phaseOrder :: Int
  }

type PhaseGroup =
  { id :: Z.SorN
  , displayIdentifier :: String
  , sets :: Z.Map Z.SorN Set
  , phase :: Phase
  }

type Player =
  { id :: Z.SorN
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
  { id :: Z.SorN
  , participants :: Array Participant
  , standing :: Standing
  }

type Tournament =
  { id :: Z.SorN
  , name :: String
  , images :: Z.Map String String
  , date :: Z.DateTime
  }

type Event =
  { id :: Z.SorN
  , site :: BracketingSite
  , name :: String
  , slug :: String
  , state :: String
  , entrants :: Z.Map Z.SorN Entrant
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
