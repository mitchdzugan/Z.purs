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
  , H2hSet
  , Slot
  , Standing
  , Tournament
  , Warning
  , challongeSource
  , mkScoreCount
  , mkScoreDQ
  , startggSource
  ) where

import Z.Prelude

import Z.H2h.Error as H2hE
import Z.H2h.Warning as H2hW

type Error = H2hE.T

type Warning = H2hW.T

data BracketingSite = Startgg | Challonge

derive instance Generic BracketingSite _

instance DecodeJson BracketingSite where
  decodeJson x = genericDecodeJson x

instance EncodeJson BracketingSite where
  encodeJson x = genericEncodeJson x

data Score = DQ Boolean | Count Int | NoScore

mkScoreDQ :: Boolean -> Score
mkScoreDQ isDQd = DQ isDQd

mkScoreCount :: Int -> Score
mkScoreCount count = Count count

derive instance Generic Score _

instance DecodeJson Score where
  decodeJson x = genericDecodeJson x

instance EncodeJson Score where
  encodeJson x = genericEncodeJson x

type Slot =
  { entrantId :: Maybe SorN
  , score :: Score
  }

type H2hSet =
  { id :: Int
  , isDQ :: Boolean
  , isBye :: Boolean
  , winner :: Maybe PairKey
  , doesCount :: Boolean
  , roundText :: String
  , slots :: Pair Slot
  , overrideScoreText :: Maybe String
  }

type Phase =
  { id :: SorN
  , name :: String
  , phaseOrder :: Int
  }

type PhaseGroup =
  { id :: SorN
  , displayIdentifier :: String
  , sets :: Map Int H2hSet
  , phase :: Phase
  }

type Player =
  { id :: SorN
  , gamerTag :: String
  , prefix :: Maybe String
  , pronouns :: Maybe String
  , name :: Maybe String
  , socials :: Map String String
  , images :: Map String String
  }

type Participant =
  { player :: Player
  , prefix :: Maybe String
  , gamerTag :: String
  , playerOrder :: Int
  }

type Standing = { placement :: Int, isFinal :: Boolean }

type Entrant =
  { id :: SorN
  , participants :: Array Participant
  , standing :: Standing
  }

type Tournament =
  { id :: SorN
  , name :: String
  , images :: Map String String
  , date :: DateTime
  }

type Event =
  { id :: SorN
  , site :: BracketingSite
  , name :: String
  , slug :: String
  , state :: String
  , entrants :: Map SorN Entrant
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
