module Z.H2h.Module
  ( BracketingSite(..)
  , Entrant
  , Error
  , Event
  , EventSource
  , H2hGameSelection
  , H2hGameSelections
  , H2hSet
  , ImageType(..)
  , Participant
  , Phase
  , PhaseGroup
  , Player
  , Score(..)
  , Slot
  , SocialType(..)
  , Standing
  , Tournament
  , Warning
  , challongeSource
  , mkScoreCount
  , mkScoreDQ
  , mkScoreWL
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

data Score
  = Count Int
  | WLBinary { isSetWinner :: Boolean, setHasDQ :: Boolean }
  | NoScore

mkScoreWL :: Boolean -> Score
mkScoreWL isSetWinner = WLBinary { isSetWinner, setHasDQ: false }

mkScoreDQ :: Boolean -> Score
mkScoreDQ isSetWinner = WLBinary { isSetWinner, setHasDQ: true }

mkScoreCount :: Int -> Score
mkScoreCount count = Count count

derive instance Generic Score _

instance DecodeJson Score where
  decodeJson x = genericDecodeJson x

instance EncodeJson Score where
  encodeJson x = genericEncodeJson x

instance Show Score where
  show (Count n) = show n
  show (WLBinary { isSetWinner: true, setHasDQ: false }) = "W"
  show (WLBinary { isSetWinner: false, setHasDQ: false }) = "L"
  show (WLBinary { isSetWinner: false, setHasDQ: true }) = "DQ"
  show _ = ""

type Slot =
  { entrantId :: Maybe SorN
  , score :: Score
  }

type H2hGameSelection =
  { orderNum :: Maybe Int
  , selectionValue :: Int
  , character :: { id :: Int, name :: String }
  }

type H2hGameSelections = HashMap PairKey H2hGameSelection

type H2hSet =
  { id :: SorN
  , isDQ :: Boolean
  , isBye :: Boolean
  , winner :: Maybe PairKey
  , doesCount :: Boolean
  , roundText :: String
  , slots :: Pair Slot
  , overrideScoreText :: Maybe String
  , eventOrder :: Int
  , games ::
      Array
        { winnerId :: Maybe Int
        , orderNum :: Maybe Int
        , entrant1Score :: Maybe Int
        , entrant2Score :: Maybe Int
        , selections :: H2hGameSelections
        }
  }

type Phase =
  { id :: SorN
  , name :: String
  , phaseOrder :: Int
  }

type PhaseGroup =
  { id :: SorN
  , displayIdentifier :: String
  , sets :: HashMap SorN H2hSet
  , phase :: Phase
  }

data SocialType = Discord | Twitter | Twitch | XBox | LuckyStats

derive instance Generic SocialType _

instance Identable SocialType where
  ident'get = id'of <<< genericShow

instance EncodeJson SocialType where
  encodeJson = genericEncodeJson

instance DecodeJson SocialType where
  decodeJson = genericDecodeJson

data ImageType = Profile | Banner

derive instance Generic ImageType _

instance Identable ImageType where
  ident'get = id'of <<< genericShow

instance EncodeJson ImageType where
  encodeJson = genericEncodeJson

instance DecodeJson ImageType where
  decodeJson = genericDecodeJson

type Player =
  { id :: SorN
  , gamerTag :: String
  , prefix :: Maybe String
  , pronouns :: Maybe String
  , name :: Maybe String
  , socials :: HashMap SocialType String
  , images :: HashMap ImageType String
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
  , participant :: Participant
  , standing :: Standing
  }

type Tournament =
  { id :: SorN
  , name :: String
  , images :: HashMap ImageType String
  , date :: DateTime
  }

type Event =
  { id :: SorN
  , site :: BracketingSite
  , name :: String
  , slug :: String
  , state :: String
  , entrants :: HashMap SorN Entrant
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
