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
  , imageType'banner
  , imageType'profile
  , mkScoreCount
  , mkScoreDQ
  , mkScoreWL
  , noScore
  , site'challonge
  , site'startgg
  , socialType'discord
  , socialType'luckyStats
  , socialType'twitch
  , socialType'twitter
  , socialType'xbox
  , startggSource
  ) where

import Z.Prelude

import Z.H2h.Error as H2hE
import Z.H2h.Warning as H2hW
import Z.Z.Json.Decode (class XDecodeJson)
import Z.Z.Variant (var'encode)

type Error = H2hE.T

type Warning = H2hW.T

newtype BracketingSite = BracketingSite
  (Variant (startgg :: Unit, challonge :: Unit))

derive instance Newtype BracketingSite _

derive newtype instance Eq BracketingSite

derive newtype instance XDecodeJson BracketingSite

instance EncodeJson BracketingSite where
  encodeJson = var'encode

newtype Score = Score
  ( Variant
      ( count :: Int
      , binaryWL :: { isSetWinner :: Boolean, setHasDQ :: Boolean }
      , noScore :: Unit
      )
  )

mkScoreWL :: Boolean -> Score
mkScoreWL isSetWinner =
  var'inj @"binaryWL" { isSetWinner, setHasDQ: false }

mkScoreDQ :: Boolean -> Score
mkScoreDQ isSetWinner =
  var'inj @"binaryWL" { isSetWinner, setHasDQ: true }

mkScoreCount :: Int -> Score
mkScoreCount count = var'inj @"count" count

noScore :: Score
noScore = var'inj @"noScore" unit

derive instance Newtype Score _

derive newtype instance XDecodeJson Score

instance EncodeJson Score where
  encodeJson = var'encode

instance Show Score where
  show = var'default ""
    # var'on @"count" show
    # var'on @"binaryWL" \props -> case props of
        { isSetWinner: true, setHasDQ: false } -> "W"
        { isSetWinner: false, setHasDQ: false } -> "L"
        { isSetWinner: false, setHasDQ: true } -> "DQ"
        _ -> ""

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

newtype SocialType = SocialType
  ( Variant
      ( discord :: Unit
      , twitter :: Unit
      , twitch :: Unit
      , xbox :: Unit
      , luckyStats :: Unit
      )
  )

derive instance Newtype SocialType _

derive newtype instance XDecodeJson SocialType

instance Identable SocialType where
  ident'get = id'of <<< var'key

instance EncodeJson SocialType where
  encodeJson = var'encode

socialType'discord :: SocialType
socialType'discord = var'inj @"discord" unit

socialType'twitter :: SocialType
socialType'twitter = var'inj @"twitter" unit

socialType'twitch :: SocialType
socialType'twitch = var'inj @"twitch" unit

socialType'xbox :: SocialType
socialType'xbox = var'inj @"xbox" unit

socialType'luckyStats :: SocialType
socialType'luckyStats = var'inj @"luckyStats" unit

newtype ImageType = ImageType
  (Variant (profile :: Unit, banner :: Unit))

derive instance Newtype ImageType _

derive newtype instance XDecodeJson ImageType

instance Identable ImageType where
  ident'get = id'of <<< var'key

instance EncodeJson ImageType where
  encodeJson = var'encode

imageType'banner :: ImageType
imageType'banner = var'inj @"banner" unit

imageType'profile :: ImageType
imageType'profile = var'inj @"profile" unit

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
startggSource slug = { slug, site: site'startgg }

challongeSource :: String -> EventSource
challongeSource slug = { slug, site: site'challonge }

site'challonge :: BracketingSite
site'challonge = var'inj @"challonge" unit

site'startgg :: BracketingSite
site'startgg = var'inj @"startgg" unit