module Z.CLM.Stats.Types
  ( ClmActionData
  , ClmAppLoadError(..)
  , ClmBuildMeta(..)
  , ClmBuildOutputs
  , ClmPlayer
  , ClmPlayer'Season
  , ClmPlayer'Season'Event
  , ClmPlayer'Season'H2h
  , ClmPlayer'Season'Set
  , ClmPlayerSeason'Json
  , ClmSeason
  , ClmSeason'Event
  , ClmSeason'Json
  , ClmSeason'Player
  , ClmSeason'Player'Char
  , ClmSeason'Player'Full
  , ClmSeason'Player'G
  , ClmTimeline
  , clm'hasRatingConfidence
  , clm'hasReq'prEvents
  , clmBuildMeta
  ) where

import Z.Prelude

import Node.Z.CLM.Stats.Manager.Action (PureAction)
import Z.H2h.Module as H2h

type ClmSeason'Event =
  { eventName :: String
  , numEntrants :: Int
  , date :: DateTime
  , source :: H2h.EventSource
  , prEligible :: Boolean
  , tournamentName :: String
  , imageUrl :: String
  , eventId :: SorN
  }

type ClmSeason'Player'Char =
  { charId :: Int
  , color :: Int
  , usage :: Number
  }

type ClmPlayer =
  { image :: Maybe String
  , socials :: HashMap H2h.SocialType String
  , gamerTag :: String
  , prefix :: Maybe String
  , realName :: Maybe String
  , pronouns :: Maybe String
  , clmId :: Int
  }

type ClmSeason'Player'G plusPlayer plusEvent =
  { clmId :: Int
  , rating :: Int
  , clmRank :: Maybe Int
  , wins :: Int
  , losses :: Int
  , prEvents :: Int
  , lastEvent :: { id :: SorN, placing :: Int | plusEvent }
  , chars :: Array ClmSeason'Player'Char
  | plusPlayer
  }

type ClmSeason'Player = ClmSeason'Player'G () ()

clm'hasReq'prEvents :: forall r. Record (prEvents :: Int | r) -> Boolean
clm'hasReq'prEvents { prEvents } = prEvents >= 8

clm'hasRatingConfidence :: forall r. Record (prEvents :: Int | r) -> Boolean
clm'hasRatingConfidence { prEvents } = prEvents >= 3

type ClmSeason'Player'Full = ClmSeason'Player'G
  ( hasReq'prEvents :: Boolean
  , hasRatingConfidence :: Boolean
  , player :: ClmPlayer
  , sets :: Int
  , winrate :: Maybe Number
  , sortAndFilteredRank :: Int
  )
  (data :: ClmSeason'Event)

type ClmSeason =
  { seasonId :: Int
  , ranks :: Array ClmSeason'Player
  , events :: HashMap SorN ClmSeason'Event
  , players :: HashMap Int ClmPlayer
  }

type ClmSeason'Json =
  { ranks :: Array ClmSeason'Player
  , events :: Array ClmSeason'Event
  , players :: Array ClmPlayer
  , otherPlayerTags :: Array (Int /\ Array String)
  , otherPlayerSeasons :: Array (Int /\ Array Int)
  , otherSeasons :: Array Int
  }

type ClmPlayer'Season'Set =
  { id :: SorN
  , eventId :: SorN
  , won :: Boolean
  , dq :: Boolean
  , round :: String
  , isOverridden :: Boolean
  , opponentId :: Maybe Int
  , self ::
      { score :: String
      , gamerTag :: String
      , prefix :: Maybe String
      }
  , oppo ::
      { score :: String
      , gamerTag :: String
      , prefix :: Maybe String
      }
  }

type ClmPlayer'Season'Event =
  { eventId :: SorN
  , placingString :: String
  , setIds :: Array SorN
  , numWins :: Int
  , numLosses :: Int
  , didDQ :: Boolean
  }

type ClmPlayer'Season'H2h =
  { opponent :: Either String Int
  , setIds :: Array SorN
  }

type ClmPlayer'Season =
  { sets :: HashMap SorN ClmPlayer'Season'Set
  , events :: Array ClmPlayer'Season'Event
  , h2hs :: Array ClmPlayer'Season'H2h
  }

type ClmPlayerSeason'Json =
  { sets :: Array ClmPlayer'Season'Set
  , events :: Array ClmPlayer'Season'Event
  , h2hs :: Array ClmPlayer'Season'H2h
  }

type ClmTimeline =
  { preDefault :: Array Int
  , defaultSeasonId :: Int
  , postDefault :: Array Int
  }

type ClmActionData = { manual :: Array PureAction, auto :: Array PureAction }

type ClmBuildOutputs =
  { timeline :: ClmTimeline
  , dbHash :: String
  }

newtype ClmBuildMeta = ClmBuildMeta
  { actionData :: ClmActionData
  , outputs :: ClmBuildOutputs
  }

derive instance Newtype ClmBuildMeta _

derive instance Generic ClmBuildMeta _

instance DecodeJson ClmBuildMeta where
  decodeJson x = genericDecodeJson x

instance EncodeJson ClmBuildMeta where
  encodeJson x = genericEncodeJson x

clmBuildMeta :: ClmActionData -> ClmBuildOutputs -> ClmBuildMeta
clmBuildMeta actionData outputs = ClmBuildMeta { actionData, outputs }

data ClmAppLoadError
  = MissingHtmlHead
  | MissingHtmlBody
  | MissingBuildOutputs
  | InvalidBuildOutputs String
  | HydrateError JsError

derive instance Generic ClmAppLoadError _

instance Show ClmAppLoadError where
  show = genericShow