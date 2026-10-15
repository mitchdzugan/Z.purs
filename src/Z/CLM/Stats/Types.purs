module Z.CLM.Stats.Types
  ( ClmActionData
  , ClmAppLoadError(..)
  , ClmBuildMeta
  , ClmBuildOutputs
  , ClmPlayer
  , ClmPlayer'Season
  , ClmPlayer'Season'Event
  , ClmPlayer'Season'H2h
  , ClmPlayer'Season'Json
  , ClmPlayer'Season'Set
  , ClmSeason
  , ClmSeason'Event
  , ClmSeason'Json
  , ClmSeason'Player
  , ClmSeason'Player'Char
  , ClmTimeline
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

type ClmSeason'Player =
  { clmId :: Int
  , rating :: Maybe Int
  , clmRank :: Maybe Int
  , wins :: Int
  , losses :: Int
  , prEvents :: Int
  , lastEvent :: { id :: SorN, placing :: Int }
  , chars :: Array ClmSeason'Player'Char
  }

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

type ClmPlayer'Season'Json =
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

type ClmBuildMeta =
  { actionData :: ClmActionData
  , outputs :: ClmBuildOutputs
  }

data ClmAppLoadError
  = MissingHtmlHead
  | MissingHtmlBody
  | MissingBuildMeta
  | InvalidBuildMeta String
  | HydrateError JsError
