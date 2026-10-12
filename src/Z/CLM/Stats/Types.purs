module Z.CLM.Stats.Types
  ( ClmPlayer'Season
  , ClmPlayer'Season'Event
  , ClmPlayer'Season'H2h
  , ClmPlayer'Season'Set
  , ClmSeason
  , ClmSeason'Event
  , ClmSeason'Player
  , ClmSeason'Player'Char
  ) where

import Z.Prelude

import Z.H2h.Module as H2h

type ClmSeason'Event =
  { eventName :: String
  , numEntrants :: Int
  , date :: DateTime
  , slug :: String
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

type ClmSeason'Player =
  { clmId :: Int
  , rating :: Maybe Int
  , clmRank :: Maybe Int
  , wins :: Int
  , losses :: Int
  , prEvents :: Int
  , lastEvent :: { id :: SorN, placing :: Int }
  , image :: Maybe String
  , socials :: HashMap H2h.SocialType String
  , gamerTag :: String
  , prefix :: Maybe String
  , realName :: Maybe String
  , pronouns :: Maybe String
  , otherTags :: HashSet String
  , otherSeasons :: HashSet Int
  , chars :: Array ClmSeason'Player'Char
  }

type ClmSeason =
  { seasonId :: Int
  , title :: String
  , isAll :: Boolean
  , ranks :: Array ClmSeason'Player
  , events :: HashMap SorN ClmSeason'Event
  }

type ClmPlayer'Season'Set =
  { id :: SorN
  , eventId :: SorN
  , won :: Boolean
  , dq :: Boolean
  , round :: String
  , selfScore :: String
  , oppoScore :: String
  , opponentName :: Maybe String
  , winnerName :: Maybe String
  , loserName :: Maybe String
  , isOverridden :: Boolean
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