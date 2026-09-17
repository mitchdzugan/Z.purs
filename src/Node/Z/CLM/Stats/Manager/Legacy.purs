module Node.Z.CLM.Stats.Manager.Legacy
  ( CLMStatsLegacyBlob
  , CLMStatsLegacyBlob'SetSummary
  ) where

import Node.Z.Prelude

type CLMStatsLegacyBlob'SetSummary =
  { id :: Maybe String
  , won :: Boolean
  , dq :: Boolean
  , round :: String
  , wonGames :: String
  , lostGames :: String
  , opponentName :: Maybe String
  , winnerName :: Maybe String
  , loserName :: Maybe String
  }

type CLMStatsLegacyBlob =
  { nameDataByPlayerId :: Object { name :: String, ident :: String }
  , nextIdTry :: Int
  , "IDENT_CLM_IDS" :: Object Int
  , timeline ::
      Array
        { seasonId :: Int
        , title :: String
        , timelineInd :: Int
        , season :: String
        }
  , events ::
      Object
        { eventName :: String
        , numEntrants :: Int
        , date :: Int
        , slug :: String
        , prEligible :: Boolean
        , tournamentName :: String
        , imageUrl :: String
        , eventId :: Int
        }
  , players ::
      Object $ Object
        { pid :: String
        , clmId :: Maybe Int
        , events ::
            Array
              { event :: { eventId :: Int }
              , placingString :: String
              , setSummaries :: Array CLMStatsLegacyBlob'SetSummary
              , numWins :: Int
              , numLosses :: Int
              , losses :: Array String
              , "DQ" :: Boolean
              }
        , h2hs ::
            Array
              { opponent :: String
              , rank :: Int
              , sets ::
                  Array
                    { setInfo :: CLMStatsLegacyBlob'SetSummary
                    , tournamentName :: String
                    , date :: String
                    , slug :: String
                    }
              }
        }
  , seasons ::
      Object
        { seasonId :: Int
        , title :: String
        , isAll :: Boolean
        , others :: Object Int
        , events :: Object { eventId :: Int }
        , players ::
            Object
              { playerId :: Int
              , image :: String
              , name :: String
              , realName :: Maybe String
              , id :: Int
              , clmId :: Int
              }
        , ranks ::
            Array
              { rank :: Int
              , winrate :: Maybe Number
              , placing :: Int
              , placingString :: String
              , wins :: Int
              , losses :: Int
              , prEvents :: Int
              , rating :: Number
              , conservativeRating :: Number
              , playerIdent :: String
              , eventId :: Int
              }
        }
  }