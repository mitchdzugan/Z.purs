module Test.Scratch where

import Node.Z.Prelude

import Foreign.Object as Obj
import Z.SSBM.Slp.Read.Impl as SlpRead

testCachePath :: String
testCachePath = "/home/dz/Repo/PS-WS/.cache-path"

-- TODO check if we can always resolve:
-- TODO   - setSummaries[*].id
-- TODO   - setSummaries[*].opponentName
-- TODO   - setSummaries[*].winnerName
-- TODO   - setSummaries[*].loserName
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

-- TODO encode all the `Obj.Object`s as Dicts (..?)
type CLMStatsLegacyBlob =
  { nameDataByPlayerId :: Obj.Object { name :: String, ident :: String }
  , nextIdTry :: Int
  , "IDENT_CLM_IDS" :: Obj.Object Int
  , "RESERVED" :: Obj.Object Int
  , timeline ::
      Array
        { periodId :: Int
        , title :: String
        , timelineInd :: Int
        , season :: String
        }
  , events ::
      Obj.Object
        { name :: Maybe String
        , eventName :: Maybe String
        , numEntrants :: Int
        , date :: Int
        , slug :: String
        , prEligible :: Boolean
        , tournamentName :: String
        , imageUrl :: String
        , eventId :: Int
        }
  , players ::
      Obj.Object $ Obj.Object
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
  , periods ::
      Obj.Object
        { periodId :: Int
        , title :: String
        , isAll :: Boolean
        , others :: Obj.Object Int
        , events :: Obj.Object { eventId :: Int }
        , players ::
            Obj.Object
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
              , winrate :: Maybe Int
              , placing :: Int
              , placingString :: String
              , wins :: Int
              , losses :: Int
              , prEvents :: Int
              , rating :: Int
              , conservativeRating :: Number
              , playerIdent :: String
              , eventId :: Int
              }
        }
  }

main :: Effect Unit
main = runXAThenExit @Void @Void do
  -- b <- xReadFile "/home/dz/Slippi/Game_20260709T183630.slp"
  -- parsed <- g @XMapE un' $ SlpRead.xParse b
  -- xOut $ key parsed
  res <- g @XTry $ xDecodeTextFile @CLMStatsLegacyBlob
    "/VOID/proj/clm-stats.manager/FULL_LEGACY.json"
  case res of
    Left e -> xOut $ encode e
    Right r -> xOut r