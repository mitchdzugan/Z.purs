module Z.SSBM.Slp.Read.Impl
  ( Game
  , Stats
  , game
  , stats
  ) where

import Z.Prelude

foreign import data Game :: Type
foreign import data Stats :: Type

foreign import js_gameOfBuffer :: Buffer -> Game
foreign import js_stats :: Game -> Stats

type SlpMatch =
  { sessionId :: String
  , gameNumber :: Int
  , tiebreakerNumber :: Int
  }

type SlpStart =
  { isTeams :: Boolean
  , stageId :: Int
  , timer :: Int
  , randomSeed :: Int
  , isPAL :: Maybe Boolean
  , isFrozenPS :: Maybe Boolean
  , matchInfo :: Maybe SlpMatch
  }

game :: Buffer -> Game
game = js_gameOfBuffer

stats :: Game -> Stats
stats = js_stats