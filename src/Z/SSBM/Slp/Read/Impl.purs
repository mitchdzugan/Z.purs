module Z.SSBM.Slp.Read.Impl
  ( Game
  , Stats
  , game
  , stats
  ) where

import Z as Z

foreign import data Game :: Type
foreign import data Stats :: Type

foreign import js_gameOfBuffer :: Z.Buffer -> Game
foreign import js_stats :: Game -> Stats

game :: Z.Buffer -> Game
game = js_gameOfBuffer

stats :: Game -> Stats
stats = js_stats