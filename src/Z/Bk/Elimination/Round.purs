module Z.Bk.Elimination.Round
  ( T(..)
  ) where

import Prelude

data T = Winners Boolean Int | Losers Boolean Int | Grands Boolean