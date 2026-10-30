module Z.Bk.Elimination.Round
  ( T(..)
  , depth
  , isDE
  , isDropRound
  , isGrands
  , isLosers
  , isReset
  , isWinners
  , roundInd
  , roundTypeInd
  ) where

import Prelude

data T = Winners Boolean Int | Losers Boolean Int | Grands Boolean

isGrands :: T -> Boolean
isGrands (Grands _) = true
isGrands _ = false

isReset :: T -> Boolean
isReset (Grands ir) = ir
isReset _ = false

isLosers :: T -> Boolean
isLosers (Losers _ _) = true
isLosers _ = false

isWinners :: T -> Boolean
isWinners (Winners _ _) = true
isWinners _ = false

isDropRound :: T -> Boolean
isDropRound (Losers idr _) = idr
isDropRound _ = false

isDE :: T -> Boolean
isDE (Winners ide _) = ide
isDE _ = true

depth :: T -> Int
depth (Winners _ d) = d
depth (Losers _ d) = d
depth (Grands true) = -2
depth _ = -1

roundTypeInd :: T -> Int
roundTypeInd (Losers idr d) = (2 * d) + (if idr then 0 else 1)
roundTypeInd (Winners _ d) = d
roundTypeInd r = depth r + 1

roundInd :: T -> Int
roundInd (Losers idr d) = (3 * d) + (if idr then 1 else 3)
roundInd (Winners ide d) = if ide then (3 * d) + 2 else d
roundInd r = depth r + 1
