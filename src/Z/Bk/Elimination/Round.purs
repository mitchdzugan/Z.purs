module Z.Bk.Elimination.Round
  ( T(..)
  , isDE
  , isDropRound
  , isGrands
  , isLosers
  , isWinners
  , isReset
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

roundTypeInd :: T -> Int
roundTypeInd (Grands ir) = if ir then 0 - 1 else 0
roundTypeInd (Losers idr depth) = (2 * depth) + (if idr then 0 else 1)
roundTypeInd (Winners _ depth) = depth

roundInd :: T -> Int
roundInd (Grands ir) = if ir then 0 - 1 else 0
roundInd (Losers idr depth) = (3 * depth) + (if idr then 1 else 3)
roundInd (Winners ide depth) = if ide then (3 * depth) + 2 else depth
