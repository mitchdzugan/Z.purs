module Z.Bk.Elimination.Round
  ( T(..)
  , isDE
  , isDropRound
  , isGrands
  , isLosers
  , isReset
  ) where

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

isDropRound :: T -> Boolean
isDropRound (Losers idr _) = idr
isDropRound _ = false

isDE :: T -> Boolean
isDE (Winners ide _) = ide
isDE _ = true