module Test.Web.ClmStats.Main where

import Z.Prelude

import Web.Z.CLM.Stats.App (runClmStatsApp)

main :: Effect Unit
main = runClmStatsApp