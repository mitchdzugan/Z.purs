module Test.CLMStatsManager where

import Node.Z.Prelude

import Node.Z.CLM.Stats.Manager.CLI as CLMStatsManagerCli

main ∷ Effect Unit
main = runXAThenExitWithArgv CLMStatsManagerCli.xRun
