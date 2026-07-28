module Test.SlpDB where

import Node.Z.Prelude
import Node.Z.SSBM.Slp.DB as SlpDB

main ∷ Effect Unit
main = xExecAndExitArgv SlpDB.run
