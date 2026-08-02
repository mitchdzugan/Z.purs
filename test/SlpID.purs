module Test.SlpID where

import Node.Z.Prelude
import Node.Z.SSBM.Slp.ID as SlpID

main ∷ Effect Unit
main = runXWithArgvThenExit SlpID.xRun
