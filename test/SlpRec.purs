module Test.SlpRec where

import Node.Z.Prelude
import Node.Z.SSBM.Slp.Rec as SlpRec

main ∷ Effect Unit
main = runXAThenExitWithArgv SlpRec.xRun
