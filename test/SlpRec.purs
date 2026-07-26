module Test.SlpRec where

import Prelude
import Z as Z
import Z.SSBM.Slp.Rec.Node.Impl as SlpRec
import Z.Sys.Node.Module as Sys

main ∷ Z.Effect Unit
main = Sys.xExecAndExitArgv SlpRec.run
