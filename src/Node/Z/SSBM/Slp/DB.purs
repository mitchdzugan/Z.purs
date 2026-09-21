module Node.Z.SSBM.Slp.DB where

import Node.Z.Prelude


xRun :: forall x. Array String -> EA' JsError x @@> Unit
xRun args = do
  x'out args
