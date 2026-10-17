module Node.Z.CLM.Stats.Manager.CLI
  ( xRun
  ) where

import Node.Z.Prelude

import Node.Z.CLM.Stats.Manager.Build (buildClm)

xRun :: forall x. Array String -> EA' JsError x @@> Unit
xRun args = do
  x'info args
  res <- buildClm []
  x'info $ case res.v of
    Left e -> encode e
    Right _ -> "data intake: success"
