module Web.Z.CLM.Stats.App
  ( runClmStatsApp
  ) where

import Web.Z.Prelude

import Web.Z.XDom as XDom
import Z.CLM.Stats.App (app'mk)
import Z.CLM.Stats.Types (ClmAppLoadError(..), ClmBuildOutputs)

runClmStatsApp :: Effect Unit
runClmStatsApp = runXAThenExit @Void @Void do
  let
    testUrl = url'pathSegments $ url'mk do
      s'set'b @"path" [ "sdf", "xD" ]
      s'set'b @"query" $ hm'fromFoldable [ "lol" /\ [ "wut" ] ]
      s'setPure'b @"hash" $ "123"
  x'out { testUrl }
  _OrError <- e'try do
    docHead <- xHead >>= e'unwrap MissingHtmlHead
    buildOutputsStr <- xGetAttribute docHead ":clmBuildOutputs" >>=
      e'unwrap MissingBuildOutputs
    buildOutputs <- e'ok $ constL (InvalidBuildOutputs buildOutputsStr) $
      decode @ClmBuildOutputs buildOutputsStr
    docBody <- xBody >>= e'unwrap MissingHtmlBody
    e'map HydrateError $ XDom.xPreactHydrate docBody $ XDom.exec'xdom do
      XDom.xDomRunWeb $ app'mk buildOutputs XDom.xProvideHistoryX
  whenJust (blush _OrError) \loadError -> do
    x'out { loadError: show loadError }