module Web.Z.CLM.Stats.App
  ( runClmStatsApp
  ) where

import Web.Z.Prelude

import Web.Z.XDom as XDom
import Z.CLM.Stats.App (app'mk)
import Z.CLM.Stats.Types (ClmAppLoadError, ClmBuildOutputs)

runClmStatsApp :: Effect Unit
runClmStatsApp = runXAThenExit @Void @Void do
  _OrError <- e'try @ClmAppLoadError do
    docHead <- xHead >>= e'unwrapAt_ @"missingHtmlHead"
    buildOutputsStr <- xGetAttribute docHead ":clmBuildOutputs" >>=
      e'unwrapAt_ @"missingBuildOutputs"
    buildOutputs <- decode @ClmBuildOutputs buildOutputsStr #
      e'unwrapAt @"invalidBuildOutputs" buildOutputsStr
    docBody <- xBody >>= e'unwrapAt_ @"missingHtmlBody"
    e'liftAt @"hydrateErr" $ XDom.xPreactHydrate docBody $ XDom.exec'xdom'web do
      XDom.xDomRunWeb $ app'mk buildOutputs XDom.xweb'historyProvider
  whenJust (blush _OrError) \loadError -> do
    x'out { loadError: show loadError }