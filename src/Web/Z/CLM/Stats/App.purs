module Web.Z.CLM.Stats.App
  ( runClmStatsApp
  ) where

import Web.Z.Prelude

import Web.Z.XDom as XDom
import Z.CLM.Stats.App (app'mk)
import Z.CLM.Stats.Types (ClmAppLoadError(..), ClmBuildMeta)

runClmStatsApp :: Effect Unit
runClmStatsApp = runXAThenExit @Void @Void do
  _OrError <- e'try do
    docHead <- xHead >>= e'unwrap MissingHtmlHead
    buildMetaStr <- xGetAttribute docHead ":clmBuildMeta" >>=
      e'unwrap MissingBuildMeta
    buildMeta <- e'ok $ constL (InvalidBuildMeta buildMetaStr) $
      decode @ClmBuildMeta buildMetaStr
    docBody <- xBody >>= e'unwrap MissingHtmlBody
    e'map HydrateError $ XDom.xPreactHydrate docBody $ XDom.exec'xdom do
      XDom.xDomRunWeb $ app'mk (g_ @"!.outputs" buildMeta) XDom.xProvideHistoryX
  whenJust (blush _OrError) \loadError -> do
    x'out { loadError }