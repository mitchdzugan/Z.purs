module Test.Web.ClmStats.Main where

import Web.Z.Prelude

import Web.Z.XDom as XDom
import Z.CLM.Stats.App (app'mk)
import Z.CLM.Stats.Types (ClmSeason'Json, ClmTimeline)

main :: Effect Unit
main = runXAThenExit do
  res <- x'fetch "/db/seasons/14.json" {}
  season <- x'decodeFetched @ClmSeason'Json res <#> hush >>=
    e'unwrap (jsError "error decoding season" "")
  x'out season
  docHead <- xHead >>= e'unwrap (jsError "htmlDoc.head not found!" "")
  timeline <- do
    timelineStr <- xGetAttribute docHead ":tl" >>=
      e'unwrap (jsError "require head atrribute missing" ":tl")
    e'ok $ constL (jsError "require head atrribute missing" ":tl") $
      decode @ClmTimeline timelineStr
  x'out { timeline }
  docBody <- xBody >>= e'unwrap (jsError "htmlDoc.body not found!" "")
  XDom.xPreactHydrate docBody $ XDom.exec'xdom do
    XDom.xDomRunWeb $ app'mk XDom.xProvideHistoryX