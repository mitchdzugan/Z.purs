module Test.Web.ClmStats.Main where

import Web.Z.Prelude

import Web.Z.XDom as XDom
import Z.CLM.Stats.App (app'mk)
import Z.CLM.Stats.Types (ClmSeason'Json)

main :: Effect Unit
main = runXAThenExit do
  res <- x'fetch "/db/seasons/14.json" {}
  season <- x'decodeFetched @ClmSeason'Json res
  x'out season
  let notFoundError = jsError "element not found!" "#root"
  domEl <- xGetElementById "root" <#> jOrE notFoundError >>= e'ok
  XDom.xPreactHydrate domEl $ XDom.exec'xdom do
    XDom.xDomRunWeb $ app'mk XDom.xProvideHistoryX