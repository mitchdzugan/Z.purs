module Test.Web.SlpDB.Main where

import Web.Z.Prelude

import Web.Z.XDom as XDom
import Z.SSBM.Slp.DB.App (app'mk)

main :: Effect Unit
main = runXAThenExit do
  let notFoundError = jsError "element not found!" "#root"
  domEl <- xGetElementById "root" <#> jOrE notFoundError >>= g @XOk
  XDom.xPreactHydrate domEl $ XDom.exec'xdom do
    XDom.xDomRunWeb $ app'mk XDom.xProvideHistoryX