module Test.Web.SlpDB.Main where

import Web.Z.Prelude

import Web.Z.XDom as XDom
import Z.SSBM.Slp.DB.App (xApp)

main :: Effect Unit
main = runXAThenExit do
  let notFoundError = jsError "element not found!" "#root"
  domEl <- xGetElementById "root" <#> jOrE notFoundError >>= g @XOk
  XDom.xPreactHydrate domEl $ XDom.renderX $ XDom.xDomRunWeb do
    xApp XDom.xProvideHistoryX
