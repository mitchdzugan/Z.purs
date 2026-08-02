module Test.Web.SlpDB.Main where

import Web.Z.Prelude
import Web.Z.XDOM as XDOM
import Z.SSBM.Slp.DB.App (xApp)

main :: Effect Unit
main = runXAThenExit do
  doc <- xDocument
  _ <- xAddEventListener eventType.click doc default \e -> xOut { e }
  let notFoundError = jsError "element not found" "#root"
  domEl <- xGetElementById "root" <#> jOrE notFoundError >>= x Ok
  flip XDOM.renderIn domEl $ XDOM.renderX xApp
