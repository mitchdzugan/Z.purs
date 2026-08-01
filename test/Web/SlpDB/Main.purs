module Test.Web.SlpDB.Main where

import Web.Z.Prelude
import Web.Z.XDOM as XDOM
import Z.SSBM.Slp.DB.App (app)

main :: Effect Unit
main = xExecAndExit do
  doc <- xDocument
  _ <- xAddEventListener eventType.click doc default \e -> xOut { e }
  let notFoundError = jsError "element not found" "#root"
  domEl <- xGetElementById "root" <#> jOrE notFoundError >>= xOk
  flip XDOM.renderIn domEl $ XDOM.xRender app
