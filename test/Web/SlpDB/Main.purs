module Test.Web.SlpDB.Main where

import Web.Z.Prelude
import Web.Z.XDOM as XDOM
import Z.SSBM.Slp.DB.App (app)

hello :: forall x. x ##> Unit
hello = do
  root <- xGetElementById "root"
  xOut { root }
  pure unit

main :: Effect Unit
main = xExecAndExit do
  hello
  domEl <- xGetElementById "root" <#> jOrE (jsError "element not found" "#root")
    >>= xOk
  flip XDOM.renderIn domEl $ XDOM.xRender app
