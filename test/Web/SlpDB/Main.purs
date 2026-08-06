module Test.Web.SlpDB.Main where

import Web.Z.Prelude

import Web.Z.XDom as XDom
import Z.SSBM.Slp.DB.App (xApp)

seqt :: forall x. X x Unit
seqt = do
  xOut "a"
  _ <- inner
  xOut "c"
  where
  inner = do
    xOut "b"
    pure $ xOut "d"

main :: Effect Unit
main = runXAThenExit do
  let notFoundError = jsError "element not found!" "#root"
  domEl <- xGetElementById "root" <#> jOrE notFoundError >>= x' @"ok"
  XDom.xPreactHydrate domEl $ XDom.renderX $ XDom.xDomRunWeb do
    seqt
    xApp XDom.xProvideHistoryX