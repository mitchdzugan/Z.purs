module Test.Web.SlpDB.Main where

import Web.Z.Prelude
import Web.Z.XDom as XDom
import Z.SSBM.Slp.DB.App (xApp)

xWebApp :: XDom.XDom (XWEB ())
xWebApp = do
  XDom.dwithNewState "" \url setUrl -> do
    xOut { url }
    XDom.d.use1Eff do
      doc <- xDocument
      win <- xWindow
      xClickOff <- xAddEventListener eventType.click doc default \e -> do
        let orTarget = evTarget e
        xOut { orTarget }
        whenJust orTarget \target -> do
          xOut { target }
          orClosest <- xClosest target "a"
          xOut { orClosest }
          whenJust orClosest \closest -> do
            xOut { closest }
            orHref <- xGetAttribute closest "href"
            xOut { orHref }
            whenJust orHref \href -> do
              xPreventDefault e
              xStopPropagation e
              x $ setUrl href
              xOut { href }
      xPushOff <- xAddEventListener eventType.pushState win default \e -> do
        xOut { t: "pushState", e }
      xPopOff <- xAddEventListener eventType.popState win default \e -> do
        xOut { t: "popState", e }
      pure do
        xPopOff
        xPushOff
        xClickOff
    xApp

main :: Effect Unit
main = runXAThenExit do
  let notFoundError = jsError "element not found" "#root"
  domEl <- xGetElementById "root" <#> jOrE notFoundError >>= x Ok
  XDom.xPreactHydrate domEl $ XDom.renderX $ XDom.xDomRunWeb xWebApp
