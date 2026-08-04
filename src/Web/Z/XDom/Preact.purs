module Web.Z.XDom.Preact
  ( xDomRunWeb
  , xPreactHydrate
  ) where

import Z.XDom.Prelude as XD
import Web.Z.Web.DOM as DOM
import Web.Z.Prelude

foreign import js_renderIn :: XD.ReactEl -> Element -> Effect (Promise Unit)

xPreactHydrate :: forall x. Element -> XD.ReactEl -> EA JsError x #> Unit
xPreactHydrate d r = x RunEffPromise $ js_renderIn r d

xDomRunWeb :: forall x. XD.RDom (DOM.XWEB x) -> XD.RDom x
xDomRunWeb m = do
  r <- xAt @XD.XSelf_ Ask
  let runEls = \mm -> r.runEls $ DOM.runXWeb mm
  let runUnit = \mm -> r.runUnit $ DOM.runXWeb mm
  let
    runDisposable = \mm -> r.runDisposable do
      mmm <- DOM.runXWeb mm
      pure $ DOM.runXWeb mmm
  let ir = { runEls, runUnit, runDisposable }
  XD.xRawFragment $ runEls $ x ExecW $ xAt @XD.XSelf_ RunR ir m