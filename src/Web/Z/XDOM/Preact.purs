module Web.Z.XDOM.Preact
  ( renderIn
  ) where

import Z.XDOM.Preact (ReactEl)
import Web.Z.Prelude

foreign import js_renderIn :: ReactEl -> Element -> Effect (Promise Unit)

renderIn :: forall x. ReactEl -> Element -> EA JsError x #> Unit
renderIn r d = xEffectPromise $ js_renderIn r d