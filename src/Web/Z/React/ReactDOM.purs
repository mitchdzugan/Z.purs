module Web.Z.React.ReactDOM
  ( renderIn
  ) where

import Z.React.React as React
import Web.Z.Prelude

foreign import js_renderIn :: React.ReactEl -> Element -> Effect (Promise Unit)

renderIn :: forall x. React.ReactEl -> Element -> EA JsError x #> Unit
renderIn r d = xEffectPromise $ js_renderIn r d