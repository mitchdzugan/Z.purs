module Z.Z.X.Util where

import Prelude

import Effect (Effect)
import Z.Z.Promise (EffPromise)

foreign import js_timeout :: Int -> EffPromise Unit

