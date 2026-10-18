module Z.Z.X.Util where

import Prelude

import Effect (Effect)
import Z.Z.Promise (Promise)

foreign import js_timeout :: Int -> Effect (Promise Unit)

