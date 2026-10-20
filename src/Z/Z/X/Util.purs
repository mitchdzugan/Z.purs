module Z.Z.X.Util where

import Prelude

import Effect (Effect)
import Z.Z.Future (Promise)

foreign import js_timeout :: Int -> Effect (Promise Unit)

