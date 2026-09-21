module Z.Z.X.Util where

import Prelude

import Control.Promise (Promise)
import Effect (Effect)

foreign import js_timeout :: Int -> Effect (Promise Unit)

