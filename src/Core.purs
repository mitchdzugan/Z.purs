module Core
  ( JsError
  ) where

import Prelude

import Data.Maybe
import Effect.Exception as Exc

type JsError = Exc.Error
