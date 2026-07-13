module Z
  ( module Maybe
  , module Lens
  , module LensRecord
  , module Proxy
  , module X
  , module JSON
  , module Core
  , module Aff
  , module Promise
  , module Effect
  , module EffectClass
  , module Record
  , promiseToAff
  ) where

import Core (JsError) as Core
import Data.Lens (Lens, Lens') as Lens
import Data.Lens.Record (prop) as LensRecord
import Data.Maybe (Maybe(..)) as Maybe
import Effect.Aff (Aff) as Aff
import JSON (JSON, null) as JSON
import Type.Proxy (Proxy(..)) as Proxy
import X (pass, tryAff, result) as X
import Control.Promise (Promise) as Promise
import Control.Promise (toAff)
import Effect (Effect) as Effect
import Effect.Class (liftEffect) as EffectClass
import Record (merge, get, set, modify) as Record

promiseToAff :: forall a. Promise.Promise a -> Aff.Aff a
promiseToAff = toAff