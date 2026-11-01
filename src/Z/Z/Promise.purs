module Z.Z.Promise
  ( EffPromise
  , Promise
  ) where

import Prelude

import Effect (Effect)

foreign import data Promise :: Type -> Type

type EffPromise a = Effect (Promise a)