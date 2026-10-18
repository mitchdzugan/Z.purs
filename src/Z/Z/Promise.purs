module Z.Z.Promise
  ( EffPromise
  , Future
  , Promise
  , future'fromEff
  , future'fromEffPromise
  , future'run
  , future'run_
  , future'try
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (Error)
import Z.Z.Core (JsError(..))
import Z.Z.Passable (pass)

foreign import data Promise :: Type -> Type

foreign import data Future :: Type -> Type

type EffPromise a = Effect (Promise a)

instance Functor Future where
  map = js_futureMap

instance Apply Future where
  apply = js_futureApply

instance Applicative Future where
  pure = js_futurePure

instance Bind Future where
  bind = js_futureBind

instance Monad Future

future'try :: forall a. Future a -> Future (Either JsError a)
future'try = js_futureTry (Left <<< JsError) Right

future'fromEff :: forall a. Effect a -> Future a
future'fromEff = js_futureFromEff

future'fromEffPromise :: forall a. EffPromise a -> Future a
future'fromEffPromise = js_futureFromEffPromise

future'run
  :: forall a
   . (a -> Effect Unit)
  -> (JsError -> Effect Unit)
  -> Future a
  -> Effect Unit
future'run = js_futureRun

future'run_ :: forall a. Future a -> Effect Unit
future'run_ = js_futureRun (const pass) (const pass)

foreign import js_futureMap
  :: forall a b. (a -> b) -> (Future a) -> Future b

foreign import js_futureApply
  :: forall a b. Future (a -> b) -> (Future a) -> Future b

foreign import js_futurePure :: forall a. a -> (Future a)

foreign import js_futureBind
  :: forall a b. Future a -> (a -> Future b) -> Future b

foreign import js_futureTry
  :: forall a
   . (Error -> Either JsError a)
  -> (a -> Either JsError a)
  -> Future a
  -> Future (Either JsError a)

foreign import js_futureFromEff :: forall a. Effect a -> Future a

foreign import js_futureRun
  :: forall a
   . (a -> Effect Unit)
  -> (JsError -> Effect Unit)
  -> Future a
  -> Effect Unit

foreign import js_futureFromEffPromise :: forall a. EffPromise a -> Future a