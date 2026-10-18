module Z.Z.Promise
  ( EffPromise
  , Promise
  , promise'fromEff
  , promise'run
  , promise'try
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (Error)
import Z.Z.Core (JsError(..))

foreign import data Promise :: Type -> Type

type EffPromise a = Effect (Promise a)

instance Functor Promise where
  map = js_promiseMap

instance Apply Promise where
  apply = js_promiseApply

instance Applicative Promise where
  pure = js_promisePure

instance Bind Promise where
  bind = js_promiseBind

instance Monad Promise

promise'try :: forall a. Promise a -> Promise (Either JsError a)
promise'try = js_promiseTry (Left <<< JsError) Right

promise'fromEff :: forall a. Effect a -> Promise a
promise'fromEff = js_promiseFromEff

promise'run
  :: forall a
   . (a -> Effect Unit)
  -> (JsError -> Effect Unit)
  -> Promise a
  -> Effect Unit
promise'run onDone onError promise = do
  pure unit

foreign import js_promiseMap :: forall a b. (a -> b) -> (Promise a) -> Promise b

foreign import js_promiseApply
  :: forall a b. Promise (a -> b) -> (Promise a) -> Promise b

foreign import js_promisePure :: forall a. a -> (Promise a)

foreign import js_promiseBind
  :: forall a b. Promise a -> (a -> Promise b) -> Promise b

foreign import js_promiseTry
  :: forall a
   . (Error -> Either JsError a)
  -> (a -> Either JsError a)
  -> Promise a
  -> Promise (Either JsError a)

foreign import js_promiseFromEff :: forall a. Effect a -> Promise a