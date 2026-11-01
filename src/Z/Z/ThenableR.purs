module Z.Z.ThenableR
  ( T'useStep
  , Tag'Async
  , Tag'Sync
  , ThenableR
  , async'run
  , thenableR'ask
  , thenableR'async
  , thenableR'async'fromEff
  , thenableR'async'try
  , thenableR'fromEffPromise
  , thenableR'parallel
  , thenableR'runAsync
  , thenableR'sync
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (Error)
import Z.Z.Core (JsError(..))
import Z.Z.Promise (EffPromise)

--------------------------------

foreign import data ThenableR :: Type -> Type -> Type -> Type

foreign import data Tag'Async :: Type

foreign import data Tag'Sync :: Type

-------------------------------

instance Functor (ThenableR rt r) where
  map = js_thenableR_map

instance Apply (ThenableR rt r) where
  apply = js_thenableR_apply

instance Applicative (ThenableR rt r) where
  pure = js_thenableR_pure

instance Bind (ThenableR rt r) where
  bind = js_thenableR_bind

instance Monad (ThenableR rt r)

instance MonadRec (ThenableR rt r) where
  tailRecM = js_thenableR_tailRecM useStep

thenableR'sync :: forall r a. r -> ThenableR Tag'Sync r a -> a
thenableR'sync = js_thenableR_sync

thenableR'async
  :: forall r rOut a. r -> ThenableR Tag'Async r a -> ThenableR Tag'Async rOut a
thenableR'async = js_thenableR_async

thenableR'runAsync
  :: forall rt r a
   . r
  -> (a -> Effect Unit)
  -> (JsError -> Effect Unit)
  -> ThenableR rt r a
  -> Effect Unit
thenableR'runAsync = js_thenableR_runA

async'run
  :: forall rt a
   . (a -> Effect Unit)
  -> (JsError -> Effect Unit)
  -> ThenableR rt Unit a
  -> Effect Unit
async'run = thenableR'runAsync unit

thenableR'ask :: forall rt r. ThenableR rt r r
thenableR'ask = js_thenableR_ask

thenableR'parallel
  :: forall rt r a. Array (ThenableR rt r a) -> ThenableR rt r (Array a)
thenableR'parallel = js_thenableR_parallel

thenableR'async'fromEff :: forall r a. Effect a -> ThenableR Tag'Async r a
thenableR'async'fromEff = js_thenableR_asyncFromEff

thenableR'async'try
  :: forall r a
   . ThenableR Tag'Async r a
  -> ThenableR Tag'Async r (Either JsError a)
thenableR'async'try = js_thenableR_try (Left <<< JsError) Right

thenableR'fromEffPromise :: forall r a. EffPromise a -> ThenableR Tag'Async r a
thenableR'fromEffPromise = js_thenableR_fromEffPromise

-------------------------------

foreign import js_thenableR_map
  :: forall rt r a b. (a -> b) -> (ThenableR rt r a) -> ThenableR rt r b

foreign import js_thenableR_apply
  :: forall rt r a b
   . ThenableR rt r (a -> b)
  -> (ThenableR rt r a)
  -> ThenableR rt r b

foreign import js_thenableR_pure :: forall rt r a. a -> ThenableR rt r a

foreign import js_thenableR_ask :: forall rt r. ThenableR rt r r

foreign import js_thenableR_bind
  :: forall rt r a b
   . ThenableR rt r a
  -> (a -> ThenableR rt r b)
  -> ThenableR rt r b

foreign import js_thenableR_tailRecM
  :: forall rt r a b
   . T'useStep a b
  -> (a -> ThenableR rt r (Step a b))
  -> a
  -> ThenableR rt r b

type T'useStep a b =
  (a -> Effect Unit) -> (b -> Effect Unit) -> Step a b -> Effect Unit

useStep :: forall a b. T'useStep a b
useStep useLoop _ (Loop a) = useLoop a
useStep _ useDone (Done b) = useDone b

foreign import js_thenableR_sync :: forall r a. r -> ThenableR Tag'Sync r a -> a

foreign import js_thenableR_runA
  :: forall rt r a
   . r
  -> (a -> Effect Unit)
  -> (JsError -> Effect Unit)
  -> ThenableR rt r a
  -> Effect Unit

foreign import js_thenableR_async
  :: forall rt r rOut a. r -> ThenableR rt r a -> ThenableR Tag'Async rOut a

foreign import js_thenableR_asyncFromEff
  :: forall r a. Effect a -> ThenableR Tag'Async r a

foreign import js_thenableR_fromEffPromise
  :: forall r a. EffPromise a -> ThenableR Tag'Async r a

foreign import js_thenableR_parallel
  :: forall rt r a. Array (ThenableR rt r a) -> ThenableR rt r (Array a)

foreign import js_thenableR_try
  :: forall r a
   . (Error -> Either JsError a)
  -> (a -> Either JsError a)
  -> ThenableR Tag'Async r a
  -> ThenableR Tag'Async r (Either JsError a)