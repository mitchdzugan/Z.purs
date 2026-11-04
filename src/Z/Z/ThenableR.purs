module Z.Z.ThenableR
  ( T'useStep
  , Tag'Async
  , Tag'Sync
  , ThenableR
  , async'fromEff
  , async'fromEffPromise
  , async'run
  , async'run_
  , thenableR'ask
  , thenableR'parallel
  , thenableR'runAsync
  , thenableR'sync
  , thenableR'try
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (Error)
import Unsafe.Coerce (unsafeCoerce)
import Z.Z.Core (JsError(..))
import Z.Z.Defaultable (class Generable, GDefault, default)
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

-------------------------------

class Supplies rHas rReq where
  supply :: rHas -> rReq

instance Supplies rHas rHas where
  supply = identity
else instance (Generable rReq GDefault rReq) => Supplies rHas rReq where
  supply _ = default

-------------------------------

thenableR'sync :: forall r a. r -> ThenableR Tag'Sync r a -> a
thenableR'sync = js_thenableR_sync

thenableR'runAsync
  :: forall rt r a
   . r
  -> (a -> Effect Unit)
  -> (JsError -> Effect Unit)
  -> ThenableR rt r a
  -> Effect Unit
thenableR'runAsync = js_thenableR_runA

async'run
  :: forall rt r a
   . Generable r GDefault r
  => (a -> Effect Unit)
  -> (JsError -> Effect Unit)
  -> ThenableR rt r a
  -> Effect Unit
async'run = thenableR'runAsync default

async'run_
  :: forall rt r a. Generable r GDefault r => ThenableR rt r a -> Effect Unit
async'run_ = async'run (const $ pure unit) (const $ pure unit)

thenableR'ask :: forall rt r. ThenableR rt r r
thenableR'ask = js_thenableR_ask

thenableR'parallel
  :: forall rt r a. Array (ThenableR rt r a) -> ThenableR rt r (Array a)
thenableR'parallel = js_thenableR_parallel

thenableR'try
  :: forall rt r a
   . ThenableR rt r a
  -> ThenableR rt r (Either JsError a)
thenableR'try = js_thenableR_try (Left <<< JsError) Right

async'fromEff
  :: forall r a. Effect a -> ThenableR Tag'Async r (Either JsError a)
async'fromEff = thenableR'try <<< js_thenableR_asyncFromEff

async'fromEffPromise
  :: forall r a. EffPromise a -> ThenableR Tag'Async r (Either JsError a)
async'fromEffPromise = thenableR'try <<< js_thenableR_fromEffPromise

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

foreign import js_thenableR_asyncFromEff
  :: forall r a. Effect a -> ThenableR Tag'Async r a

js_thenableR_fromEffPromise
  :: forall r a. EffPromise a -> ThenableR Tag'Async r a
js_thenableR_fromEffPromise = unsafeCoerce

foreign import js_thenableR_parallel
  :: forall rt r a. Array (ThenableR rt r a) -> ThenableR rt r (Array a)

foreign import js_thenableR_try
  :: forall rt r a
   . (Error -> Either JsError a)
  -> (a -> Either JsError a)
  -> ThenableR rt r a
  -> ThenableR rt r (Either JsError a)