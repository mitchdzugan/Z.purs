module Z.Z.Run
  ( Ctx
  , Ctx'Async
  , Ctx'Except
  , Ctx'x_async
  , Future
  , Run'
  , RunA
  , RunS
  , ctxA'adapt
  , ctxA'fromEff
  , ctxA'fromEffPromise
  , ctxA'lift
  , ctxA'run
  , ctxE'failAt
  , ctxE'okAt
  , ctxE'runAt
  , ctxE'throwAt
  , ctxR'askAt
  , ctxR'runAt
  , ctxS'run
  ) where

import Prelude

import Data.Either (Either)
import Data.Symbol (reflectSymbol)
import Effect (Effect)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Z.Z.Core (class ConsSymbol, JsError, eOr'_, mapL)
import Z.Z.Promise (EffPromise)
import Z.Z.ThenableR
  ( Tag'Async
  , Tag'Sync
  , ThenableR
  , async'fromEff
  , async'fromEffPromise
  , thenableR'ask
  , thenableR'sync
  , thenableR'try
  )

-----------------------------------------------------------

foreign import data Ctx :: Row Type -> Type

foreign import data Ctx'Except :: Type -> Type

foreign import data Ctx'Async :: Type

type Ctx'x_async x = (_'x_async :: Ctx'Async | x)

type Run' rt x = ThenableR rt (Ctx x)

type RunS x = Run' Tag'Sync x

type RunA x = Run' Tag'Async x

type Future = ThenableR Tag'Async Unit

ctxR'askAt :: forall @p rt v x' x. ConsSymbol p v x' x => Run' rt x v
ctxR'askAt = thenableR'ask <#> js_ctx_at (reflectSymbol (Proxy @p))

ctxR'runAt
  :: forall @p rt v x' x a
   . ConsSymbol p v x' x
  => v
  -> Run' rt x a
  -> Run' rt x' a
ctxR'runAt = js_ctx_runReaderAt (reflectSymbol (Proxy @p))

ctxE'runAt
  :: forall @p @e rt x' x a
   . ConsSymbol p (Ctx'Except e) x' x
  => Run' rt x a
  -> Run' rt x' (Either e a)
ctxE'runAt m = do
  let symK = reflectSymbol (Proxy @p)
  js_ctx_runExceptAt symK do
    r <- thenableR'ask
    mapL (js_ctx_unwrapErrorAt symK r) <$> thenableR'try m

ctxE'throwAt
  :: forall @p e rt x' x a
   . ConsSymbol p (Ctx'Except e) x' x
  => e
  -> Run' rt x a
ctxE'throwAt e = thenableR'ask <#> js_ctx_throwAt (reflectSymbol (Proxy @p)) e

ctxE'failAt
  :: forall @p e rt x' x
   . ConsSymbol p (Ctx'Except e) x' x
  => e
  -> Run' rt x Unit
ctxE'failAt = ctxE'throwAt @p

ctxE'okAt
  :: forall @p e rt x' x a
   . ConsSymbol p (Ctx'Except e) x' x
  => Either e a
  -> Run' rt x a
ctxE'okAt = eOr'_ (ctxE'throwAt @p) pure

ctxA'fromEff :: forall x a. Effect a -> RunS (Ctx'x_async x) (Either JsError a)
ctxA'fromEff = ctx_asyncX_to_syncAX <<< async'fromEff

ctxA'fromEffPromise
  :: forall x a. EffPromise a -> RunS (Ctx'x_async x) (Either JsError a)
ctxA'fromEffPromise = ctx_asyncX_to_syncAX <<< async'fromEffPromise

ctxS'run :: forall a. RunS () a -> a
ctxS'run = thenableR'sync unit <<< js_ctx_provideInitial

ctxA'run :: forall a. RunS (Ctx'x_async ()) a -> ThenableR Tag'Async Unit a
ctxA'run = js_ctx_provideInitial <<< ctx_syncAX_to_asyncX

ctxA'lift :: forall x a. ThenableR Tag'Async Unit a -> RunS (Ctx'x_async x) a
ctxA'lift = unsafeCoerce

ctxA'adapt
  :: forall x x' a
   . (RunS x a -> RunS x' a)
  -> (RunS (Ctx'x_async x) a -> RunS (Ctx'x_async x') a)
ctxA'adapt = unsafeCoerce

foreign import js_ctx_runReaderAt
  :: forall rt v x' x a. String -> v -> Run' rt x a -> Run' rt x' a

foreign import js_ctx_at :: forall x v. String -> Ctx x -> v

foreign import js_ctx_runExceptAt
  :: forall rt x' x a. String -> Run' rt x a -> Run' rt x' a

foreign import js_ctx_unwrapErrorAt
  :: forall x e. String -> Ctx x -> JsError -> e

foreign import js_ctx_throwAt :: forall e x a. String -> e -> Ctx x -> a

foreign import js_ctx_provideInitial
  :: forall rt a. Run' rt () a -> ThenableR rt Unit a

ctx_syncAX_to_asyncX :: forall x a. RunS (Ctx'x_async x) a -> RunA x a
ctx_syncAX_to_asyncX = unsafeCoerce

ctx_asyncX_to_syncAX :: forall x a. RunA x a -> RunS (Ctx'x_async x) a
ctx_asyncX_to_syncAX = unsafeCoerce