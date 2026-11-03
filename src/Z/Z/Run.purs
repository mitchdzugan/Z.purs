module Z.Z.Run
  ( Ctx
  , Ctx'Async
  , Ctx'Except
  , Ctx'x'async
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
  ) where

import Prelude

import Data.Either (Either)
import Data.Symbol (reflectSymbol)
import Effect (Effect)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Z.Z.Core (class ConsSymbol, JsError, eOr'_, mapL)
import Z.Z.Defaultable (class Generable, GDefault)
import Z.Z.Promise (EffPromise)
import Z.Z.ThenableR
  ( Tag'Async
  , Tag'Sync
  , ThenableR
  , async'fromEff
  , async'fromEffPromise
  , thenableR'ask
  , thenableR'try
  )

-----------------------------------------------------------

foreign import data Ctx :: Row Type -> Type

foreign import data Ctx'Except :: Type -> Type

foreign import data Ctx'Async :: Type

instance (TypeEquals x ()) => Generable (Ctx x) GDefault (Ctx x) where
  mkGenerable = js_ctx_default

type Ctx'x'async x = (_'x'async :: Ctx'Async | x)

type Run' rt x a = ThenableR rt (Ctx x) a

type RunS x a = Run' Tag'Sync x a

type RunA x a = Run' Tag'Async x a

type Future a = RunA () a

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

ctxA'fromEff :: forall x a. Effect a -> RunS (Ctx'x'async x) (Either JsError a)
ctxA'fromEff = ctx_asyncX_to_syncAX <<< async'fromEff

ctxA'fromEffPromise
  :: forall x a. EffPromise a -> RunS (Ctx'x'async x) (Either JsError a)
ctxA'fromEffPromise = ctx_asyncX_to_syncAX <<< async'fromEffPromise

ctxA'run :: forall x a. RunS (Ctx'x'async x) a -> RunA x a
ctxA'run = ctx_syncAX_to_asyncX

ctxA'lift :: forall x a. RunA () a -> RunS (Ctx'x'async x) a
ctxA'lift = unsafeCoerce

ctxA'adapt
  :: forall x x' a
   . (RunS x a -> RunS x' a)
  -> (RunS (Ctx'x'async x) a -> RunS (Ctx'x'async x') a)
ctxA'adapt = unsafeCoerce

foreign import js_ctx_runReaderAt
  :: forall rt v x' x a. String -> v -> Run' rt x a -> Run' rt x' a

foreign import js_ctx_default :: forall x. Ctx x

foreign import js_ctx_at :: forall x v. String -> Ctx x -> v

foreign import js_ctx_runExceptAt
  :: forall rt x' x a. String -> Run' rt x a -> Run' rt x' a

foreign import js_ctx_unwrapErrorAt
  :: forall x e. String -> Ctx x -> JsError -> e

foreign import js_ctx_throwAt :: forall e x a. String -> e -> Ctx x -> a

ctx_syncAX_to_asyncX :: forall x a. RunS (Ctx'x'async x) a -> RunA x a
ctx_syncAX_to_asyncX = unsafeCoerce

ctx_asyncX_to_syncAX :: forall x a. RunA x a -> RunS (Ctx'x'async x) a
ctx_asyncX_to_syncAX = unsafeCoerce