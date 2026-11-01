module Z.Z.Run
  ( Ctx
  , Ctx'Async
  , Ctx'Except
  , Ctx'Sync
  , CtxR
  , RunA
  , RunS
  , ctx'askAt
  , ctx'attempt
  , ctx'runAsync
  , ctx'runExceptAt
  , ctx'runReaderAt
  , ctx'runSync
  , ctx'throwAt
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (reflectSymbol)
import Type.Proxy (Proxy(..))
import Z.Z.Core (class ConsSymbol)
import Z.Z.Future (Future)

-----------------------------------------------------------

foreign import data Ctx :: Type -> Row Type -> Type

foreign import data Ctx'Async :: Type

foreign import data Ctx'Sync :: Type

foreign import data Ctx'Except :: Type -> Type

type CtxR rt x a = Ctx rt x -> a

type RunS x a = forall rt. CtxR rt x a

type RunA x a = CtxR Ctx'Async x a

ctx'runSync :: forall a. RunS () a -> a
ctx'runSync = js_ctx_runSync

ctx'runAsync :: forall a. RunA () a -> Future a
ctx'runAsync = js_ctx_runAsync

ctx'runReaderAt
  :: forall @p v x' x a
   . ConsSymbol p v x' x
  => v
  -> RunS x a
  -> RunS x' a
ctx'runReaderAt = js_ctx_runReaderAt (reflectSymbol (Proxy @p))

ctx'askAt :: forall @p v x' x. ConsSymbol p v x' x => RunS x v
ctx'askAt = js_ctx_askAt (reflectSymbol (Proxy @p))

ctx'runExceptAt
  :: forall @p e x' x a
   . ConsSymbol p (Ctx'Except e) x' x
  => RunS x a
  -> RunS x' a
ctx'runExceptAt = js_ctx_runExceptAt Left Right (reflectSymbol (Proxy @p))

ctx'throwAt
  :: forall @p rt e x' x a
   . ConsSymbol p (Ctx'Except e) x' x
  => e
  -> CtxR rt x a
ctx'throwAt = js_ctx_throwAt (reflectSymbol (Proxy @p))

ctx'attempt :: forall x a. Future a -> RunA x a
ctx'attempt f = js_ctx_liftFuture f pure

-----------------------------------------------------------

foreign import js_ctx_runSync :: forall a. RunS () a -> a

foreign import js_ctx_runAsync :: forall a. RunA () a -> Future a

foreign import js_ctx_askAt :: forall v x. String -> RunS x v

foreign import js_ctx_runReaderAt
  :: forall v x' x a. String -> v -> RunS x a -> RunS x' a

foreign import js_ctx_throwAt :: forall e x a. String -> e -> RunS x a

foreign import js_ctx_runExceptAt
  :: forall e x' x a
   . (e -> Either e a)
  -> (a -> Either e a)
  -> String
  -> RunS x a
  -> RunS x' a

foreign import js_ctx_liftFuture
  :: forall x a. Future a -> (a -> RunA x a) -> RunA x a