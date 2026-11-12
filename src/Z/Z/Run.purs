module Z.Z.Run
  ( Ctx
  , Ctx'Async
  , Ctx'Except
  , Ctx'Extract
  , Future
  , Run'
  , RunA
  , RunG
  , RunG1
  , RunJoin
  , RunS
  , ctx'clone
  , ctx'expand
  , ctxA'adapt
  , ctxA'lift
  , ctxA'run
  , ctxA'runFrom
  , ctxE'failAt
  , ctxE'okAt
  , ctxE'runAt
  , ctxE'throwAt
  , ctxR'askAt
  , ctxR'runAt
  , ctxS'run
  , ctxS'runFrom
  , type (*->)
  , type (@*->)
  , type (@->)
  , type (@.->)
  , type (@^->)
  ) where

import Prelude

import Data.Either (Either)
import Data.Symbol (reflectSymbol)
import Foreign.Object (Object)
import Prim.Row (class Union)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Z.Z.Core (class ConsSymbol, JsError, eOr'_, mapL, obj'empty)
import Z.Z.ThenableR
  ( Tag'Async
  , Tag'Sync
  , ThenableR
  , thenableR'ask
  , thenableR'sync
  , thenableR'try
  )

-----------------------------------------------------------

foreign import data Ctx :: Row Type -> Type

foreign import data Ctx'Extract :: Row Type -> Type

foreign import data Ctx'Except :: Type -> Type

foreign import data Ctx'Async :: Type

type Run' rt x = ThenableR rt (Ctx x)

type RunS x = Run' Tag'Sync x

type RunA x = Run' Tag'Async x

type RunG x a rt = Run' rt x a

type RunG1 x a = forall rt. Run' rt x a

type RunJoin :: forall k. (k -> Type) -> (k -> Type) -> Type
type RunJoin f1 f2 = forall rt. f1 rt -> f2 rt

type Future = ThenableR Tag'Async Unit

infixl 3 type RunG as @->
infixl 3 type RunG1 as @*->
infixl 3 type RunS as @.->
infixl 3 type RunA as @^->
infixl 2 type RunJoin as *->

ctx'expand
  :: forall x' @x_ x a. Union x' x_ x => x' @-> a *-> x @-> a
ctx'expand = unsafeCoerce

ctxR'askAt :: forall @p rt v x' x. ConsSymbol p v x' x => Run' rt x v
ctxR'askAt = thenableR'ask <#> js_ctx_at (reflectSymbol (Proxy @p))

ctxR'runAt
  :: forall @p rt v x' x a
   . ConsSymbol p v x' x
  => v
  -> Run' rt x a
  -> Run' rt x' a
ctxR'runAt v m = do
  let k = reflectSymbol (Proxy @p)
  ctx <- thenableR'ask
  res <- js_ctx_initReaderAt ctx k v m
  js_ctx_completeReaderAt ctx k $ pure res

ctxE'runAt
  :: forall @p @e rt x' x a
   . ConsSymbol p (Ctx'Except e) x' x
  => Run' rt x a
  -> Run' rt x' (Either e a)
ctxE'runAt m = do
  let symK = reflectSymbol (Proxy @p)
  exc <- thenableR'ask <#> js_ctx_mkExceptAt
  ctxR'runAt @p exc do
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

ctxS'run :: forall a. RunS () a -> a
ctxS'run = thenableR'sync unit <<< js_ctx_provideInitial

ctxA'run :: forall a. RunA () a -> Future a
ctxA'run = js_ctx_provideInitial

ctxS'runFrom :: forall x a. Ctx'Extract x -> RunS x a -> a
ctxS'runFrom x = thenableR'sync unit <<< js_ctx_provideInitialFrom x

ctxA'runFrom :: forall x a. Ctx'Extract x -> RunA x a -> Future a
ctxA'runFrom = js_ctx_provideInitialFrom

ctxA'lift :: forall x a. ThenableR Tag'Async Unit a -> RunA x a
ctxA'lift = unsafeCoerce

ctxA'adapt
  :: forall x x' a. (RunS x a -> RunS x' a) -> (RunA x a -> RunA x' a)
ctxA'adapt = unsafeCoerce

foreign import js_ctx_initReaderAt
  :: forall rt v x' x a
   . Ctx x'
  -> String
  -> v
  -> Run' rt x a
  -> Run' rt x' a

foreign import js_ctx_completeReaderAt
  :: forall rt x a
   . Ctx x
  -> String
  -> Run' rt x a
  -> Run' rt x a

foreign import js_ctx_at :: forall x v. String -> Ctx x -> v

foreign import js_ctx_mkExceptAt
  :: forall x e. Ctx x -> Ctx'Except e

foreign import js_ctx_unwrapErrorAt
  :: forall x e. String -> Ctx x -> JsError -> e

foreign import js_ctx_throwAt :: forall e x a. String -> e -> Ctx x -> a

foreign import js_ctx_provideInitial
  :: forall rt a. Run' rt () a -> ThenableR rt Unit a

foreign import js_ctx_provideInitialFrom
  :: forall x rt a. Ctx'Extract x -> Run' rt x a -> ThenableR rt Unit a

foreign import js_ctx_clone
  :: forall x' x. Object (forall a. Ctx x' -> a) -> Ctx x' -> Ctx'Extract x

ctx'clone :: forall @x x'. Ctx x' -> Ctx'Extract x
ctx'clone = js_ctx_clone obj'empty