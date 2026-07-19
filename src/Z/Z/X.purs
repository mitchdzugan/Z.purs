module Z.Z.X
  ( A
  , AFF
  , AffF(..)
  , E
  , EFF
  , EffF
  , R
  , RunX
  , S
  , UpdateX
  , W
  , aff
  , eff
  , logInfo
  , pass
  , r_view
  , result
  , runEff
  , s_over
  , s_view
  , tryAff
  , tryEff
  , updateX
  , xLiftE
  , xMapE
  , xOk
  , xSet
  , xWithReturn
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens as Lens
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Unsafe as Unsafe
import Prim.Row as Row
import Run (Run, lift, extract, run)
import Run as Run
import Run.Except (EXCEPT, throw)
import Run.Except as RunE
import Run.Reader (READER)
import Run.Reader as RunR
import Run.State (STATE, execState)
import Run.State as RunS
import Run.Writer (WRITER)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Z.Z.Core (JsError(..))

foreign import js_consoleFn
  :: forall a. String -> String -> Array a -> Effect Unit

data AffF a = AffCmd (Aff.Aff a)

derive instance functorAffF :: Functor AffF

type AFF x = (aff :: AffF | x)

_aff = Proxy :: Proxy "aff"

aff :: forall f r. (Aff.Aff f) -> Run (AFF + r) f
aff f = lift _aff (AffCmd f)

------------------------------

data EffF a = EffCmd (Effect a)

derive instance functorEffF :: Functor EffF

type EFF x = (eff :: EffF | x)

_eff = Proxy :: Proxy "eff"

eff :: forall f r. (Effect f) -> Run (EFF + r) f
eff f = lift _eff (EffCmd f)

handleEff :: forall r. EffF ~> Run r
handleEff = case _ of
  EffCmd e -> pure $ Unsafe.unsafePerformEffect e

runEff :: forall r. Run (EFF + r) ~> Run r
runEff = Run.interpret (Run.on _eff handleEff Run.send)

logInfo :: forall l x. l -> Run (EFF + x) Unit
logInfo v = eff $ js_consoleFn "log" "χ::info" [ v ]

------------------------------

result :: forall e r x. Either e r -> Run (EXCEPT e + x) r
result res =
  case res of
    Right r -> pure r
    Left e -> throw e

tryAff
  :: forall f x. (Aff.Aff f) -> Run (AFF + EXCEPT JsError + x) f
tryAff a = do
  res <- aff $ Aff.attempt a
  xMapE JsError $ result res

tryEff
  :: forall f x. (Effect f) -> Run (AFF + EXCEPT JsError + x) f
tryEff a = do
  res <- aff $ Aff.attempt $ liftEffect a
  xMapE JsError $ result res

type A x = AFF x
type R r x = READER r x

type E :: forall k. Type -> Row (k -> Type) -> Row (k -> Type)
type E e x = EXCEPT e x

type W w x = WRITER w x
type S s x = STATE s x

type RunX x r = Run (EFF + x) r

type UpdateX s = Run (S s + ()) Unit

updateX :: forall a. a -> UpdateX a -> a
updateX init m = extract $ execState init m

r_view :: forall x r a. Lens.Lens' r a -> Run (READER r + x) a
r_view l = do
  o <- RunR.ask
  pure $ Lens.view l o

s_view :: forall x s a. Lens.Lens' s a -> Run (STATE s + x) a
s_view l = do
  o <- RunS.get
  pure $ Lens.view l o

s_over :: forall x s a. Lens.Lens' s a -> (a -> a) -> Run (STATE s + x) Unit
s_over l f = do
  o <- RunS.get
  RunS.put $ Lens.over l f o

xSet :: forall x s a. Lens.Lens' s a -> a -> Run (STATE s + x) Unit
xSet l a = do
  o <- RunS.get
  RunS.put $ Lens.set l a o

pass :: forall a. Applicative a => a Unit
pass = pure unit

xMapE
  :: forall x e1 e2 a
   . (e1 -> e2)
  -> Run (EXCEPT e1 + EXCEPT e2 + x) a
  -> Run (EXCEPT e2 + x) a
xMapE f m = do
  res <- RunE.runExcept m
  result $ either (\e1 -> Left $ f e1) (\r -> Right r) res

xOk :: forall x e a. Either e a -> Run (EXCEPT e x) a
xOk (Left e) = RunE.throw e
xOk (Right a) = pure a

type XWithReturnFn x e a =
  (forall xi. a -> Run (EXCEPT (Either e a) + xi) Unit)
  -> (Run (EXCEPT (Either e a) + EXCEPT e + x) a)

type XWithReturn x e a = XWithReturnFn x e a -> (Run (EXCEPT e + x) a)

xWithReturn
  :: forall x e a. XWithReturn x e a
xWithReturn f = RunE.runExcept m >>= (\x -> handleRes x)
  where
  m = f (RunE.throw <<< Right)
  handleRes (Left earlyRet) = xOk earlyRet
  handleRes (Right ret) = pure ret

xLiftE
  :: forall x e a r
   . Run (EXCEPT e + EXCEPT (Either e a) + x) r
  -> Run (EXCEPT (Either e a) + x) r
xLiftE = xMapE Left
