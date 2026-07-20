module Z.Z.X2
  ( A
  , AFF
  , AffF
  , E
  , EA
  , EFF
  , EarlyReturn
  , Edit
  , EffF(..)
  , R
  , RA
  , RE
  , REA
  , RS
  , RSA
  , RSE
  , RSEA
  , RW
  , RWA
  , RWE
  , RWEA
  , RWS
  , RWSA
  , RWSE
  , RWSEA
  , Result
  , S
  , SA
  , SE
  , SEA
  , W
  , WA
  , WE
  , WEA
  , WS
  , WSA
  , WSE
  , X
  , edit
  , effectPromiseX
  , xAEff
  , xAff
  , xAsk
  , xEval
  , xEvalAff
  , xExec
  , xExecAff
  , xFail
  , xGet
  , xHush
  , xInfo
  , xLogError
  , xLogWarning
  , xMapE
  , xOk
  , xOver
  , xReading
  , xResult
  , xRetErr
  , xRetLift
  , xReturn
  , xSet
  , xTell
  , xTimeout
  , xTry
  , xUnwrap
  , xView
  , xWithRet
  , xrView
  ) where

import Prelude

import Control.Promise as Promise
import Data.Either as Eor
import Data.Lens as Lens
import Data.Maybe as May
import Data.Monoid as Monoid
import Data.Tuple as Tup
import Effect as Eff
import Effect.Aff as Aff
import Effect.Class as EffC
import Effect.Unsafe as Unsafe
import Run as R
import Run.Except as RunE
import Run.Reader as RunR
import Run.State as RunS
import Run.Writer as RunW
import Type.Proxy as P
import Type.Row (type (+))
import Z.Z.Core as Z

--------------- EVAL -------------------------------------------------------

xEval :: forall a. X () a -> a
xEval r = Unsafe.unsafePerformEffect $ R.runBaseEffect $ R.expand $ runEff r

xExec :: forall e a. X (E e ()) a -> Eor.Either e a
xExec = xEval <<< xTry

xEvalAff :: forall a. X (A ()) a -> Aff.Aff a
xEvalAff x = R.match { aff: \(AffCmd a) -> a } # R.run $ runEff x

xExecAff :: forall e a. X (EA e ()) a -> Aff.Aff (Eor.Either e a)
xExecAff = xEvalAff <<< xTry

--------------- EDIT ------------------------------------------------------

type Edit s = X (S s ()) Unit

edit :: forall a. a -> Edit a -> a
edit init m = R.extract $ RunS.execState init $ runEff m

--------------- XRet ------------------------------------------------------

newtype EarlyReturn e a = EarlyReturn (Eor.Either e a)

type XShortCircuit x e a r = R.Run (E (EarlyReturn e a) x) r

type XRet x e r = XShortCircuit x e r r

xReturn :: forall x e r. r -> XShortCircuit x e r Unit
xReturn r = xFail $ EarlyReturn $ Eor.Right r

xRetErr :: forall x e r a. e -> XShortCircuit x e r a
xRetErr e = xFail $ EarlyReturn $ Eor.Left e

xRetLift
  :: forall x e r a
   . R.Run (E e + E (EarlyReturn e r) + x) a
  -> XShortCircuit x e r a
xRetLift = xMapE (EarlyReturn <<< Eor.Left)

xWithRet :: forall x e r. XRet (E e x) e r -> R.Run (E e x) r
xWithRet m = RunE.runExcept m >>= handleRes
  where
  handleRes (Eor.Left (EarlyReturn earlyRet)) = xOk earlyRet
  handleRes (Eor.Right ret) = pure ret

--------------- R FNS -----------------------------------------------------

xReading :: forall x r a. r -> R.Run (R r x) a -> R.Run x a
xReading = RunR.runReader

xAsk :: forall x r. R.Run (R r x) r
xAsk = RunR.ask

xrView :: forall x r a. Lens.Lens' r a -> R.Run (R r x) a
xrView l = do
  o <- xAsk
  pure $ Lens.view l o

--------------- W FNS -----------------------------------------------------

xTell :: forall x w. Monoid.Monoid w => w -> X (W w x) Unit
xTell w = RunW.tell w

--------------- S FNS -----------------------------------------------------

xGet :: forall x s. R.Run (S s x) s
xGet = RunS.get

xView :: forall x s a. Lens.Lens' s a -> R.Run (S s x) a
xView l = do
  o <- xGet
  pure $ Lens.view l o

xOver :: forall x s a. Lens.Lens' s a -> (a -> a) -> R.Run (S s x) Unit
xOver l f = do
  o <- RunS.get
  RunS.put $ Lens.over l f o

xSet :: forall x s a. Lens.Lens' s a -> a -> R.Run (S s x) Unit
xSet l v = do
  o <- RunS.get
  RunS.put $ Lens.set l v o

--------------- E FNS -----------------------------------------------------

type Result w e a = { w :: (Array w), v :: (Eor.Either e a) }

xResult :: forall x w e a. X (WE (Array w) e x) a -> X x (Result w e a)
xResult m = do
  w <- RunW.runWriter $ RunE.runExcept m
  pure $ { w: (Tup.fst w), v: (Tup.snd w) }

xMapE
  :: forall x e1 e2 a
   . (e1 -> e2)
  -> R.Run (RunE.EXCEPT e1 + E e2 x) a
  -> R.Run (E e2 x) a
xMapE f m = do
  res <- RunE.runExcept m
  xOk $ Eor.either (\e1 -> Eor.Left $ f e1) (\r -> Eor.Right r) res

xOk :: forall x e a. Eor.Either e a -> R.Run (E e x) a
xOk (Eor.Left e) = RunE.throw e
xOk (Eor.Right a) = pure a

xTry :: forall x e a. R.Run (E e x) a -> R.Run x (Eor.Either e a)
xTry = RunE.runExcept

xFail :: forall x e a. e -> R.Run (E e x) a
xFail e = RunE.throw e

xUnwrap :: forall x e a. e -> May.Maybe a -> X (E e x) a
xUnwrap _ (May.Just a) = pure a
xUnwrap e _ = xFail e

xHush :: forall x e a. R.Run (E e x) a -> R.Run x (May.Maybe a)
xHush m = xTry m <#> Eor.hush

--------------- A FNS -----------------------------------------------------

foreign import js_timeout :: Int -> Eff.Effect (Promise.Promise Unit)

xAff
  :: forall f x. (Aff.Aff f) -> R.Run (EA Z.JsError x) f
xAff a = do
  res <- aff $ Aff.attempt a
  xMapE Z.JsError $ xOk res

xAEff
  :: forall f x. (Eff.Effect f) -> R.Run (EA Z.JsError x) f
xAEff a = do
  res <- aff $ Aff.attempt $ EffC.liftEffect a
  xMapE Z.JsError $ xOk res

promiseToAff :: forall a. Promise.Promise a -> Aff.Aff a
promiseToAff = Promise.toAff

effectPromiseToAff :: forall a. Eff.Effect (Promise.Promise a) -> Aff.Aff a
effectPromiseToAff e = EffC.liftEffect e >>= promiseToAff

effectPromiseX
  :: forall a x
   . Eff.Effect (Promise.Promise a)
  -> X (EA Z.JsError x) a
effectPromiseX = effectPromiseToAff >>> xAff

xTimeout :: forall x. Int -> X (A x) Unit
xTimeout ms = Z.fDiscard $ xTry $ effectPromiseX $ js_timeout ms

--------------- UNSAFE EFFECT FNS -----------------------------------------

foreign import js_consoleFn
  :: forall a. String -> Array a -> Eff.Effect Unit

xInfo :: forall l x. l -> X x Unit
xInfo v = eff $ js_consoleFn "log" [ v ]

xLogWarning :: forall l x. l -> X x Unit
xLogWarning v = eff $ js_consoleFn "warn" [ v ]

xLogError :: forall l x. l -> X x Unit
xLogError v = eff $ js_consoleFn "error" [ v ]

--------------- CORE TYPE ---------------------------------------------------

type X x a = R.Run (EFF x) a

-- type XRet m e a = R.R

-- type Xclass x f a = R.Run (f x) a

--------------- AFF -------------------------------------------------------

data AffF a = AffCmd (Aff.Aff a)

derive instance functorAffF :: Functor AffF

type AFF x = (aff :: AffF | x)

_aff = P.Proxy :: P.Proxy "aff"

aff :: forall f r. (Aff.Aff f) -> R.Run (AFF + r) f
aff f = R.lift _aff (AffCmd f)

--------------- UNSAFE EFF ---------------------------------------------------

data EffF a = EffCmd (Eff.Effect a)

derive instance functorEffF :: Functor EffF

type EFF x = (eff :: EffF | x)

_eff = P.Proxy :: P.Proxy "eff"

eff :: forall f r. (Eff.Effect f) -> R.Run (EFF + r) f
eff f = R.lift _eff (EffCmd f)

handleEff :: forall r. EffF ~> R.Run r
handleEff = case _ of
  EffCmd e -> pure $ Unsafe.unsafePerformEffect e

runEff :: forall r. R.Run (EFF + r) ~> R.Run r
runEff = R.interpret (R.on _eff handleEff R.send)

--------------- XBuilders ---------------------------------------------------

type R r x =
  RunR.READER r + x

type W w x =
  RunW.WRITER w + x

type RW r w x =
  RunR.READER r + RunW.WRITER w + x

type S s x =
  RunS.STATE s + x

type RS r s x =
  RunR.READER r + RunS.STATE s + x

type WS w s x =
  RunW.WRITER w + RunS.STATE s + x

type RWS r w s x =
  RunR.READER r + RunW.WRITER w + RunS.STATE s + x

type E :: forall k. Type -> Row (k -> Type) -> Row (k -> Type)
type E e x =
  RunE.EXCEPT e + x

type RE r e x =
  RunR.READER r + RunE.EXCEPT e + x

type WE w e x =
  RunW.WRITER w + RunE.EXCEPT e + x

type RWE r w e x =
  RunR.READER r + RunW.WRITER w + RunE.EXCEPT e + x

type SE s e x =
  RunS.STATE s + RunE.EXCEPT e + x

type RSE r s e x =
  RunR.READER r + RunS.STATE s + RunE.EXCEPT e + x

type WSE w s e x =
  RunW.WRITER w + RunS.STATE s + RunE.EXCEPT e + x

type RWSE r w s e x =
  RunR.READER r + RunW.WRITER w + RunS.STATE s + RunE.EXCEPT e + x

type A x =
  AFF + x

type RA r x =
  RunR.READER r + AFF + x

type WA w x =
  RunW.WRITER w + AFF + x

type RWA r w x =
  RunR.READER r + RunW.WRITER w + AFF + x

type SA s x =
  RunS.STATE s + AFF + x

type RSA r s x =
  RunR.READER r + RunS.STATE s + AFF + x

type WSA w s x =
  RunW.WRITER w + RunS.STATE s + AFF + x

type RWSA r w s x =
  RunR.READER r + RunW.WRITER w + RunS.STATE s + AFF + x

type EA e x =
  RunE.EXCEPT e + AFF + x

type REA r e x =
  RunR.READER r + RunE.EXCEPT e + AFF + x

type WEA w e x =
  RunW.WRITER w + RunE.EXCEPT e + AFF + x

type RWEA r w e x =
  RunR.READER r + RunW.WRITER w + RunE.EXCEPT e + AFF + x

type SEA s e x =
  RunS.STATE s + RunE.EXCEPT e + AFF + x

type RSEA r s e x =
  RunR.READER r + RunS.STATE s + RunE.EXCEPT e + AFF + x

type RWSEA r w s e x =
  RunR.READER r + RunW.WRITER w + RunS.STATE s + RunE.EXCEPT e + AFF + x