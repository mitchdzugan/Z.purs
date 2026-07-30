module Z.Z.X
  ( A
  , AFF
  , AffF
  , E
  , EA
  , EarlyReturn
  , Edit
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
  , RWa
  , RWaA
  , RWaE
  , RWaEA
  , RWaS
  , RWaSA
  , RWaSE
  , RWaSEA
  , Result
  , S
  , SA
  , SE
  , SEA
  , TEarlyResult
  , TEarlyReturn
  , TError
  , TResult
  , W
  , WA
  , WE
  , WEA
  , WRITERa
  , WS
  , WSA
  , WSE
  , Wa
  , WaA
  , WaE
  , WaEA
  , WaS
  , WaSA
  , WaSE
  , X
  , X2
  , XBASE
  , XBaseF
  , XRet
  , XShortCircuit
  , XWa
  , edit
  , type (!$)
  , type (!)
  , type (-!$)
  , type (-!)
  , x2EvalAff
  , x2ExecAff
  , xAEff
  , xAff
  , xAsk
  , xAskAt
  , xBindE
  , xEffectPromise
  , xEval
  , xEvalAff
  , xEvalS
  , xEvalSAt
  , xExec
  , xExecAff
  , xExecS
  , xFail
  , xGet
  , xGetAt
  , xHush
  , xInfo
  , xInvert
  , xListen
  , xListen_
  , xLogError
  , xLogWarning
  , xMapE
  , xMapW
  , xMapWE
  , xModify
  , xOk
  , xOrDefault
  , xOut
  , xOutErr
  , xOver
  , xOverAt
  , xOverAt_
  , xOver_
  , xParser
  , xPlusS
  , xPreview
  , xPreviewR
  , xPutAt
  , xRespondWithAt
  , xRespondWith
  , xResult
  , xRetFail
  , xRetLift
  , xReturn
  , xReview
  , xReviewR
  , xRunS
  , xSay
  , xSet
  , xSetAt_
  , xSet_
  , xTell
  , xTellMappedHush
  , xTellMappedMHush
  , xTimeout
  , xToArrayOf
  , xToArrayOfR
  , xTry
  , xTryUntil
  , xUnresult
  , xUnwrap
  , xUnwrap'
  , xView
  , xViewAt_
  , xViewR
  , xView_
  , xWithRet
  ) where

import Prelude

import Control.Monad as Monad
import Control.Promise as Promise
import Data.Either as Eor
import Data.Lens as Lens
import Data.List.Types as ListT
import Data.Maybe as May
import Data.Maybe.First as MayFirst
import Data.Monoid as Monoid
import Data.Monoid.Endo as Endo
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tup
import Data.Tuple.Nested as TupN
import Effect as Eff
import Effect.Aff as Aff
import Effect.Class as EffC
import Effect.Unsafe as Unsafe
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)
import Prim.Row as Row
import Record as Rec
import Run as R
import Run.Except as RunE
import Run.Reader as RunR
import Run.State as RunS
import Run.Writer as RunW
import Type.Proxy as P
import Type.Row (type (+))
import Parsing as Parsing
import Z.Z.Barlow as Bl
import Z.Z.Defaultable as ZD
import Z.Z.Core as Z

------------------------------------------------------------------

xParser :: forall x s a. s -> Parsing.Parser s a -> R.Run (E Z.ParseError x) a
xParser s pr = xOk $ Z.runParser s pr

xOrDefault :: forall x d. ZD.Defaultable d => R.Run x (May.Maybe d) -> R.Run x d
xOrDefault m = m <#> ZD.orDefault

--------------- EVAL -------------------------------------------------------

xEval :: forall a. X () a -> a
xEval r = Unsafe.unsafePerformEffect $ R.runBaseEffect $ R.expand $ runXBase r

xExec :: forall e a. X (E e ()) a -> Eor.Either e a
xExec = xEval <<< xTry

xEvalAff :: forall a. X (A ()) a -> Aff.Aff a
xEvalAff x = R.match { aff: \(AffCmd a) -> a } # R.run $ runXBase x

xExecAff :: forall e a. X (EA e ()) a -> Aff.Aff (Eor.Either e a)
xExecAff = xEvalAff <<< xTry

x2EvalAff :: forall a. X2 () A a -> Aff.Aff a
x2EvalAff x = R.match { aff: \(AffCmd a) -> a } # R.run $ runXBase do
  RunW.runWriter (RunR.runReader {} $ RunS.evalState {} x) <#> Tup.snd

x2ExecAff :: forall e a. X2 () (EA e) a -> Aff.Aff (Eor.Either e a)
x2ExecAff = x2EvalAff <<< xTry

--------------- EDIT ------------------------------------------------------

type Edit s = X (S s ()) Unit

edit :: forall a. a -> Edit a -> a
edit init m = R.extract $ RunS.execState init $
  runXBase m

--------------- ShortCiruit ----------------------------------------------

newtype EarlyReturn e a = EarlyReturn (Eor.Either e a)

type XShortCircuit x e a r = R.Run (E (EarlyReturn e a) x) r

xReturn :: forall x e r. r -> XShortCircuit x e r Unit
xReturn r = xFail $ EarlyReturn $ Eor.Right r

xRetFail :: forall x e r a. e -> XShortCircuit x e r a
xRetFail e = xFail $ EarlyReturn $ Eor.Left e

xRetLift
  :: forall x e r a
   . R.Run (E e + E (EarlyReturn e r) + x) a
  -> XShortCircuit x e r a
xRetLift = xMapE (EarlyReturn <<< Eor.Left)

xWithRet :: forall x e r. XShortCircuit (E e x) e r r -> R.Run (E e x) r
xWithRet m = RunE.runExcept m >>= handleRes
  where
  handleRes (Eor.Left (EarlyReturn earlyRet)) = xOk earlyRet
  handleRes (Eor.Right ret) = pure ret

xTryUntil
  :: forall x e r
   . R.Run (E e + E r + E e x) r
  -> Array (e -> R.Run (E e + E r + E e x) r)
  -> R.Run (E e x) r
xTryUntil try1 tryRest = xInvert do
  e1 <- xInvert try1
  Z.reduceM (\e tryN -> xInvert $ tryN e) e1 tryRest

--------------- R FNS -----------------------------------------------------

xRespondWith :: forall x r a. r -> R.Run (R r x) a -> R.Run x a
xRespondWith = RunR.runReader

xRespondWithAt
  :: forall @p x' x r a
   . IsSymbol p
  => Cons p (RunR.Reader r) x' x
  => r
  -> R.Run x a
  -> R.Run x' a
xRespondWithAt = RunR.runReaderAt (P.Proxy :: P.Proxy p)

xAsk :: forall x r. R.Run (R r x) r
xAsk = RunR.ask

xAskAt
  :: forall @p x' x r. IsSymbol p => Cons p (RunR.Reader r) x' x => R.Run x r
xAskAt = RunR.askAt (P.Proxy :: P.Proxy p)

xViewR :: forall x s t a b. Lens.Lens s t a b -> R.Run (R s x) a
xViewR l = xAsk <#> Lens.view l

xReviewR :: forall x s t a b. Lens.Review s t a b -> R.Run (R b x) t
xReviewR l = xAsk <#> Lens.review l

xToArrayOfR
  :: forall x s t a b
   . Lens.Fold (Endo.Endo Function (ListT.List a)) s t a b
  -> R.Run (R s x) (Array a)
xToArrayOfR l = xAsk <#> Lens.toArrayOf l

xPreviewR
  :: forall x s t a b
   . Lens.Fold (MayFirst.First a) s t a b
  -> R.Run (R s x) (May.Maybe a)
xPreviewR l = xAsk <#> Lens.preview l

--------------- W FNS -----------------------------------------------------

xSay :: forall x m w. Monad.Monad m => w -> R.Run (W (m w) x) Unit
xSay w = RunW.tell $ pure w

xTell :: forall x w. Monoid.Monoid w => w -> R.Run (W w x) Unit
xTell w = RunW.tell w

xTellMappedHush
  :: forall x e m d w
   . Monad.Monad m
  => ZD.Defaultable d
  => (e -> w)
  -> R.Run (WE (m w) e x) d
  -> R.Run (W (m w) x) d
xTellMappedHush mapW m = xTry m >>= onDone
  where
  onDone (Eor.Left e) = xSay (mapW e) <#> const ZD.default
  onDone (Eor.Right r) = pure $ r

xTellMappedMHush
  :: forall x e m d w
   . Monad.Monad m
  => ZD.Defaultable d
  => (e -> m w)
  -> R.Run (WE (m w) e x) d
  -> R.Run (W (m w) x) d
xTellMappedMHush mapW m = xTry m >>= onDone
  where
  onDone (Eor.Left e) = RunW.tell (mapW e) <#> const ZD.default
  onDone (Eor.Right r) = pure $ r

xMapW
  :: forall x m w1 w2 a
   . Monad.Monad m
  => Monoid.Monoid (m w1)
  => Monoid.Monoid (m w2)
  => (w1 -> w2)
  -> R.Run (W (m w1) + W (m w2) x) a
  -> R.Run (W (m w2) x) a
xMapW f m = do
  (w TupN./\ res) <- RunW.runWriter m
  RunW.tell $ map f w
  pure res

xListen
  :: forall x @w a. Monoid.Monoid w => R.Run (W w x) a -> R.Run x (w TupN./\ a)
xListen = RunW.runWriter

xListen_
  :: forall x @w. Monoid.Monoid w => R.Run (W w x) Unit -> R.Run x w
xListen_ m = RunW.runWriter m <#> Tup.fst

--------------- S FNS -----------------------------------------------------

xGet :: forall x s. R.Run (S s x) s
xGet = RunS.get

xGetAt
  :: forall @p x' x s. IsSymbol p => Cons p (RunS.State s) x' x => R.Run x s
xGetAt = RunS.getAt (P.Proxy :: P.Proxy p)

xPutAt
  :: forall @p x' x s
   . IsSymbol p
  => Cons p (RunS.State s) x' x
  => s
  -> R.Run x Unit
xPutAt v = RunS.putAt (P.Proxy :: P.Proxy p) v

xModify :: forall x s. (s -> s) -> R.Run (S s x) Unit
xModify f = RunS.modify f

xView :: forall x s t a b. Lens.Lens s t a b -> R.Run (S s x) a
xView l = xGet <#> Lens.view l

xView_
  :: forall @sym x lenses s t a b
   . Bl.ParseSymbol sym lenses
  => Bl.ConstructBarlow lenses (Bl.Forget a) s t a b
  => Bl.IsSymbol sym
  => R.Run (S s x) a
xView_ = xGet <#> Lens.view (Bl.barlow @sym)

xViewAt_
  :: forall @p @sym x x' lenses s t a b
   . IsSymbol p
  => Cons p (RunS.State s) x' x
  => Bl.ParseSymbol sym lenses
  => Bl.ConstructBarlow lenses (Bl.Forget a) s t a b
  => Bl.IsSymbol sym
  => R.Run x a
xViewAt_ = (xGetAt @p) <#> Lens.view (Bl.barlow @sym)

xToArrayOf
  :: forall x s t a b
   . Lens.Fold (Endo.Endo Function (ListT.List a)) s t a b
  -> R.Run (S s x) (Array a)
xToArrayOf l = xGet <#> Lens.toArrayOf l

xReview :: forall x s t a b. Lens.Review s t a b -> b -> R.Run (S t x) Unit
xReview l b = RunS.put $ Lens.review l b

xPreview
  :: forall x s t a b
   . Lens.Fold (MayFirst.First a) s t a b
  -> R.Run (S s x) (May.Maybe a)
xPreview l = xGet <#> Lens.preview l

xOver :: forall x s a b. Lens.Setter s s a b -> (a -> b) -> R.Run (S s x) Unit
xOver l f = RunS.get >>= RunS.put <<< Lens.over l f

xOverAt
  :: forall @p x' x s a b
   . IsSymbol p
  => Cons p (RunS.State s) x' x
  => Lens.Setter s s a b
  -> (a -> b)
  -> R.Run x Unit
xOverAt l f = (xGetAt @p) >>= (xPutAt @p) <<< Lens.over l f

xSet :: forall x s a b. Lens.Setter s s a b -> b -> R.Run (S s x) Unit
xSet l v = RunS.get >>= RunS.put <<< Lens.set l v

xSet_
  :: forall @sym x s a b lenses
   . Bl.IsSymbol sym
  => Bl.ParseSymbol sym lenses
  => Bl.ConstructBarlow lenses Function s s a b
  => b
  -> R.Run (S s x) Unit
xSet_ v = RunS.get >>= RunS.put <<< Lens.set (Bl.barlow @sym) v

xSetAt_
  :: forall @p @sym x' x s a b lenses
   . Bl.IsSymbol sym
  => Bl.ParseSymbol sym lenses
  => Bl.ConstructBarlow lenses Function s s a b
  => IsSymbol p
  => Cons p (RunS.State s) x' x
  => b
  -> R.Run x Unit
xSetAt_ v = (xGetAt @p) >>= (xPutAt @p) <<< Lens.set (Bl.barlow @sym) v

xOver_
  :: forall @sym x s a b lenses
   . Bl.IsSymbol sym
  => Bl.ParseSymbol sym lenses
  => Bl.ConstructBarlow lenses Function s s a b
  => (a -> b)
  -> R.Run (S s x) Unit
xOver_ f = RunS.get >>= RunS.put <<< Lens.over (Bl.barlow @sym) f

xOverAt_
  :: forall @p @sym x' x s a b lenses
   . IsSymbol p
  => Cons p (RunS.State s) x' x
  => Bl.IsSymbol sym
  => Bl.ParseSymbol sym lenses
  => Bl.ConstructBarlow lenses Function s s a b
  => (a -> b)
  -> R.Run x Unit
xOverAt_ f = (xGetAt @p) >>= (xPutAt @p) <<< Lens.over (Bl.barlow @sym) f

xExecS :: forall x s a. s -> R.Run (S s x) a -> R.Run x (s TupN./\ a)
xExecS = RunS.runState

xEvalS :: forall x s a. s -> R.Run (S s x) a -> R.Run x a
xEvalS i m = RunS.runState i m <#> Tup.snd

xEvalSAt
  :: forall @p x' x s a
   . IsSymbol p
  => Cons p (RunS.State s) x' x
  => s
  -> R.Run x a
  -> R.Run x' a
xEvalSAt i m = RunS.runStateAt (P.Proxy :: P.Proxy p) i m <#> Tup.snd

xRunS :: forall x s a. s -> R.Run (S s x) a -> R.Run x s
xRunS i m = RunS.runState i m <#> Tup.fst

xPlusS
  :: forall x r1 r2 @l a v
   . IsSymbol l
  => Row.Lacks l r1
  => Cons l a r1 r2
  => a
  -> R.Run (S { | r2 } + S { | r1 } + x) v
  -> R.Run (S { | r1 } x) v
xPlusS v m = do
  curr <- xGet
  let next = Rec.insert (Proxy :: Proxy l) v curr
  (s TupN./\ r) <- xExecS next m
  RunS.put (Rec.delete (Proxy :: Proxy l) s)
  pure r

--------------- E FNS -----------------------------------------------------

type Result w e a = { w :: (Array w), v :: (Eor.Either e a) }

xResult :: forall x w e a. R.Run (WE (Array w) e x) a -> R.Run x (Result w e a)
xResult m = do
  w <- RunW.runWriter $ RunE.runExcept m
  pure $ { w: (Tup.fst w), v: (Tup.snd w) }

xUnresult :: forall x w e a. (Result w e a) -> R.Run (WE (Array w) e x) a
xUnresult { w, v } = do
  RunW.tell w
  xOk v

xBindE
  :: forall x e1 e2 a
   . (e1 -> R.Run (E e2 x) a)
  -> R.Run (E e1 + E e2 x) a
  -> R.Run (E e2 x) a
xBindE h m = RunE.runExcept m >>= onDone
  where
  onDone (Eor.Left e1) = h e1
  onDone (Eor.Right r) = pure r

xMapE
  :: forall x e1 e2 a
   . (e1 -> e2)
  -> R.Run (E e1 + E e2 x) a
  -> R.Run (E e2 x) a
xMapE f m = xBindE (xFail <<< f) m

xMapWE
  :: forall x m w1 w2 e1 e2 a
   . Monad.Monad m
  => Monoid.Monoid (m w1)
  => Monoid.Monoid (m w2)
  => (w1 -> w2)
  -> (e1 -> e2)
  -> R.Run (W (m w1) + W (m w2) + E e1 + E e2 x) a
  -> R.Run (W (m w2) + E e2 x) a
xMapWE fw fe m = xMapW fw $ xMapE fe m

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

xUnwrap' :: forall x a. May.Maybe a -> X (E Z.JsError x) a
xUnwrap' = xUnwrap $ Z.jsError' "Nothing#unwrap"

xHush :: forall x e d. ZD.Defaultable d => R.Run (E e x) d -> R.Run x d
xHush m = xOrDefault $ xTry m <#> Eor.hush

xInvert :: forall x e a. R.Run (E a + E e x) e -> R.Run (E e x) a
xInvert r = xTry r <#> Z.invert >>= xOk

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

xEffectPromise
  :: forall a x
   . Eff.Effect (Promise.Promise a)
  -> X (EA Z.JsError x) a
xEffectPromise = effectPromiseToAff >>> xAff

xTimeout :: forall x. Int -> X (A x) Unit
xTimeout ms = Z.fDiscard $ xTry $ xEffectPromise $ js_timeout ms

--------------- CORE TYPE ---------------------------------------------------

type X x a = R.Run (XBASE x) a
type X2 x fx a = R.Run (fx + XBASE + Wa Void + R {} + S {} x) a

type XWa w fx a = R.Run (XBASE + fx + Wa w ()) a

type TEarlyReturn
  :: forall k1 k2
   . (Row (k1 -> Type) -> Type -> k2)
  -> Row (k1 -> Type)
  -> Type
  -> Type
  -> k2
type TEarlyReturn m x e a = m (E (EarlyReturn e a) x) a

type TEarlyResult
  :: forall k
   . (Row (Type -> Type) -> Type -> k)
  -> Row (Type -> Type)
  -> Type
  -> Type
  -> Type
  -> k
type TEarlyResult m x w e a = m (WE w (EarlyReturn e a) x) a

type TError
  :: forall k1 k2 k3
   . (Row (k1 -> Type) -> k2 -> k3)
  -> Row (k1 -> Type)
  -> Type
  -> k2
  -> k3
type TError m x e a = m (E e x) a

type TResult
  :: forall k1 k2
   . (Row (Type -> Type) -> k1 -> k2)
  -> Row (Type -> Type)
  -> Type
  -> Type
  -> k1
  -> k2
type TResult m x w e a = m (WE w e x) a

infixr 0 type TEarlyReturn as !$
infixr 0 type TEarlyResult as -!$
infixr 0 type TError as !
infixr 0 type TResult as -!

type XRet x e a = X (E (EarlyReturn e a) x) a

-- type Xclass x f a = R.Run (f x) a

--------------- AFF -------------------------------------------------------

data AffF a = AffCmd (Aff.Aff a)

derive instance functorAffF :: Functor AffF

type AFF x = (aff :: AffF | x)

_aff = P.Proxy :: P.Proxy "aff"

aff :: forall f r. (Aff.Aff f) -> R.Run (AFF + r) f
aff f = R.lift _aff (AffCmd f)

--------------- XBase ---------------------------------------------------

foreign import js_consoleFn
  :: forall a. String -> String -> Array a -> Eff.Effect Unit

foreign import js_consoleDirectFn
  :: forall a. String -> a -> Eff.Effect Unit

foreign import js_getStack :: Eff.Effect String

data XBaseF a = LogCmd String String Z.JsAny a | LogDirectCmd String Z.JsAny a

derive instance functorXBaseF :: Functor XBaseF

type XBASE x = (xBase :: XBaseF | x)

_eff = P.Proxy :: P.Proxy "xBase"

handleXBase :: forall r. XBaseF ~> R.Run r
handleXBase = case _ of
  LogCmd k src v e -> do
    pure $ Unsafe.unsafePerformEffect $ js_consoleFn k src [ v ]
    pure e
  LogDirectCmd k v e -> do
    pure $ Unsafe.unsafePerformEffect $ js_consoleDirectFn k v
    pure e

runXBase :: forall r. R.Run (XBASE + r) ~> R.Run r
runXBase = R.run (R.on _eff handleXBase R.send)

xOut :: forall l x. l -> X x Unit
xOut v = Z.fDiscard $ R.lift _eff (LogDirectCmd "log" (Z.jsAny v) unit)

xOutErr :: forall l x. l -> X x Unit
xOutErr v = Z.fDiscard $ R.lift _eff (LogDirectCmd "error" (Z.jsAny v) unit)

xLogCmd :: forall l x. String -> l -> X x Unit
xLogCmd k v = do
  let src = Unsafe.unsafePerformEffect js_getStack
  Z.fDiscard $ R.lift _eff (LogCmd k src (Z.jsAny v) unit)

xInfo :: forall l x. l -> X x Unit
xInfo = xLogCmd "log"

xLogWarning :: forall l x. l -> X x Unit
xLogWarning = xLogCmd "warn"

xLogError :: forall l x. l -> X x Unit
xLogError = xLogCmd "error"

--------------- XBuilders ---------------------------------------------------

type WRITERa w x = RunW.WRITER (Array w) x

type R r x =
  RunR.READER r + x

type W w x =
  RunW.WRITER w + x

type Wa w x =
  WRITERa w + x

type RW r w x =
  RunR.READER r + RunW.WRITER w + x

type RWa r w x =
  RunR.READER r + WRITERa w + x

type S s x =
  RunS.STATE s + x

type RS r s x =
  RunR.READER r + RunS.STATE s + x

type WS w s x =
  RunW.WRITER w + RunS.STATE s + x

type WaS w s x =
  WRITERa w + RunS.STATE s + x

type RWS r w s x =
  RunR.READER r + RunW.WRITER w + RunS.STATE s + x

type RWaS r w s x =
  RunR.READER r + WRITERa w + RunS.STATE s + x

type E :: forall k. Type -> Row (k -> Type) -> Row (k -> Type)
type E e x =
  RunE.EXCEPT e + x

type RE r e x =
  RunR.READER r + RunE.EXCEPT e + x

type WE w e x =
  RunW.WRITER w + RunE.EXCEPT e + x

type WaE w e x =
  WRITERa w + RunE.EXCEPT e + x

type RWE r w e x =
  RunR.READER r + RunW.WRITER w + RunE.EXCEPT e + x

type RWaE r w e x =
  RunR.READER r + WRITERa w + RunE.EXCEPT e + x

type SE s e x =
  RunS.STATE s + RunE.EXCEPT e + x

type RSE r s e x =
  RunR.READER r + RunS.STATE s + RunE.EXCEPT e + x

type WSE w s e x =
  RunW.WRITER w + RunS.STATE s + RunE.EXCEPT e + x

type WaSE w s e x =
  WRITERa w + RunS.STATE s + RunE.EXCEPT e + x

type RWSE r w s e x =
  RunR.READER r + RunW.WRITER w + RunS.STATE s + RunE.EXCEPT e + x

type RWaSE r w s e x =
  RunR.READER r + WRITERa w + RunS.STATE s + RunE.EXCEPT e + x

type A x =
  AFF + x

type RA r x =
  RunR.READER r + AFF + x

type WA w x =
  RunW.WRITER w + AFF + x

type WaA w x =
  WRITERa w + AFF + x

type RWA r w x =
  RunR.READER r + RunW.WRITER w + AFF + x

type RWaA r w x =
  RunR.READER r + WRITERa w + AFF + x

type SA s x =
  RunS.STATE s + AFF + x

type RSA r s x =
  RunR.READER r + RunS.STATE s + AFF + x

type WSA w s x =
  RunW.WRITER w + RunS.STATE s + AFF + x

type WaSA w s x =
  WRITERa w + RunS.STATE s + AFF + x

type RWSA r w s x =
  RunR.READER r + RunW.WRITER w + RunS.STATE s + AFF + x

type RWaSA r w s x =
  RunR.READER r + WRITERa w + RunS.STATE s + AFF + x

type EA e x =
  RunE.EXCEPT e + AFF + x

type REA r e x =
  RunR.READER r + RunE.EXCEPT e + AFF + x

type WEA w e x =
  RunW.WRITER w + RunE.EXCEPT e + AFF + x

type WaEA w e x =
  WRITERa w + RunE.EXCEPT e + AFF + x

type RWEA r w e x =
  RunR.READER r + RunW.WRITER w + RunE.EXCEPT e + AFF + x

type RWaEA r w e x =
  RunR.READER r + WRITERa w + RunE.EXCEPT e + AFF + x

type SEA s e x =
  RunS.STATE s + RunE.EXCEPT e + AFF + x

type RSEA r s e x =
  RunR.READER r + RunS.STATE s + RunE.EXCEPT e + AFF + x

type RWSEA r w s e x =
  RunR.READER r + RunW.WRITER w + RunS.STATE s + RunE.EXCEPT e + AFF + x

type RWaSEA r w s e x =
  RunR.READER r + WRITERa w + RunS.STATE s + RunE.EXCEPT e + AFF + x
