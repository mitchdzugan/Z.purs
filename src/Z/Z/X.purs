module Z.Z.X
  ( A
  , AFF
  , AffF
  , AtR(..)
  , AtS(..)
  , E
  , EA
  , Edit
  , EvalS(..)
  , EvalW(..)
  , ExecS(..)
  , ExecW(..)
  , Get(..)
  , Modify(..)
  , Over(..)
  , Over_(..)
  , PlusS(..)
  , Preview(..)
  , PreviewR(..)
  , PreviewR_(..)
  , PreviewS(..)
  , PreviewS_(..)
  , Preview_(..)
  , Put(..)
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
  , RunParser(..)
  , RunR(..)
  , RunS(..)
  , RunW(..)
  , S
  , SA
  , SE
  , SEA
  , Say(..)
  , Set(..)
  , Set_(..)
  , Tell(..)
  , ToArrayOf(..)
  , ToArrayOfR(..)
  , ToArrayOfR_(..)
  , ToArrayOfS(..)
  , ToArrayOfS_(..)
  , ToArrayOf_(..)
  , Try(..)
  , TryUntil(..)
  , View(..)
  , ViewR(..)
  , ViewR_(..)
  , ViewS(..)
  , ViewS_(..)
  , View_(..)
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
  , WithReturn(..)
  , X
  , XBASE
  , XBaseF
  , XEnv
  , XState
  , XWa
  , class RWSEFn
  , class XPSel
  , class XReturnP
  , edit
  , rwseApply
  , x
  , xAEff
  , xAff
  , xAt
  , xBindE
  , xEffectPromise
  , xEval
  , xEvalAff
  , xExec
  , xExecAff
  , xFail
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
  , xOk
  , xOut
  , xOutErr
  , xResult
  , xTellMappedHush
  , xTellMappedMHush
  , xTimeout
  , xUnresult
  , xUnwrap
  , xUnwrap'
  ) where

import Prelude

import Control.Monad as Monad
import Control.Promise as Promise
import Data.Either as Eor
import Data.Lens (Forget)
import Data.Lens as Lens
import Data.List.Types as ListT
import Data.Maybe as May
import Data.Maybe.First as MayFirst
import Data.Monoid as Monoid
import Data.Monoid.Endo as Endo
import Data.Newtype (wrap, unwrap, class Newtype) as NT
import Data.Profunctor (class Profunctor)
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tup
import Data.Tuple.Nested as TupN
import Effect as Eff
import Effect.Aff as Aff
import Effect.Class as EffC
import Effect.Unsafe as Unsafe
import Parsing as Parsing
import Prim.Row (class Cons)
import Prim.Row as Row
import Record as Rec
import Run as R
import Run.Except as RunE
import Run.Reader as RunR
import Run.State as RunS
import Run.Writer as RunW
import Type.Equality (class TypeEquals) as TypeEquals
import Type.Proxy (Proxy(..))
import Type.Proxy as P
import Type.Row (type (+))
import Unsafe.Coerce as UnsafeC
import Z.Z.Barlow (class Strong)
import Z.Z.Barlow as Bl
import Z.Z.Core as Z
import Z.Z.Defaultable as ZD
import Z.Z.Ext as ZE

px :: forall @k. P.Proxy k
px = P.Proxy

class RWSEFn
  :: forall k1 k2 k3 k4. Type -> k1 -> k2 -> k3 -> k4 -> Type -> Constraint
class RWSEFn f rp wp sp ep o | f rp wp sp ep -> o where
  rwseApply :: f -> P.Proxy rp -> P.Proxy wp -> P.Proxy sp -> P.Proxy ep -> o

-------------------- OTHER ----------------------

data RunParser = RunParser

instance rwseApplyRunParser ::
  RWSEFn RunParser
    _r
    _w
    _s
    ep
    (s -> Parsing.Parser s a -> R.Run (E Z.ParseError x) a) where
  rwseApply _ _ _ _ _ = xParser

data TryUntil = TryUntil

instance rwseApplyTryUntil ::
  RWSEFn TryUntil
    _r
    _w
    _s
    _e
    ( R.Run (E e + E r + E e x) r
      -> Array (e -> R.Run (E e + E r + E e x) r)
      -> R.Run (E e x) r
    ) where
  rwseApply _ _ _ _ _ try1 tryRest = xInvert do
    e1 <- xInvert try1
    Z.reduceM (\e tryN -> xInvert $ tryN e) e1 tryRest

data WithReturn = WithReturn

class XReturnP :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Constraint
class XReturnP rp wp sp ep fp | rp wp sp ep -> fp

instance XReturnP "reader" "writer" "state" "except" "earlyReturn"
else instance XReturnP _r _w _s ep ep

instance
  ( XReturnP rp wp sp ep p
  , IsSymbol p
  , Cons p (RunE.Except r) x' x
  ) =>
  RWSEFn WithReturn
    rp
    wp
    sp
    ep
    (((r -> R.Run x Unit) -> R.Run x r) -> R.Run x' r) where
  rwseApply _ _ _ _ _ m = RunE.runExceptAt (px @p) (m return) >>= onRes
    where
    return = RunE.throwAt (px @p)
    onRes (Eor.Left ret) = pure ret
    onRes (Eor.Right ret) = pure ret

data RunR = RunR

instance rwseApplyRunEnv ::
  ( IsSymbol rp
  , Cons rp (RunR.Reader r) x' x
  ) =>
  RWSEFn RunR
    rp
    _w
    _s
    _e
    (r -> R.Run x a -> R.Run x' a) where
  rwseApply _ _ _ _ _ = RunR.runReaderAt (P.Proxy :: P.Proxy rp)

--------------------- R/S -----------------------

data Get t = Get t

instance rwseApplyGetR ::
  ( IsSymbol rp
  , Cons rp (RunR.Reader r) x' x
  ) =>
  RWSEFn (Get XEnv) rp _w _s _e (R.Run x r) where
  rwseApply _ rp _ _ _ = RunR.askAt rp

instance rwseApplyGetS ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn (Get XState) _r _w sp _e (R.Run x s) where
  rwseApply _ _ _ sp _ = RunS.getAt sp

data View g = View g

instance rwseApplyView ::
  ( RWSEFn (Get g) rp wp sp ep (R.Run x s)
  ) =>
  RWSEFn (View g) rp wp sp ep ((Lens.Optic (Forget a) s t a b) -> R.Run x a) where
  rwseApply (View t) _ _ _ _ l = do
    v <- mkXFn @rp @wp @sp @ep $ Get t
    pure $ Lens.view l v

data View_ :: forall @k. k -> Type -> Type
data View_ b t = View_ t

instance rwseApplyView_ ::
  ( RWSEFn (Get g) rp wp sp ep (R.Run x s)
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget a) s t a b
  , Bl.IsSymbol sym
  ) =>
  RWSEFn (View_ sym g) rp wp sp ep (R.Run x a) where
  rwseApply (View_ t) _ _ _ _ = do
    v <- mkXFn @rp @wp @sp @ep $ Get t
    pure $ Lens.view (Bl.barlow @sym) v

data Preview g = Preview g

instance rwseApplyPreview ::
  ( RWSEFn (Get g) rp wp sp ep (R.Run x s)
  ) =>
  RWSEFn (Preview g)
    rp
    wp
    sp
    ep
    ((Lens.Optic (Forget (MayFirst.First a)) s t a b) -> R.Run x (May.Maybe a)) where
  rwseApply (Preview t) _ _ _ _ l = do
    v <- mkXFn @rp @wp @sp @ep $ Get t
    pure $ Lens.preview l v

data Preview_ :: forall @k. k -> Type -> Type
data Preview_ b t = Preview_ t

instance rwseApplyPreview_ ::
  ( RWSEFn (Get g) rp wp sp ep (R.Run x s)
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget (MayFirst.First a)) s t a b
  , Bl.IsSymbol sym
  ) =>
  RWSEFn (Preview_ sym g) rp wp sp ep (R.Run x (May.Maybe a)) where
  rwseApply (Preview_ t) _ _ _ _ = do
    v <- mkXFn @rp @wp @sp @ep $ Get t
    pure $ Lens.preview (Bl.barlow @sym) v

data ToArrayOf t = ToArrayOf t

instance rwseApplyToArrayOf ::
  ( RWSEFn (Get g) rp wp sp ep (R.Run x s)
  ) =>
  RWSEFn (ToArrayOf g)
    rp
    wp
    sp
    ep
    ( (Lens.Optic (Forget (Endo.Endo Function (ListT.List a))) s t a b)
      -> R.Run x (Array a)
    ) where
  rwseApply (ToArrayOf t) _ _ _ _ l = do
    v <- mkXFn @rp @wp @sp @ep $ Get t
    pure $ Lens.toArrayOf l v

data ToArrayOf_ :: forall @k. k -> Type -> Type
data ToArrayOf_ b t = ToArrayOf_ t

instance rwseApplyToArrayOf_ ::
  ( RWSEFn (Get g) rp wp sp ep (R.Run x s)
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget (Endo.Endo Function (ListT.List a))) s
      t
      a
      b
  , Bl.IsSymbol sym
  ) =>
  RWSEFn (ToArrayOf_ sym g) rp wp sp ep (R.Run x (Array a)) where
  rwseApply (ToArrayOf_ t) _ _ _ _ = do
    v <- mkXFn @rp @wp @sp @ep $ Get t
    pure $ Lens.toArrayOf (Bl.barlow @sym) v

---------------------- R ------------------------

data AtR = AtR

instance rwseApplyAtR ::
  ( RWSEFn (Get XEnv) rp wp sp ep f
  ) =>
  RWSEFn AtR rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ Get XEnv

data ViewR = ViewR

instance rwseApplyViewR ::
  ( RWSEFn (View XEnv) rp wp sp ep f
  ) =>
  RWSEFn (ViewR) rp wp sp ep f where
  rwseApply (ViewR) _ _ _ _ = mkXFn @rp @wp @sp @ep $ View XEnv

data ViewR_ :: forall @k. k -> Type
data ViewR_ b = ViewR_

instance rwseApplyViewR_ ::
  ( RWSEFn (View_ b XEnv) rp wp sp ep f
  ) =>
  RWSEFn (ViewR_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ View_ @b XEnv

data PreviewR = PreviewR

instance rwseApplyPreviewR ::
  ( RWSEFn (Preview XEnv) rp wp sp ep f
  ) =>
  RWSEFn (PreviewR) rp wp sp ep f where
  rwseApply (PreviewR) _ _ _ _ = mkXFn @rp @wp @sp @ep $ Preview XEnv

data PreviewR_ :: forall @k. k -> Type
data PreviewR_ b = PreviewR_

instance rwseApplyPreviewR_ ::
  ( RWSEFn (Preview_ b XEnv) rp wp sp ep f
  ) =>
  RWSEFn (PreviewR_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ Preview_ @b XEnv

data ToArrayOfR = ToArrayOfR

instance rwseApplyToArrayOfR ::
  ( RWSEFn (ToArrayOf XEnv) rp wp sp ep f
  ) =>
  RWSEFn (ToArrayOfR) rp wp sp ep f where
  rwseApply (ToArrayOfR) _ _ _ _ = mkXFn @rp @wp @sp @ep $ ToArrayOf XEnv

data ToArrayOfR_ :: forall @k. k -> Type
data ToArrayOfR_ b = ToArrayOfR_

instance rwseApplyToArrayOfR_ ::
  ( RWSEFn (ToArrayOf_ b XEnv) rp wp sp ep f
  ) =>
  RWSEFn (ToArrayOfR_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ ToArrayOf_ @b XEnv

---------------------- S ------------------------

data AtS = AtS

instance rwseApplyAtS ::
  ( RWSEFn (Get XState) rp wp sp ep f
  ) =>
  RWSEFn AtS rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ Get XState

data ViewS = ViewS

instance rwseApplyViewS ::
  ( RWSEFn (View XState) rp wp sp ep f
  ) =>
  RWSEFn (ViewS) rp wp sp ep f where
  rwseApply (ViewS) _ _ _ _ = mkXFn @rp @wp @sp @ep $ View XState

data ViewS_ :: forall @k. k -> Type
data ViewS_ b = ViewS_

instance rwseApplyViewS_ ::
  ( RWSEFn (View_ b XState) rp wp sp ep f
  ) =>
  RWSEFn (ViewS_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ View_ @b XState

data PreviewS = PreviewS

instance rwseApplyPreviewS ::
  ( RWSEFn (Preview XState) rp wp sp ep f
  ) =>
  RWSEFn (PreviewS) rp wp sp ep f where
  rwseApply (PreviewS) _ _ _ _ = mkXFn @rp @wp @sp @ep $ Preview XState

data PreviewS_ :: forall @k. k -> Type
data PreviewS_ b = PreviewS_

instance rwseApplyPreviewS_ ::
  ( RWSEFn (Preview_ b XState) rp wp sp ep f
  ) =>
  RWSEFn (PreviewS_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ Preview_ @b XState

data ToArrayOfS = ToArrayOfS

instance rwseApplyToArrayOfS ::
  ( RWSEFn (ToArrayOf XState) rp wp sp ep f
  ) =>
  RWSEFn (ToArrayOfS) rp wp sp ep f where
  rwseApply (ToArrayOfS) _ _ _ _ = mkXFn @rp @wp @sp @ep $ ToArrayOf XState

data ToArrayOfS_ :: forall @k. k -> Type
data ToArrayOfS_ b = ToArrayOfS_

instance rwseApplyToArrayOfS_ ::
  ( RWSEFn (ToArrayOf_ b XState) rp wp sp ep f
  ) =>
  RWSEFn (ToArrayOfS_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ ToArrayOf_ @b XState

data Put = Put

instance rwseApplyPut ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn Put rp wp sp ep (s -> R.Run x Unit) where
  rwseApply _ _ _ sp _ = RunS.putAt sp

data Modify = Modify

instance rwseApplyModify ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn Modify rp wp sp ep ((s -> s) -> R.Run x Unit) where
  rwseApply _ _ _ sp _ = RunS.modifyAt sp

data Set = Set

instance rwseApplySet ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn Set rp wp sp ep (Lens.Optic Function s s a b -> b -> R.Run x Unit) where
  rwseApply _ _ _ sp _ l v = do
    s <- RunS.getAt sp
    RunS.putAt sp $ Lens.set l v s

data Set_ :: forall @k. k -> Type
data Set_ b = Set_

instance rwseApplySet_ ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses Function s s a b
  , Bl.IsSymbol sym
  ) =>
  RWSEFn (Set_ sym) rp wp sp ep (b -> R.Run x Unit) where
  rwseApply _ _ _ sp _ v = do
    s <- RunS.getAt sp
    RunS.putAt sp $ Lens.set (Bl.barlow @sym) v s

data Over = Over

instance rwseApplyOver ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn Over
    rp
    wp
    sp
    ep
    (Lens.Optic Function s s a b -> (a -> b) -> R.Run x Unit) where
  rwseApply _ _ _ sp _ l f = do
    s <- RunS.getAt sp
    RunS.putAt sp $ Lens.over l f s

data Over_ :: forall @k. k -> Type
data Over_ b = Over_

instance rwseApplyOver_ ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses Function s s a b
  , Bl.IsSymbol sym
  ) =>
  RWSEFn (Over_ sym) rp wp sp ep ((a -> b) -> R.Run x Unit) where
  rwseApply _ _ _ sp _ f = do
    s <- RunS.getAt sp
    RunS.putAt sp $ Lens.over (Bl.barlow @sym) f s

data ExecS = ExecS

instance rwseApplyExecS ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn ExecS rp wp sp ep (s -> R.Run x Unit -> R.Run x' s) where
  rwseApply _ _ _ sp _ initState m = RunS.execStateAt sp initState m

data RunS = RunS

instance rwseApplyRunS ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn RunS rp wp sp ep (s -> R.Run x f -> R.Run x' (s TupN./\ f)) where
  rwseApply _ _ _ sp _ initState m = RunS.runStateAt sp initState m

data EvalS = EvalS

instance rwseApplyEvalS ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn EvalS rp wp sp ep (s -> R.Run x f -> R.Run x' f) where
  rwseApply _ _ _ sp _ initState m = RunS.evalStateAt sp initState m

data PlusS :: forall @k. k -> Type
data PlusS sym = PlusS

instance rwseApplyPlusS ::
  ( IsSymbol sp
  , IsSymbol sym
  , Row.Lacks sym r1
  , Cons sym a r1 r2
  , Cons sp (RunS.State { | r1 }) x'' x'
  , Cons sp (RunS.State { | r2 }) x' x
  ) =>
  RWSEFn (PlusS sym) rp wp sp ep (a -> R.Run x f -> R.Run x' f) where
  rwseApply _ _ _ sp _ v m = do
    curr <- RunS.getAt sp
    let next = Rec.insert (Proxy :: Proxy sym) v curr
    (s TupN./\ r) <- RunS.runStateAt sp next m
    RunS.putAt sp (Rec.delete (Proxy :: Proxy sym) s)
    pure r

---------------------- W ------------------------

data Say = Say

instance rwseApplySay ::
  ( IsSymbol wp
  , Cons wp (RunW.Writer (m w)) x' x
  , Monad.Monad m
  ) =>
  RWSEFn Say
    _p
    wp
    _s
    _e
    (w -> R.Run x Unit) where
  rwseApply _ _ wp _ _ = RunW.tellAt wp <<< pure

data Tell = Tell

instance rwseApplyTell ::
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  RWSEFn Tell
    _p
    wp
    _s
    _e
    (w -> R.Run x Unit) where
  rwseApply _ _ wp _ _ = RunW.tellAt wp

data ExecW = ExecW

instance rwseApplyExecW ::
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  RWSEFn ExecW rp wp sp ep (R.Run x Unit -> R.Run x' w) where
  rwseApply _ _ wp _ _ m = RunW.runWriterAt wp m <#> Tup.fst

data RunW = RunW

instance rwseApplyRunW ::
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  RWSEFn RunW rp wp sp ep (R.Run x f -> R.Run x' (w TupN./\ f)) where
  rwseApply _ _ wp _ _ m = RunW.runWriterAt wp m

data EvalW = EvalW

instance rwseApplyEvalW ::
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  RWSEFn EvalW rp wp sp ep (R.Run x f -> R.Run x' f) where
  rwseApply _ _ wp _ _ m = RunW.runWriterAt wp m <#> Tup.snd

---------------------- E ------------------------

data Try = Try

instance rwseApplyTry ::
  ( IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  RWSEFn Try rp wp sp ep (R.Run x a -> R.Run x' (Eor.Either e a)) where
  rwseApply _ _ _ _ ep m = RunE.runExceptAt ep m

--------------------- x -----------------------

type XFnG :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Type
type XFnG rp wp sp ep = forall f o. RWSEFn f rp wp sp ep o => f -> o

mkXFn :: forall @rp @wp @sp @ep. XFnG rp wp sp ep
mkXFn f = rwseApply f (P.Proxy :: P.Proxy rp) (P.Proxy :: P.Proxy wp)
  (P.Proxy :: P.Proxy sp)
  (P.Proxy :: P.Proxy ep)

x :: XFnG "reader" "writer" "state" "except"
x = mkXFn @"reader" @"writer" @"state" @"except"

xAt :: forall @p. XFnG p p p p
xAt = mkXFn @p @p @p @p

class XPSel :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Constraint
class XPSel a b c d | a b c -> d

instance envXPSel :: XPSel XEnv rs ss rs
instance stateXPSel :: XPSel XState rs ss ss

data XEnv = XEnv
data XState = XState

------------------------------------------------------------------

xParser :: forall x s a. s -> Parsing.Parser s a -> R.Run (E Z.ParseError x) a
xParser s pr = xOk $ Z.runParser s pr

--------------- EVAL -------------------------------------------------------

xEval :: forall a. X () a -> a
xEval m = Unsafe.unsafePerformEffect $ R.runBaseEffect $ R.expand $ runXBase m

xExec :: forall e a. X (E e ()) a -> Eor.Either e a
xExec = xEval <<< xTry

xEvalAff :: forall a. X (A ()) a -> Aff.Aff a
xEvalAff m = R.match { aff: \(AffCmd a) -> a } # R.run $ runXBase m

xExecAff :: forall e a. X (EA e ()) a -> Aff.Aff (Eor.Either e a)
xExecAff = xEvalAff <<< xTry

--------------- EDIT ------------------------------------------------------

type Edit s = X (S s ()) Unit

edit :: forall a. a -> Edit a -> a
edit init m = R.extract $ RunS.execState init $
  runXBase m

--------------- W FNS -----------------------------------------------------

xTellMappedHush
  :: forall x e m d w
   . Monad.Monad m
  => ZD.Defaultable d
  => (e -> w)
  -> R.Run (WE (m w) e x) d
  -> R.Run (W (m w) x) d
xTellMappedHush mapW m = xTry m >>= onDone
  where
  onDone (Eor.Left e) = x Say (mapW e) <#> const ZD.default
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
xHush m = (<$>) ZD.orDefault $ xTry m <#> Eor.hush

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

type XWa w fx a = R.Run (XBASE + fx + Wa w ()) a

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
