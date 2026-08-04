module Z.Z.X
  ( A
  , AFF
  , AffF
  , Ask(..)
  , BindE(..)
  , E
  , EA
  , Edit
  , EvalS(..)
  , EvalW(..)
  , ExecS(..)
  , ExecW(..)
  , Fail(..)
  , FromE(..)
  , GGet(..)
  , Get(..)
  , Hush(..)
  , Impure(..)
  , Invert(..)
  , MapE(..)
  , MapW(..)
  , MapWE(..)
  , Modify(..)
  , Ok(..)
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
  , Run'
  , RunAff(..)
  , RunEffA(..)
  , RunEffPromise(..)
  , RunParser(..)
  , RunR(..)
  , RunResult(..)
  , RunS(..)
  , RunW(..)
  , Run_
  , S
  , SA
  , SE
  , SEA
  , Say(..)
  , Set(..)
  , Set_(..)
  , StrW
  , Tell(..)
  , TellMappedHush(..)
  , TellMappedMHush(..)
  , ToArrayOf(..)
  , ToArrayOfR(..)
  , ToArrayOfR_(..)
  , ToArrayOfS(..)
  , ToArrayOfS_(..)
  , ToArrayOf_(..)
  , Try(..)
  , TryUntil(..)
  , Unwrap(..)
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
  , X'
  , XBASE
  , XBaseF
  , XBase_
  , XEnv
  , XPure(..)
  , XState
  , XWa
  , X_
  , class RWSEFn
  , class WpEpPickEp
  , class XPSel
  , class XReturnP
  , edit
  , evalX
  , evalXA
  , joinStrW
  , pureFnX
  , runX
  , runXA
  , runXBase
  , rwseApply
  , x
  , xAt
  , xAtWE
  , xInfo
  , xLogError
  , xLogWarning
  , xOut
  , xOutErr
  , xPass
  , xTimeout
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
import Data.String.Common as StrCommon
import Data.Symbol (class IsSymbol)
import Data.Tuple as Tup
import Data.Tuple.Nested as TupN
import Effect as Eff
import Effect.Aff as Aff
import Effect.Class as EffC
import Effect.Exception as Exc
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

newtype XPure a = XPure (R.Run () a)

instance
  RWSEFn (XPure a) _r _w _s _e (R.Run x a) where
  rwseApply (XPure m) _ _ _ _ = R.expand $ m

pureFnX :: forall i a. (i -> R.Run () a) -> i -> XPure a
pureFnX f i = XPure $ f i

data Impure = Impure

instance
  RWSEFn Impure _r _w _s _e (XPure (X x a) -> X x a) where
  rwseApply Impure _ _ _ _ (XPure m) = do
    xPass
    mm <- R.expand m
    mm

--------------------- R/S -----------------------

data GGet t = GGet t

instance rwseApplyGetR ::
  ( IsSymbol rp
  , Cons rp (RunR.Reader r) x' x
  ) =>
  RWSEFn (GGet XEnv) rp _w _s _e (R.Run x r) where
  rwseApply _ rp _ _ _ = RunR.askAt rp

instance rwseApplyGetS ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn (GGet XState) _r _w sp _e (R.Run x s) where
  rwseApply _ _ _ sp _ = RunS.getAt sp

data View g = View g

instance rwseApplyView ::
  ( RWSEFn (GGet g) rp wp sp ep (R.Run x s)
  ) =>
  RWSEFn (View g) rp wp sp ep ((Lens.Optic (Forget a) s t a b) -> R.Run x a) where
  rwseApply (View t) _ _ _ _ l = do
    v <- mkXFn @rp @wp @sp @ep $ GGet t
    pure $ Lens.view l v

data View_ :: forall @k. k -> Type -> Type
data View_ b t = View_ t

instance rwseApplyView_ ::
  ( RWSEFn (GGet g) rp wp sp ep (R.Run x s)
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget a) s t a b
  , Bl.IsSymbol sym
  ) =>
  RWSEFn (View_ sym g) rp wp sp ep (R.Run x a) where
  rwseApply (View_ t) _ _ _ _ = do
    v <- mkXFn @rp @wp @sp @ep $ GGet t
    pure $ Lens.view (Bl.barlow @sym) v

data Preview g = Preview g

instance rwseApplyPreview ::
  ( RWSEFn (GGet g) rp wp sp ep (R.Run x s)
  ) =>
  RWSEFn (Preview g)
    rp
    wp
    sp
    ep
    ((Lens.Optic (Forget (MayFirst.First a)) s t a b) -> R.Run x (May.Maybe a)) where
  rwseApply (Preview t) _ _ _ _ l = do
    v <- mkXFn @rp @wp @sp @ep $ GGet t
    pure $ Lens.preview l v

data Preview_ :: forall @k. k -> Type -> Type
data Preview_ b t = Preview_ t

instance rwseApplyPreview_ ::
  ( RWSEFn (GGet g) rp wp sp ep (R.Run x s)
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget (MayFirst.First a)) s t a b
  , Bl.IsSymbol sym
  ) =>
  RWSEFn (Preview_ sym g) rp wp sp ep (R.Run x (May.Maybe a)) where
  rwseApply (Preview_ t) _ _ _ _ = do
    v <- mkXFn @rp @wp @sp @ep $ GGet t
    pure $ Lens.preview (Bl.barlow @sym) v

data ToArrayOf t = ToArrayOf t

instance rwseApplyToArrayOf ::
  ( RWSEFn (GGet g) rp wp sp ep (R.Run x s)
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
    v <- mkXFn @rp @wp @sp @ep $ GGet t
    pure $ Lens.toArrayOf l v

data ToArrayOf_ :: forall @k. k -> Type -> Type
data ToArrayOf_ b t = ToArrayOf_ t

instance rwseApplyToArrayOf_ ::
  ( RWSEFn (GGet g) rp wp sp ep (R.Run x s)
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget (Endo.Endo Function (ListT.List a))) s
      t
      a
      b
  , Bl.IsSymbol sym
  ) =>
  RWSEFn (ToArrayOf_ sym g) rp wp sp ep (R.Run x (Array a)) where
  rwseApply (ToArrayOf_ t) _ _ _ _ = do
    v <- mkXFn @rp @wp @sp @ep $ GGet t
    pure $ Lens.toArrayOf (Bl.barlow @sym) v

---------------------- R ------------------------

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

data Ask = Ask

instance rwseApplyAtR ::
  ( RWSEFn (GGet XEnv) rp wp sp ep f
  ) =>
  RWSEFn Ask rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ GGet XEnv

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

data Get = Get

instance rwseApplyAtS ::
  ( RWSEFn (GGet XState) rp wp sp ep f
  ) =>
  RWSEFn Get rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ GGet XState

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

data FromW = FromW

instance
  ( IsSymbol wp
  , IsSymbol baseW
  , Cons wp (RunE.Except e) x'' x'
  , Cons baseW (RunE.Except e) x' x
  , TypeEquals.TypeEquals baseW "writer"
  ) =>
  RWSEFn FromW rp wp sp ep (R.Run x a -> R.Run x' a) where
  rwseApply _ _ _ _ _ m = do
    RunE.runExceptAt (px @baseW) m >>= onDone
    where
    onDone (Eor.Left e) = RunE.throwAt (px @wp) e
    onDone (Eor.Right v) = pure v

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

instance
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

instance
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  RWSEFn ExecW rp wp sp ep (R.Run x Unit -> R.Run x' w) where
  rwseApply _ _ wp _ _ m = RunW.runWriterAt wp m <#> Tup.fst

data RunW = RunW

instance
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  RWSEFn RunW rp wp sp ep (R.Run x f -> R.Run x' (w TupN./\ f)) where
  rwseApply _ _ wp _ _ m = RunW.runWriterAt wp m

data EvalW = EvalW

instance
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  RWSEFn EvalW rp wp sp ep (R.Run x f -> R.Run x' f) where
  rwseApply _ _ wp _ _ m = RunW.runWriterAt wp m <#> Tup.snd

data MapW = MapW

instance
  ( IsSymbol wp
  , Cons wp (RunW.Writer (m w2)) x'' x'
  , Cons wp (RunW.Writer (m w1)) x' x
  , Monoid.Monoid (m w2)
  , Monoid.Monoid (m w1)
  , Monad.Monad m
  ) =>
  RWSEFn MapW rp wp sp ep ((w1 -> w2) -> R.Run x f -> R.Run x' f) where
  rwseApply _ _ wp _ _ f m = do
    (w TupN./\ res) <- RunW.runWriterAt wp m
    RunW.tellAt wp $ map f w
    pure res

---------------------- E ------------------------

data FromE = FromE

instance
  ( IsSymbol ep
  , IsSymbol baseE
  , Cons ep (RunE.Except e) x'' x'
  , Cons baseE (RunE.Except e) x' x
  , TypeEquals.TypeEquals baseE "except"
  ) =>
  RWSEFn FromE rp wp sp ep (R.Run x a -> R.Run x' a) where
  rwseApply _ _ _ _ _ m = do
    RunE.runExceptAt (px @baseE) m >>= onDone
    where
    onDone (Eor.Left e) = RunE.throwAt (px @ep) e
    onDone (Eor.Right v) = pure v

data Try = Try

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  RWSEFn Try rp wp sp ep (R.Run x a -> R.Run x' (Eor.Either e a)) where
  rwseApply _ _ _ _ ep m = RunE.runExceptAt ep m

data Fail = Fail

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  RWSEFn Fail
    _r
    _w
    _s
    ep
    (e -> R.Run x a) where
  rwseApply _ _ _ _ _ e = RunE.throwAt (px @ep) e

data Ok = Ok

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  RWSEFn Ok
    _r
    _w
    _s
    ep
    (Eor.Either e a -> R.Run x a) where
  rwseApply _ _ _ _ _ (Eor.Left e) = RunE.throwAt (px @ep) e
  rwseApply _ _ _ _ _ (Eor.Right a) = pure a

data RunParser = RunParser

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except Z.ParseError) x' x
  ) =>
  RWSEFn RunParser
    _r
    _w
    _s
    ep
    (s -> Parsing.Parser s a -> R.Run x a) where
  rwseApply _ _ _ _ _ s pr = xAt @ep Ok $ Z.runParser s pr

data BindE = BindE

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e2) x'' x'
  , Cons ep (RunE.Except e1) x' x
  ) =>
  RWSEFn BindE rp wp sp ep ((e1 -> R.Run x' f) -> R.Run x f -> R.Run x' f) where
  rwseApply _ _ _ _ _ be m = xAt @ep Try m >>= onDone
    where
    onDone (Eor.Left e) = be e
    onDone (Eor.Right v) = pure v

data MapE = MapE

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e2) x'' x'
  , Cons ep (RunE.Except e1) x' x
  ) =>
  RWSEFn MapE rp wp sp ep ((e1 -> e2) -> R.Run x f -> R.Run x' f) where
  rwseApply _ _ _ _ _ fe m = xAt @ep BindE (xAt @ep Fail <<< fe) m

data Unwrap = Unwrap

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  RWSEFn Unwrap
    _r
    _w
    _s
    ep
    (e -> May.Maybe a -> R.Run x a) where
  rwseApply _ _ _ _ _ _ (May.Just a) = pure a
  rwseApply _ _ _ _ _ e _ = xAt @ep Fail e

data Unwrap' = Unwrap'

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' x
  ) =>
  RWSEFn Unwrap'
    _r
    _w
    _s
    ep
    (May.Maybe a -> R.Run x a) where
  rwseApply _ _ _ _ _ = xAt @ep Unwrap $ Z.jsError' "Nothing#unwrap"

data Hush = Hush

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' x
  , ZD.Defaultable d
  ) =>
  RWSEFn Hush
    _r
    _w
    _s
    ep
    (R.Run x d -> R.Run x' d) where
  rwseApply _ _ _ _ _ m = (<$>) ZD.orDefault $ xAt @ep Try m <#> Eor.hush

data Invert = Invert

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e) x'' x'
  , Cons ep (RunE.Except r) x' x
  ) =>
  RWSEFn Invert rp wp sp ep (R.Run x e -> R.Run x' r) where
  rwseApply _ _ _ _ _ m = xAt @ep Try m <#> Z.invert >>= xAt @ep Ok

data TryUntil = TryUntil

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e) x''' x''
  , Cons ep (RunE.Except r) x'' x'
  , Cons ep (RunE.Except e) x' x
  ) =>
  RWSEFn TryUntil
    _r
    _w
    _s
    ep
    ( R.Run x r
      -> Array (e -> R.Run x r)
      -> R.Run x'' r
    ) where
  rwseApply _ _ _ _ _ try1 tryRest = xAt @ep Invert do
    e1 <- xAt @ep Invert try1
    Z.reduceM (\e tryN -> xAt @ep Invert $ tryN e) e1 tryRest

data RunAff = RunAff

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' (A x)
  ) =>
  RWSEFn RunAff
    _r
    _w
    _s
    ep
    (Aff.Aff f -> R.Run (A x) f) where
  rwseApply _ _ _ _ _ a = do
    res <- aff $ Aff.attempt a
    onDone res
    where
    onDone (Eor.Left e) = xAt @ep Fail $ Z.JsError e
    onDone (Eor.Right v) = pure v

data RunEffA = RunEffA

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' (A x)
  ) =>
  RWSEFn RunEffA
    _r
    _w
    _s
    ep
    (Eff.Effect f -> R.Run (A x) f) where
  rwseApply _ _ _ _ _ eff = do
    res <- aff $ Aff.attempt $ EffC.liftEffect eff
    onDone res
    where
    onDone (Eor.Left e) = xAt @ep Fail $ Z.JsError e
    onDone (Eor.Right v) = pure v

data RunEffPromise = RunEffPromise

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' (A x)
  ) =>
  RWSEFn RunEffPromise
    _r
    _w
    _s
    ep
    (Eff.Effect (Promise.Promise f) -> R.Run (A x) f) where
  rwseApply _ _ _ _ _ = effectPromiseToAff >>> xAt @ep RunAff

-------------------- W/E ----------------------

-- run E at default "except" if only 1 symbol provided
-- use that symbol for the W dimension. this typeclass
-- constraint handles that case

class WpEpPickEp :: Symbol -> Symbol -> Symbol -> Constraint
class WpEpPickEp wp ep ep' | wp ep -> ep'

instance WpEpPickEp wp wp "except"
else instance WpEpPickEp wp ep ep

data RunResult = RunResult

instance
  ( IsSymbol wp
  , IsSymbol ep'
  , WpEpPickEp wp ep ep'
  , Cons wp (RunW.Writer (Array w)) x'' x'
  , Cons ep' (RunE.Except e) x' x
  ) =>
  RWSEFn RunResult rp wp sp ep (R.Run x a -> R.Run x'' (Result w e a)) where
  rwseApply _ _ _ _ _ m = do
    w <- RunW.runWriterAt (px @wp) $ RunE.runExceptAt (px @ep') m
    pure $ { w: (Tup.fst w), v: (Tup.snd w) }

data Unresult = Unresult

instance
  ( IsSymbol wp
  , IsSymbol ep'
  , WpEpPickEp wp ep ep'
  , Cons wp (RunW.Writer (Array w)) x'' x
  , Cons ep' (RunE.Except e) x' x
  ) =>
  RWSEFn Unresult rp wp sp ep (Result w e a -> R.Run x a) where
  rwseApply _ _ _ _ _ { w, v } = do
    xAt @wp Tell w
    xAt @ep' Ok v

data MapWE = MapWE

instance
  ( RWSEFn MapE rp wp sp ep' ((e1 -> e2) -> f'' -> f')
  , RWSEFn MapW rp wp sp ep' ((w1 -> w2) -> f' -> f)
  , WpEpPickEp wp ep ep'
  ) =>
  RWSEFn MapWE rp wp sp ep ((w1 -> w2) -> (e1 -> e2) -> f'' -> f) where
  rwseApply _ _ _ _ _ fw fe m = mkXFn @rp @wp @sp @ep' MapW fw
    $ mkXFn @rp @wp @sp @ep' MapE fe m

data TellMappedHush = TellMappedHush

instance
  ( IsSymbol wp
  , IsSymbol ep'
  , WpEpPickEp wp ep ep'
  , Cons wp (RunW.Writer (m w)) x'' x'
  , Cons ep' (RunE.Except e) x' x
  , Monad.Monad m
  , ZD.Defaultable d
  ) =>
  RWSEFn TellMappedHush rp wp sp ep ((e -> w) -> R.Run x d -> R.Run x' d) where
  rwseApply _ _ _ _ _ mapW m = xAt @ep' Try m >>= onDone
    where
    onDone (Eor.Left e) = xAt @wp Say (mapW e) <#> const ZD.default
    onDone (Eor.Right r) = pure $ r

data TellMappedMHush = TellMappedMHush

instance
  ( IsSymbol wp
  , IsSymbol ep'
  , WpEpPickEp wp ep ep'
  , Cons wp (RunW.Writer (m w)) x'' x'
  , Cons ep' (RunE.Except e) x' x
  , Monad.Monad m
  , Monoid.Monoid (m w)
  , ZD.Defaultable d
  ) =>
  RWSEFn TellMappedMHush rp wp sp ep ((e -> m w) -> R.Run x d -> R.Run x' d) where
  rwseApply _ _ _ _ _ mapW m = xAt @ep' Try m >>= onDone
    where
    onDone (Eor.Left e) = xAt @wp Tell (mapW e) <#> const ZD.default
    onDone (Eor.Right r) = pure $ r

--------------------- x -----------------------

type XFnG :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Type
type XFnG rp wp sp ep = forall f o. RWSEFn f rp wp sp ep o => f -> o

mkXFn :: forall @rp @wp @sp @ep. XFnG rp wp sp ep
mkXFn f = rwseApply f (px @rp) (px @wp) (px @sp) (px @ep)

-- | Runs `class RWSEFn` implementers at the default variant keys:
-- |   "reader" "writer" "state" "except"
x :: XFnG "reader" "writer" "state" "except"
x = mkXFn @"reader" @"writer" @"state" @"except"

-- | Runs `class RWSEFn` implementers primarily at the specified variant key
xAt :: forall @p. XFnG p p p p
xAt = mkXFn @p @p @p @p

-- | Runs `class RWSEFn` implementers with Writer variant key `@wp` and
-- | Except variant key `@ep`
xAtWE :: forall @wp @ep. XFnG wp wp ep ep
xAtWE = mkXFn @wp @wp @ep @ep

class XPSel :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Constraint
class XPSel a b c d | a b c -> d

instance envXPSel :: XPSel XEnv rs ss rs
instance stateXPSel :: XPSel XState rs ss ss

data XEnv = XEnv
data XState = XState

--------------- EVAL -------------------------------------------------------

evalX :: forall a. X () a -> a
evalX m = Unsafe.unsafePerformEffect $ R.runBaseEffect $ R.expand $ runXBase m

runX :: forall e a. X (E e ()) a -> Eor.Either e a
runX = evalX <<< x Try

evalXA :: forall a. X (A ()) a -> Aff.Aff a
evalXA m = R.match { aff: \(AffCmd a) -> a } # R.run $ runXBase m

runXA :: forall e a. X (EA e ()) a -> Aff.Aff (Eor.Either e a)
runXA = evalXA <<< x Try

--------------- OTHER ------------------------------------------------------

type Edit s = X (S s ()) Unit

edit :: forall a. a -> Edit a -> a
edit init m = R.extract $ RunS.execState init $
  runXBase m

type StrW = X (Wa String ()) Unit

joinStrW :: String -> StrW -> String
joinStrW s m = StrCommon.joinWith s $ evalX $ x ExecW m

--------------- E FNS -----------------------------------------------------

type Result w e a = { w :: (Array w), v :: (Eor.Either e a) }

--------------- A FNS -----------------------------------------------------

foreign import js_timeout :: Int -> Eff.Effect (Promise.Promise Unit)

promiseToAff :: forall a. Promise.Promise a -> Aff.Aff a
promiseToAff = Promise.toAff

effectPromiseToAff :: forall a. Eff.Effect (Promise.Promise a) -> Aff.Aff a
effectPromiseToAff e = EffC.liftEffect e >>= promiseToAff

xTimeout :: forall x. Int -> X (A x) Unit
xTimeout ms = Z.fDiscard $ x Try $ x RunEffPromise $ js_timeout ms

--------------- CORE TYPE ---------------------------------------------------

type X x a = R.Run (XBASE x) a

type X' x = X x Unit

type X_ a = X () a

type Run' x = R.Run x Unit

type Run_ a = R.Run () a

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

data XBaseF a
  = PassCmd a
  | LogCmd String String Z.JsAny a
  | LogDirectCmd String Z.JsAny a

derive instance functorXBaseF :: Functor XBaseF

type XBASE x = (xBase :: XBaseF | x)

type XBase_ = "xBase"

_eff = P.Proxy :: P.Proxy XBase_

handleXBase :: forall r. XBaseF ~> R.Run r
handleXBase = case _ of
  PassCmd a -> pure a
  LogCmd k src v e -> do
    pure $ Unsafe.unsafePerformEffect $ js_consoleFn k src [ v ]
    pure e
  LogDirectCmd k v e -> do
    pure $ Unsafe.unsafePerformEffect $ js_consoleDirectFn k v
    pure e

runXBase :: forall r. R.Run (XBASE + r) ~> R.Run r
runXBase = R.run (R.on _eff handleXBase R.send)

xPass :: forall x. X x Unit
xPass = R.lift _eff (PassCmd unit)

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
