module Z.Z.X
  ( A
  , AFF
  , AffF
  , Ask
  , BindE
  , E
  , EA
  , Edit
  , EvalS
  , EvalW
  , ExecS
  , ExecW
  , Fail
  , FromE
  , GGet
  , Get
  , Hush
  , Invert
  , MapE
  , MapW
  , MapWE
  , Modify
  , Ok
  , Over
  , Over_
  , PlusS
  , Preview
  , PreviewR
  , PreviewR_
  , PreviewS
  , PreviewS_
  , Preview_
  , Put
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
  , RunAff
  , RunEffA
  , RunEffPromise
  , RunParser
  , RunR
  , RunResult
  , RunS
  , RunW
  , Run_
  , S
  , SA
  , SE
  , SEA
  , Say
  , Set
  , Set_
  , StrW
  , Tell
  , TellMappedHush
  , TellMappedMHush
  , ToArrayOf
  , ToArrayOfR
  , ToArrayOfR_
  , ToArrayOfS
  , ToArrayOfS_
  , ToArrayOf_
  , Try
  , TryUntil
  , Unwrap
  , View
  , ViewR
  , ViewR_
  , ViewS
  , ViewS_
  , View_
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
  , WithReturn
  , X
  , X'
  , XApply
  , XAt(..)
  , XBASE
  , XBaseF
  , XBase_
  , XEnv
  , XPure(..)
  , XState
  , XWa
  , X_
  , Xwe(..)
  , class Cons0
  , class DimensionedVal
  , class DimensionedValTag
  , class E_
  , class ParseRootTagParts
  , class ParseRootTagPartsImpl
  , class RWSEFn
  , class R_
  , class ReturnP_
  , class RevSym
  , class RootDimensionedValueTag
  , class S_
  , class SplitSp1
  , class UpCat
  , class UpCf
  , class UpCt
  , class W_
  , class WpEpPickEp
  , class XPSel
  , class XReturnP
  , class XTLS
  , class XTLSFull
  , class XTLSRFull
  , class XTLSSFull
  , class XTLSunAt
  , cons0
  , edit
  , evalX
  , evalXA
  , eval_
  , joinStrW
  , mkDim
  , mkDimAt
  , mkDimWE
  , mkDimensional
  , pureFnX
  , runX
  , runXA
  , runXBase
  , rwseApply
  , x
  , x'
  , xAtWE
  , xImpure
  , xInfo
  , xLogError
  , xLogWarning
  , xOut
  , xOutErr
  , xPass
  , xPure
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
import Prim.Symbol as Symbol
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

class DimensionedValTag tagIn tagOut | tagIn -> tagOut

class RootDimensionedValueTag tagIn tagOut | tagIn -> tagOut

class ParseRootTagPartsImpl w1 w2 tagOut | w1 w2 -> tagOut

instance ParseRootTagPartsImpl "~" t (Over_ t)

class ParseRootTagParts tagIn tagOut | tagIn -> tagOut

instance
  ( SplitSp1 tagIn w1 w2
  , ParseRootTagPartsImpl w1 w2 tagOut
  , IsSymbol tagIn
  ) =>
  ParseRootTagParts tagIn tagOut
else instance ParseRootTagParts ti to

instance RootDimensionedValueTag "fail" Fail
else instance RootDimensionedValueTag "^" (GGet XEnv)
else instance RootDimensionedValueTag "-" (GGet XState)
else instance RootDimensionedValueTag "-~" Set
else instance RootDimensionedValueTag "-%" Over
else instance RootDimensionedValueTag "-" (GGet XState)
else instance (DimensionedValTag ti to) => RootDimensionedValueTag ti to

class
  DimensionedValTag tag tag <=
  DimensionedVal tag dspec t
  | dspec -> t where
  mkDimensional :: P.Proxy tag -> P.Proxy dspec -> t

mkDim
  :: forall @tt tag t
   . DimensionedVal tag Void t
  => RootDimensionedValueTag tt tag
  => t
mkDim = mkDimensional (P.Proxy :: P.Proxy tag) (P.Proxy :: P.Proxy Void)

mkDimAt
  :: forall @at @tt tag t
   . DimensionedVal tag (XAt at) t
  => RootDimensionedValueTag tt tag
  => t
mkDimAt = mkDimensional (P.Proxy :: P.Proxy tag)
  (P.Proxy :: P.Proxy (XAt at))

mkDimWE
  :: forall @wp @ep @tt tag t
   . DimensionedVal tag (Xwe wp ep) t
  => RootDimensionedValueTag tt tag
  => t
mkDimWE = mkDimensional (P.Proxy :: P.Proxy tag)
  (P.Proxy :: P.Proxy (Xwe wp ep))

data XAt at = XAt
data Xwe atw ate = Xwe

class R_ i o | i -> o
class W_ i o | i -> o
class S_ i o | i -> o
class E_ i o | i -> o

instance R_ (XAt t) t
else instance R_ t "reader"

instance W_ (Xwe w e) w
else instance W_ (XAt t) t
else instance W_ t "writer"

instance S_ (XAt t) t
else instance S_ t "state"

instance E_ (Xwe w e) e
else instance E_ (XAt t) t
else instance E_ t "except"

class Cons0 t where
  cons0 :: t

instance Cons0 BindE where
  cons0 = BindE

instance Cons0 EvalS where
  cons0 = EvalS

instance Cons0 EvalW where
  cons0 = EvalW

instance Cons0 ExecS where
  cons0 = ExecS

instance Cons0 ExecW where
  cons0 = ExecW

instance Cons0 Fail where
  cons0 = Fail

instance Cons0 FromE where
  cons0 = FromE

instance Cons0 Hush where
  cons0 = Hush

instance Cons0 Invert where
  cons0 = Invert

instance Cons0 MapE where
  cons0 = MapE

instance Cons0 MapW where
  cons0 = MapW

instance Cons0 MapWE where
  cons0 = MapWE

instance Cons0 Say where
  cons0 = Say

instance Cons0 RunW where
  cons0 = RunW

instance Cons0 RunS where
  cons0 = RunS

instance Cons0 RunResult where
  cons0 = RunResult

instance Cons0 RunR where
  cons0 = RunR

instance Cons0 RunParser where
  cons0 = RunParser

instance Cons0 RunEffPromise where
  cons0 = RunEffPromise

instance Cons0 RunEffA where
  cons0 = RunEffA

instance Cons0 RunAff where
  cons0 = RunAff

instance Cons0 Put where
  cons0 = Put

instance Cons0 (PlusS t) where
  cons0 = PlusS

instance Cons0 Ok where
  cons0 = Ok

instance Cons0 Modify where
  cons0 = Modify

instance Cons0 Get where
  cons0 = Get

instance Cons0 Set where
  cons0 = Set

instance Cons0 Ask where
  cons0 = Ask

instance Cons0 Over where
  cons0 = Over

instance Cons0 ViewR where
  cons0 = ViewR

instance Cons0 ViewS where
  cons0 = ViewS

instance Cons0 PreviewR where
  cons0 = PreviewR

instance Cons0 PreviewS where
  cons0 = PreviewS

instance Cons0 ToArrayOfR where
  cons0 = ToArrayOfR

instance Cons0 ToArrayOfS where
  cons0 = ToArrayOfS

instance Cons0 Try where
  cons0 = Try

instance Cons0 TryUntil where
  cons0 = TryUntil

instance Cons0 WithReturn where
  cons0 = WithReturn

instance Cons0 Tell where
  cons0 = Tell

instance Cons0 TellMappedHush where
  cons0 = TellMappedHush

instance Cons0 TellMappedMHush where
  cons0 = TellMappedMHush

instance Cons0 Unwrap where
  cons0 = Unwrap

instance Cons0 (ViewR_ t) where
  cons0 = ViewR_

instance Cons0 (ViewS_ t) where
  cons0 = ViewS_

instance Cons0 (PreviewR_ t) where
  cons0 = PreviewR_

instance Cons0 (PreviewS_ t) where
  cons0 = PreviewS_

instance Cons0 (ToArrayOfR_ t) where
  cons0 = ToArrayOfR_

instance Cons0 (ToArrayOfS_ t) where
  cons0 = ToArrayOfS_

instance Cons0 (Over_ t) where
  cons0 = Over_

instance Cons0 (Set_ t) where
  cons0 = Set_

instance Cons0 XApply where
  cons0 = XApply

class XTLS
  :: forall k1
   . Symbol
  -> k1
  -> Constraint
class XTLS sym f | sym -> f

instance XTLS "get" Get
else instance XTLS "$" XApply
else instance XTLS "set" Set
else instance XTLS "evalS" EvalS
else instance XTLS "evalW" EvalW
else instance XTLS "execS" ExecS
else instance XTLS "execW" ExecW
else instance XTLS "fail" Fail
else instance XTLS "fromE" FromE
else instance XTLS "hush" Hush
else instance XTLS "invert" Invert
else instance XTLS "mapE" MapE
else instance XTLS "mapW" MapW
else instance XTLS "mapWE" MapWE
else instance XTLS "set" Set
else instance XTLS "modify" Modify
else instance XTLS "ok" Ok
else instance XTLS "put" Put
else instance XTLS "runAff" RunAff
else instance XTLS "runEffA" RunEffA
else instance XTLS "runEffPromise" RunEffPromise
else instance XTLS "runParser" RunParser
else instance XTLS "runR" RunR
else instance XTLS "runResult" RunResult
else instance XTLS "runS" RunS
else instance XTLS "runW" RunW
else instance XTLS "say" Say
else instance XTLS "over" Over
else instance XTLS "ask" Ask
else instance XTLS "viewR" ViewR
else instance XTLS "viewS" ViewS
else instance XTLS "previewR" PreviewR
else instance XTLS "previewS" PreviewS
else instance XTLS "toArrayOfR" ToArrayOfR
else instance XTLS "toArrayOfS" ToArrayOfS
else instance XTLS "try" Try
else instance XTLS "tryUntil" TryUntil
else instance XTLS "withReturn" WithReturn
else instance XTLS "tell" Tell
else instance XTLS "tellMappedHush" TellMappedHush
else instance XTLS "unwrap" Unwrap
else instance XTLS "tellMappedMHush" TellMappedMHush
else instance
  ( Symbol.Cons sh stail s
  , XTLSFull sh stail f
  ) =>
  XTLS s f

x'
  :: forall @sym o f
   . Cons0 f
  => XTLS sym f
  => RWSEFn f "reader" "writer" "state" "except" o
  => o
x' = rwseApply (cons0 :: f) (px @"reader") (px @"writer") (px @"state")
  (px @"except")

x
  :: forall @pp @sym f o
   . Cons0 f
  => XTLS sym f
  => RWSEFn f pp pp pp pp o
  => o
x = rwseApply (cons0 :: f) (px @pp) (px @pp) (px @pp) (px @pp)

class XTLSFull sh stail f | sh stail -> f

instance
  XTLSFull "%" rest (Over_ rest)
else instance
  XTLSFull "~" rest (Set_ rest)
else instance
  ( Symbol.Cons rest1 rest' rest
  , XTLSRFull rest1 rest' f
  ) =>
  XTLSFull "^" rest f
else instance
  ( Symbol.Cons rest1 rest' rest
  , XTLSSFull rest1 rest' f
  ) =>
  XTLSFull "-" rest f

class XTLSRFull sh stail f | sh stail -> f

instance XTLSRFull "." rest (ViewR_ rest)
else instance XTLSRFull "?" rest (PreviewR_ rest)
else instance XTLSRFull "*" rest (ToArrayOfR_ rest)

class XTLSSFull sh stail f | sh stail -> f

instance XTLSSFull "." rest (ViewS_ rest)
else instance XTLSSFull "?" rest (PreviewS_ rest)
else instance XTLSSFull "*" rest (ToArrayOfS_ rest)
else instance XTLSSFull "+" rest (PlusS rest)

class SplitSp1 i o1 o2 | i -> o1 o2

instance (XTLSunAt i "" "" "f" o1 o2) => SplitSp1 i o1 o2

class XTLSunAt sym cat cf ct tat tf | sym cat cf ct -> tat tf

class UpCat c cat ct cat' | c cat ct -> cat'

instance UpCat " " cat ct cat
else instance UpCat c cat "t" cat
else instance (Symbol.Cons c cat cat') => UpCat c cat "f" cat'

class UpCf c cf ct cf' | c cf ct -> cf'

instance UpCf c cf "f" cf
else instance (Symbol.Cons c cf cf') => UpCf c cf "t" cf'

class UpCt c ct ct' | c ct -> ct'

instance UpCt " " ct "t"
else instance UpCt c ct ct

class RevSym s c s' | s c -> s'

instance RevSym "" c c
else instance
  ( Symbol.Cons c1 s' s
  , Symbol.Cons c1 c c'
  , RevSym s' c' f
  ) =>
  RevSym s c f

instance
  ( RevSym cf "" tf
  , RevSym cat "" tat
  ) =>
  XTLSunAt "" cat cf "t" tat tf
else instance
  ( Symbol.Cons c s' s
  , UpCat c cat ct cat'
  , UpCf c cf ct cf'
  , UpCt c ct ct'
  , XTLSunAt s' cat' cf' ct' tat tf
  ) =>
  XTLSunAt s cat cf ct tat tf

opViewR_
  :: forall @sym lhsI o
   . IsSymbol sym
  => ((lhsI -> ViewR_ sym) -> o)
  -> o
opViewR_ f = f (\_ -> ViewR_ @sym)

infixr 0 opViewR_ as ^@

opPreviewR_
  :: forall @sym lhsI o
   . IsSymbol sym
  => ((lhsI -> PreviewR_ sym) -> o)
  -> o
opPreviewR_ f = f (\_ -> PreviewR_ @sym)

infixr 0 opPreviewR_ as ^?@

opToArrayOfR_
  :: forall @sym lhsI o
   . IsSymbol sym
  => ((lhsI -> ToArrayOfR_ sym) -> o)
  -> o
opToArrayOfR_ f = f (\_ -> ToArrayOfR_ @sym)

infixr 0 opToArrayOfR_ as ^*@

opViewS_
  :: forall @sym lhsI o
   . IsSymbol sym
  => ((lhsI -> ViewS_ sym) -> o)
  -> o
opViewS_ f = f (\_ -> ViewS_ @sym)

infixr 0 opViewS_ as -@

opPreviewS_
  :: forall @sym lhsI o
   . IsSymbol sym
  => ((lhsI -> PreviewS_ sym) -> o)
  -> o
opPreviewS_ f = f (\_ -> PreviewS_ @sym)

infixr 0 opPreviewS_ as -?@

opToArrayOfS_
  :: forall @sym lhsI o
   . IsSymbol sym
  => ((lhsI -> ToArrayOfS_ sym) -> o)
  -> o
opToArrayOfS_ f = f (\_ -> ToArrayOfS_ @sym)

infixr 0 opToArrayOfS_ as -*@

opOver_
  :: forall @sym lhsI rhs o
   . IsSymbol sym
  => ((lhsI -> Over_ sym) -> rhs -> o)
  -> rhs
  -> o
opOver_ f rhs = f (\_ -> Over_ @sym) rhs

infixr 0 opOver_ as @%

opSet_
  :: forall @sym lhsI rhs o
   . IsSymbol sym
  => ((lhsI -> Set_ sym) -> rhs -> o)
  -> rhs
  -> o
opSet_ f rhs = f (\_ -> Set_ @sym) rhs

infixr 0 opSet_ as @~

px :: forall @k. P.Proxy k
px = P.Proxy

class RWSEFn
  :: forall k1 k2 k3 k4. Type -> k1 -> k2 -> k3 -> k4 -> Type -> Constraint
class RWSEFn f rp wp sp ep o | f rp wp sp ep -> o where
  rwseApply :: f -> P.Proxy rp -> P.Proxy wp -> P.Proxy sp -> P.Proxy ep -> o

-------------------- OTHER ----------------------

data WithReturn = WithReturn

class ReturnP_ pdesc p | pdesc -> p

instance ReturnP_ (XAt t) t
else instance ReturnP_ t "(x)::earlyReturn"

class XReturnP rp wp sp ep fp | rp wp sp ep -> fp

instance XReturnP rp wp sp ep "earlyReturn"
else instance XReturnP _r _w _s ep ep

instance DimensionedValTag WithReturn WithReturn

instance
  ( ReturnP_ dspec pp
  , IsSymbol pp
  , Cons pp (RunE.Except r) x' x
  ) =>
  DimensionedVal WithReturn
    dspec
    (((r -> R.Run x Unit) -> R.Run x r) -> R.Run x' r) where
  mkDimensional _ _ m = RunE.runExceptAt (px @pp) (m return) >>= onRes
    where
    return = RunE.throwAt (px @pp)
    onRes (Eor.Left ret) = pure ret
    onRes (Eor.Right ret) = pure ret

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

newtype XPure a = XPure (X () a)

instance
  RWSEFn (XPure a) _r _w _s _e (X x a) where
  rwseApply (XPure m) _ _ _ _ = xPass *> (pure $ evalX m)

pureFnX :: forall i a. (i -> X () a) -> i -> XPure a
pureFnX f i = XPure $ f i

xImpure :: forall x a. XPure (X x a) -> X x a
xImpure (XPure m) = xPass *> R.expand m >>= identity

xPure :: forall x a. XPure a -> R.Run x a
xPure (XPure m) = runXBase $ xPass *> R.expand m

data XApply = XApply

instance
  ( RWSEFn i rp wp sp ep o
  ) =>
  RWSEFn XApply rp wp sp ep (i -> o) where
  rwseApply _ rp wp sp ep i = rwseApply i rp wp sp ep

--------------------- R/S -----------------------

data GGet t = GGet t

instance DimensionedValTag (GGet t) (GGet t)

instance
  ( R_ dspec rp
  , IsSymbol rp
  , Cons rp (RunR.Reader r) x' x
  ) =>
  DimensionedVal (GGet XEnv) dspec (R.Run x r) where
  mkDimensional _ _ = RunR.askAt (px @rp)

instance
  ( S_ dspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  DimensionedVal (GGet XState) dspec (R.Run x s) where
  mkDimensional _ _ = RunS.getAt (px @sp)

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

instance DimensionedValTag (View g) (View g)

instance
  ( DimensionedVal (GGet g) dspec (R.Run x s)
  ) =>
  DimensionedVal (View g) dspec ((Lens.Optic (Forget a) s t a b) -> R.Run x a) where
  mkDimensional _ _ l = do
    v <- mkDimensional (px @(GGet g)) (px @dspec)
    pure $ Lens.view l v

instance rwseApplyView ::
  ( RWSEFn (GGet g) rp wp sp ep (R.Run x s)
  ) =>
  RWSEFn (View g) rp wp sp ep ((Lens.Optic (Forget a) s t a b) -> R.Run x a) where
  rwseApply (View t) _ _ _ _ l = do
    v <- mkXFn @rp @wp @sp @ep $ GGet t
    pure $ Lens.view l v

data View_ :: forall @k. k -> Type -> Type
data View_ sym g = View_ g

instance DimensionedValTag (View_ sym g) (View_ sym g)

instance
  ( DimensionedVal (GGet g) dspec (R.Run x s)
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget a) s t a b
  , Bl.IsSymbol sym
  ) =>
  DimensionedVal (View_ sym g) dspec (R.Run x a) where
  mkDimensional _ _ = do
    v <- mkDimensional (px @(GGet g)) (px @dspec)
    pure $ Lens.view (Bl.barlow @sym) v

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

instance DimensionedValTag (Preview g) (Preview g)

instance
  ( DimensionedVal (GGet g) dspec (R.Run x s)
  ) =>
  DimensionedVal (Preview g)
    dspec
    ((Lens.Optic (Forget (MayFirst.First a)) s t a b) -> R.Run x (May.Maybe a)) where
  mkDimensional _ _ l = do
    v <- mkDimensional (px @(GGet g)) (px @dspec)
    pure $ Lens.preview l v

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

instance DimensionedValTag (Preview_ sym g) (Preview_ sym g)

instance
  ( DimensionedVal (GGet g) dspec (R.Run x s)
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget (MayFirst.First a)) s t a b
  , Bl.IsSymbol sym
  ) =>
  DimensionedVal (Preview_ sym g) dspec (R.Run x (May.Maybe a)) where
  mkDimensional _ _ = do
    v <- mkDimensional (px @(GGet g)) (px @dspec)
    pure $ Lens.preview (Bl.barlow @sym) v

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

instance DimensionedValTag (ToArrayOf g) (ToArrayOf g)

instance
  ( DimensionedVal (GGet g) dspec (R.Run x s)
  ) =>
  DimensionedVal (ToArrayOf g)
    dspec
    ( (Lens.Optic (Forget (Endo.Endo Function (ListT.List a))) s t a b)
      -> R.Run x (Array a)
    ) where
  mkDimensional _ _ l = do
    v <- mkDimensional (px @(GGet g)) (px @dspec)
    pure $ Lens.toArrayOf l v

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

instance DimensionedValTag (ToArrayOf_ sym g) (ToArrayOf_ sym g)

instance
  ( DimensionedVal (GGet g) dspec (R.Run x s)
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget (Endo.Endo Function (ListT.List a))) s
      t
      a
      b
  , Bl.IsSymbol sym
  ) =>
  DimensionedVal (ToArrayOf_ sym g) dspec (R.Run x (Array a)) where
  mkDimensional _ _ = do
    v <- mkDimensional (px @(GGet g)) (px @dspec)
    pure $ Lens.toArrayOf (Bl.barlow @sym) v

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

instance DimensionedValTag RunR RunR

instance
  ( R_ dspec rp
  , IsSymbol rp
  , Cons rp (RunR.Reader r) x' x
  ) =>
  DimensionedVal RunR dspec (r -> R.Run x a -> R.Run x' a) where
  mkDimensional _ _ = RunR.runReaderAt (P.Proxy :: P.Proxy rp)

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

instance DimensionedValTag Ask (GGet XEnv)

instance rwseApplyAtR ::
  ( RWSEFn (GGet XEnv) rp wp sp ep f
  ) =>
  RWSEFn Ask rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ GGet XEnv

data ViewR = ViewR

instance DimensionedValTag ViewR (View XEnv)

instance rwseApplyViewR ::
  ( RWSEFn (View XEnv) rp wp sp ep f
  ) =>
  RWSEFn (ViewR) rp wp sp ep f where
  rwseApply (ViewR) _ _ _ _ = mkXFn @rp @wp @sp @ep $ View XEnv

data ViewR_ :: forall @k. k -> Type
data ViewR_ b = ViewR_

instance DimensionedValTag (ViewR_ b) (View_ b XEnv)

instance rwseApplyViewR_ ::
  ( RWSEFn (View_ b XEnv) rp wp sp ep f
  ) =>
  RWSEFn (ViewR_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ View_ @b XEnv

data PreviewR = PreviewR

instance DimensionedValTag PreviewR (Preview XEnv)

instance rwseApplyPreviewR ::
  ( RWSEFn (Preview XEnv) rp wp sp ep f
  ) =>
  RWSEFn (PreviewR) rp wp sp ep f where
  rwseApply (PreviewR) _ _ _ _ = mkXFn @rp @wp @sp @ep $ Preview XEnv

data PreviewR_ :: forall @k. k -> Type
data PreviewR_ b = PreviewR_

instance DimensionedValTag (PreviewR_ b) (Preview_ b XEnv)

instance rwseApplyPreviewR_ ::
  ( RWSEFn (Preview_ b XEnv) rp wp sp ep f
  ) =>
  RWSEFn (PreviewR_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ Preview_ @b XEnv

data ToArrayOfR = ToArrayOfR

instance DimensionedValTag ToArrayOfR (ToArrayOf XEnv)

instance rwseApplyToArrayOfR ::
  ( RWSEFn (ToArrayOf XEnv) rp wp sp ep f
  ) =>
  RWSEFn (ToArrayOfR) rp wp sp ep f where
  rwseApply (ToArrayOfR) _ _ _ _ = mkXFn @rp @wp @sp @ep $ ToArrayOf XEnv

data ToArrayOfR_ :: forall @k. k -> Type
data ToArrayOfR_ b = ToArrayOfR_

instance DimensionedValTag (ToArrayOfR_ b) (ToArrayOf_ b XEnv)

instance rwseApplyToArrayOfR_ ::
  ( RWSEFn (ToArrayOf_ b XEnv) rp wp sp ep f
  ) =>
  RWSEFn (ToArrayOfR_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ ToArrayOf_ @b XEnv

---------------------- S ------------------------

data Get = Get

instance DimensionedValTag Get (GGet XState)

instance rwseApplyAtS ::
  ( RWSEFn (GGet XState) rp wp sp ep f
  ) =>
  RWSEFn Get rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ GGet XState

data ViewS = ViewS

instance DimensionedValTag ViewS (View XState)

instance rwseApplyViewS ::
  ( RWSEFn (View XState) rp wp sp ep f
  ) =>
  RWSEFn (ViewS) rp wp sp ep f where
  rwseApply (ViewS) _ _ _ _ = mkXFn @rp @wp @sp @ep $ View XState

data ViewS_ :: forall @k. k -> Type
data ViewS_ b = ViewS_

instance DimensionedValTag (ViewS_ b) (View_ b XState)

instance rwseApplyViewS_ ::
  ( RWSEFn (View_ b XState) rp wp sp ep f
  ) =>
  RWSEFn (ViewS_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ View_ @b XState

data PreviewS = PreviewS

instance DimensionedValTag PreviewS (Preview XState)

instance rwseApplyPreviewS ::
  ( RWSEFn (Preview XState) rp wp sp ep f
  ) =>
  RWSEFn (PreviewS) rp wp sp ep f where
  rwseApply (PreviewS) _ _ _ _ = mkXFn @rp @wp @sp @ep $ Preview XState

data PreviewS_ :: forall @k. k -> Type
data PreviewS_ b = PreviewS_

instance DimensionedValTag (PreviewS_ b) (Preview_ b XState)

instance rwseApplyPreviewS_ ::
  ( RWSEFn (Preview_ b XState) rp wp sp ep f
  ) =>
  RWSEFn (PreviewS_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ Preview_ @b XState

data ToArrayOfS = ToArrayOfS

instance DimensionedValTag ToArrayOfS (ToArrayOf XState)

instance rwseApplyToArrayOfS ::
  ( RWSEFn (ToArrayOf XState) rp wp sp ep f
  ) =>
  RWSEFn (ToArrayOfS) rp wp sp ep f where
  rwseApply (ToArrayOfS) _ _ _ _ = mkXFn @rp @wp @sp @ep $ ToArrayOf XState

data ToArrayOfS_ :: forall @k. k -> Type
data ToArrayOfS_ b = ToArrayOfS_

instance DimensionedValTag (ToArrayOfS_ b) (ToArrayOf_ b XState)

instance rwseApplyToArrayOfS_ ::
  ( RWSEFn (ToArrayOf_ b XState) rp wp sp ep f
  ) =>
  RWSEFn (ToArrayOfS_ b) rp wp sp ep f where
  rwseApply _ _ _ _ _ = mkXFn @rp @wp @sp @ep $ ToArrayOf_ @b XState

data Put = Put

instance DimensionedValTag Put Put

instance
  ( S_ dspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  DimensionedVal Put dspec (s -> R.Run x Unit) where
  mkDimensional _ _ = RunS.putAt (px @sp)

instance rwseApplyPut ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn Put rp wp sp ep (s -> R.Run x Unit) where
  rwseApply _ _ _ sp _ = RunS.putAt sp

data Modify = Modify

instance DimensionedValTag Modify Modify

instance
  ( S_ dspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  DimensionedVal Modify dspec ((s -> s) -> R.Run x Unit) where
  mkDimensional _ _ = RunS.modifyAt (px @sp)

instance rwseApplyModify ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn Modify rp wp sp ep ((s -> s) -> R.Run x Unit) where
  rwseApply _ _ _ sp _ = RunS.modifyAt sp

data Set = Set

instance DimensionedValTag Set Set

instance
  ( S_ dspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  DimensionedVal Set
    dspec
    (Lens.Optic Function s s a b -> b -> R.Run x Unit) where
  mkDimensional _ _ l v = do
    s <- RunS.getAt (px @sp)
    RunS.putAt (px @sp) $ Lens.set l v s

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

instance DimensionedValTag (Set_ sym) (Set_ sym)

instance
  ( S_ dspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses Function s s a b
  , Bl.IsSymbol sym
  ) =>
  DimensionedVal (Set_ sym) dspec (b -> R.Run x Unit) where
  mkDimensional _ _ v = do
    s <- RunS.getAt (px @sp)
    RunS.putAt (px @sp) $ Lens.set (Bl.barlow @sym) v s

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

instance DimensionedValTag Over Over

instance
  ( S_ dspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  DimensionedVal Over
    dspec
    (Lens.Optic Function s s a b -> (a -> b) -> R.Run x Unit) where
  mkDimensional _ _ l f = do
    s <- RunS.getAt (px @sp)
    RunS.putAt (px @sp) $ Lens.over l f s

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
data Over_ sym = Over_

instance DimensionedValTag (Over_ sym) (Over_ sym)

instance
  ( S_ dspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses Function s s a b
  , Bl.IsSymbol sym
  ) =>
  DimensionedVal (Over_ sym) dspec ((a -> b) -> R.Run x Unit) where
  mkDimensional _ _ f = do
    s <- RunS.getAt (px @sp)
    RunS.putAt (px @sp) $ Lens.over (Bl.barlow @sym) f s

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

instance DimensionedValTag ExecS ExecS

instance
  ( S_ dspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  DimensionedVal ExecS dspec (s -> R.Run x f -> R.Run x' s) where
  mkDimensional _ _ initState m = RunS.execStateAt (px @sp) initState m

instance rwseApplyExecS ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn ExecS rp wp sp ep (s -> R.Run x Unit -> R.Run x' s) where
  rwseApply _ _ _ sp _ initState m = RunS.execStateAt sp initState m

data RunS = RunS

instance DimensionedValTag RunS RunS

instance
  ( S_ dspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  DimensionedVal RunS dspec (s -> R.Run x f -> R.Run x' (s TupN./\ f)) where
  mkDimensional _ _ initState m = RunS.runStateAt (px @sp) initState m

instance rwseApplyRunS ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn RunS rp wp sp ep (s -> R.Run x f -> R.Run x' (s TupN./\ f)) where
  rwseApply _ _ _ sp _ initState m = RunS.runStateAt sp initState m

data EvalS = EvalS

instance DimensionedValTag EvalS EvalS

instance mkdEvalS ::
  ( S_ dspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  DimensionedVal EvalS dspec (s -> R.Run x f -> R.Run x' f) where
  mkDimensional _ _ initState m = RunS.evalStateAt (px @sp) initState m

instance rwseApplyEvalS ::
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  ) =>
  RWSEFn EvalS rp wp sp ep (s -> R.Run x f -> R.Run x' f) where
  rwseApply _ _ _ sp _ initState m = RunS.evalStateAt sp initState m

data PlusS :: forall @k. k -> Type
data PlusS sym = PlusS

instance DimensionedValTag (PlusS sym) (PlusS sym)

instance
  ( S_ dspec sp
  , IsSymbol sp
  , IsSymbol sym
  , Row.Lacks sym r1
  , Cons sym a r1 r2
  , Cons sp (RunS.State { | r1 }) x'' x'
  , Cons sp (RunS.State { | r2 }) x' x
  ) =>
  DimensionedVal (PlusS sym) dspec (a -> R.Run x f -> R.Run x' f) where
  mkDimensional _ _ v m = do
    curr <- RunS.getAt (px @sp)
    let next = Rec.insert (Proxy :: Proxy sym) v curr
    (s TupN./\ r) <- RunS.runStateAt (px @sp) next m
    RunS.putAt (px @sp) (Rec.delete (Proxy :: Proxy sym) s)
    pure r

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

{-
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
-}

data Say = Say

instance DimensionedValTag Say Say

instance
  ( W_ dspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer (m w)) x' x
  , Monad.Monad m
  ) =>
  DimensionedVal Say dspec (w -> R.Run x Unit) where
  mkDimensional _ _ = RunW.tellAt (px @wp) <<< pure

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

instance DimensionedValTag Tell Tell

instance
  ( W_ dspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  DimensionedVal Tell dspec (w -> R.Run x Unit) where
  mkDimensional _ _ = RunW.tellAt (px @wp)

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

instance DimensionedValTag ExecW ExecW

instance
  ( W_ dspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  DimensionedVal ExecW dspec (R.Run x Unit -> R.Run x' w) where
  mkDimensional _ _ m = RunW.runWriterAt (px @wp) m <#> Tup.fst

instance
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  RWSEFn ExecW rp wp sp ep (R.Run x Unit -> R.Run x' w) where
  rwseApply _ _ wp _ _ m = RunW.runWriterAt wp m <#> Tup.fst

data RunW = RunW

instance DimensionedValTag RunW RunW

instance
  ( W_ dspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  DimensionedVal RunW dspec (R.Run x f -> R.Run x' (w TupN./\ f)) where
  mkDimensional _ _ m = RunW.runWriterAt (px @wp) m

instance
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  RWSEFn RunW rp wp sp ep (R.Run x f -> R.Run x' (w TupN./\ f)) where
  rwseApply _ _ wp _ _ m = RunW.runWriterAt wp m

data EvalW = EvalW

instance DimensionedValTag EvalW EvalW

instance
  ( W_ dspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  DimensionedVal EvalW dspec (R.Run x f -> R.Run x' f) where
  mkDimensional _ _ m = RunW.runWriterAt (px @wp) m <#> Tup.snd

instance
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  RWSEFn EvalW rp wp sp ep (R.Run x f -> R.Run x' f) where
  rwseApply _ _ wp _ _ m = RunW.runWriterAt wp m <#> Tup.snd

data MapW = MapW

instance DimensionedValTag MapW MapW

instance
  ( W_ dspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer (m w2)) x'' x'
  , Cons wp (RunW.Writer (m w1)) x' x
  , Monoid.Monoid (m w2)
  , Monoid.Monoid (m w1)
  , Monad.Monad m
  ) =>
  DimensionedVal MapW dspec ((w1 -> w2) -> R.Run x f -> R.Run x' f) where
  mkDimensional _ _ f m = do
    (w TupN./\ res) <- RunW.runWriterAt (px @wp) m
    RunW.tellAt (px @wp) $ map f w
    pure res

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

instance DimensionedValTag FromE FromE

instance
  ( E_ dspec ep
  , IsSymbol ep
  , IsSymbol baseE
  , Cons ep (RunE.Except e) x'' x'
  , Cons baseE (RunE.Except e) x' x
  , TypeEquals.TypeEquals baseE "except"
  ) =>
  DimensionedVal FromE dspec (R.Run x a -> R.Run x' a) where
  mkDimensional _ _ m = do
    RunE.runExceptAt (px @baseE) m >>= onDone
    where
    onDone (Eor.Left e) = RunE.throwAt (px @ep) e
    onDone (Eor.Right v) = pure v

data Try = Try

instance DimensionedValTag Try Try

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  DimensionedVal Try dspec (R.Run x a -> R.Run x' (Eor.Either e a)) where
  mkDimensional _ _ m = RunE.runExceptAt (px @ep) m

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  RWSEFn Try rp wp sp ep (R.Run x a -> R.Run x' (Eor.Either e a)) where
  rwseApply _ _ _ _ ep m = RunE.runExceptAt ep m

data Fail = Fail

instance DimensionedValTag Fail Fail

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  DimensionedVal Fail dspec (e -> R.Run x a) where
  mkDimensional _ _ e = RunE.throwAt (px @ep) e

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

instance DimensionedValTag Ok Ok

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  DimensionedVal Ok dspec (Eor.Either e a -> R.Run x a) where
  mkDimensional _ _ (Eor.Left e) = RunE.throwAt (px @ep) e
  mkDimensional _ _ (Eor.Right a) = pure a

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

instance DimensionedValTag RunParser RunParser

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.ParseError) x' x
  ) =>
  DimensionedVal RunParser dspec (s -> Parsing.Parser s a -> R.Run x a) where
  mkDimensional _ _ s pr = xAt @ep Ok $ Z.runParser s pr

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

instance DimensionedValTag BindE BindE

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e2) x'' x'
  , Cons ep (RunE.Except e1) x' x
  ) =>
  DimensionedVal BindE dspec ((e1 -> R.Run x' f) -> R.Run x f -> R.Run x' f) where
  mkDimensional _ _ be m = xAt @ep Try m >>= onDone
    where
    onDone (Eor.Left e) = be e
    onDone (Eor.Right v) = pure v

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

instance DimensionedValTag MapE MapE

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e2) x'' x'
  , Cons ep (RunE.Except e1) x' x
  ) =>
  DimensionedVal MapE dspec ((e1 -> e2) -> R.Run x f -> R.Run x' f) where
  mkDimensional _ _ fe m = xAt @ep BindE (xAt @ep Fail <<< fe) m

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e2) x'' x'
  , Cons ep (RunE.Except e1) x' x
  ) =>
  RWSEFn MapE rp wp sp ep ((e1 -> e2) -> R.Run x f -> R.Run x' f) where
  rwseApply _ _ _ _ _ fe m = xAt @ep BindE (xAt @ep Fail <<< fe) m

data Unwrap = Unwrap

instance DimensionedValTag Unwrap Unwrap

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  DimensionedVal Unwrap dspec (e -> May.Maybe a -> R.Run x a) where
  mkDimensional _ _ _ (May.Just a) = pure a
  mkDimensional _ _ e _ = xAt @ep Fail e

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

instance DimensionedValTag Unwrap' Unwrap'

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' x
  ) =>
  DimensionedVal Unwrap' dspec (May.Maybe a -> R.Run x a) where
  mkDimensional _ _ = xAt @ep Unwrap $ Z.jsError' "Nothing#unwrap"

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

instance DimensionedValTag Hush Hush

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' x
  , ZD.Defaultable d
  ) =>
  DimensionedVal Hush dspec (R.Run x d -> R.Run x' d) where
  mkDimensional _ _ m = (<$>) ZD.orDefault $ xAt @ep Try m <#> Eor.hush

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

instance DimensionedValTag Invert Invert

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x'' x'
  , Cons ep (RunE.Except r) x' x
  ) =>
  DimensionedVal Invert dspec (R.Run x e -> R.Run x' r) where
  mkDimensional _ _ m = xAt @ep Try m <#> Z.invert >>= xAt @ep Ok

instance
  ( IsSymbol ep
  , Cons ep (RunE.Except e) x'' x'
  , Cons ep (RunE.Except r) x' x
  ) =>
  RWSEFn Invert rp wp sp ep (R.Run x e -> R.Run x' r) where
  rwseApply _ _ _ _ _ m = xAt @ep Try m <#> Z.invert >>= xAt @ep Ok

data TryUntil = TryUntil

instance DimensionedValTag TryUntil TryUntil

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x''' x''
  , Cons ep (RunE.Except r) x'' x'
  , Cons ep (RunE.Except e) x' x
  ) =>
  DimensionedVal TryUntil
    dspec
    ( R.Run x r
      -> Array (e -> R.Run x r)
      -> R.Run x'' r
    ) where
  mkDimensional _ _ try1 tryRest = xAt @ep Invert do
    e1 <- xAt @ep Invert try1
    Z.reduceM (\e tryN -> xAt @ep Invert $ tryN e) e1 tryRest

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

instance DimensionedValTag RunAff RunAff

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' (A x)
  ) =>
  DimensionedVal RunAff dspec (Aff.Aff f -> R.Run (A x) f) where
  mkDimensional _ _ a = do
    res <- aff $ Aff.attempt a
    onDone res
    where
    onDone (Eor.Left e) = xAt @ep Fail $ Z.JsError e
    onDone (Eor.Right v) = pure v

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

instance DimensionedValTag RunEffA RunEffA

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' (A x)
  ) =>
  DimensionedVal RunEffA dspec (Eff.Effect f -> R.Run (A x) f) where
  mkDimensional _ _ eff = do
    res <- aff $ Aff.attempt $ EffC.liftEffect eff
    onDone res
    where
    onDone (Eor.Left e) = xAt @ep Fail $ Z.JsError e
    onDone (Eor.Right v) = pure v

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

instance DimensionedValTag RunEffPromise RunEffPromise

instance
  ( E_ dspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' (A x)
  ) =>
  DimensionedVal RunEffPromise
    dspec
    (Eff.Effect (Promise.Promise f) -> R.Run (A x) f) where
  mkDimensional _ _ = effectPromiseToAff >>> xAt @ep RunAff

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

instance DimensionedValTag RunResult RunResult

instance
  ( E_ dspec ep
  , IsSymbol ep
  , W_ dspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer (Array w)) x'' x'
  , Cons ep (RunE.Except e) x' x
  ) =>
  DimensionedVal RunResult
    dspec
    (R.Run x a -> R.Run x'' (Result w e a)) where
  mkDimensional _ _ m = do
    w <- RunW.runWriterAt (px @wp) $ RunE.runExceptAt (px @ep) m
    pure $ { w: (Tup.fst w), v: (Tup.snd w) }

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

instance DimensionedValTag Unresult Unresult

instance
  ( E_ dspec ep
  , IsSymbol ep
  , W_ dspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer (Array w)) x'' x
  , Cons ep (RunE.Except e) x' x
  ) =>
  DimensionedVal Unresult dspec (Result w e a -> R.Run x a) where
  mkDimensional _ _ { w, v } = do
    xAt @wp Tell w
    xAt @ep Ok v

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

instance DimensionedValTag MapWE MapWE

instance
  ( DimensionedVal MapE dspec ((e1 -> e2) -> f'' -> f')
  , DimensionedVal MapW dspec ((w1 -> w2) -> f' -> f)
  ) =>
  DimensionedVal MapWE dspec ((w1 -> w2) -> (e1 -> e2) -> f'' -> f) where
  mkDimensional _ _ fw fe m = mkDimensional (px @MapW) (px @dspec) fw
    $ mkDimensional (px @MapE) (px @dspec) fe m

instance
  ( RWSEFn MapE rp wp sp ep' ((e1 -> e2) -> f'' -> f')
  , RWSEFn MapW rp wp sp ep' ((w1 -> w2) -> f' -> f)
  , WpEpPickEp wp ep ep'
  ) =>
  RWSEFn MapWE rp wp sp ep ((w1 -> w2) -> (e1 -> e2) -> f'' -> f) where
  rwseApply _ _ _ _ _ fw fe m = mkXFn @rp @wp @sp @ep' MapW fw
    $ mkXFn @rp @wp @sp @ep' MapE fe m

data TellMappedHush = TellMappedHush

instance DimensionedValTag TellMappedHush TellMappedHush

instance
  ( E_ dspec ep
  , IsSymbol ep
  , W_ dspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer (m w)) x'' x'
  , Cons ep (RunE.Except e) x' x
  , Monad.Monad m
  , ZD.Defaultable d
  ) =>
  DimensionedVal TellMappedHush dspec ((e -> w) -> R.Run x d -> R.Run x' d) where
  mkDimensional _ _ mapW m = xAt @ep Try m >>= onDone
    where
    onDone (Eor.Left e) = xAt @wp Say (mapW e) <#> const ZD.default
    onDone (Eor.Right r) = pure $ r

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

instance DimensionedValTag TellMappedMHush TellMappedMHush

instance
  ( E_ dspec ep
  , IsSymbol ep
  , W_ dspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer (m w)) x'' x'
  , Cons ep (RunE.Except e) x' x
  , Monad.Monad m
  , Monoid.Monoid (m w)
  , ZD.Defaultable d
  ) =>
  DimensionedVal TellMappedMHush dspec ((e -> m w) -> R.Run x d -> R.Run x' d) where
  mkDimensional _ _ mapW m = xAt @ep Try m >>= onDone
    where
    onDone (Eor.Left e) = xAt @wp Tell (mapW e) <#> const ZD.default
    onDone (Eor.Right r) = pure $ r

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

eval_ :: forall a. R.Run () a -> a
eval_ m = Unsafe.unsafePerformEffect $ R.runBaseEffect $ R.expand m

evalX :: forall a. X () a -> a
evalX m = Unsafe.unsafePerformEffect $ R.runBaseEffect $ R.expand $ runXBase m

runX :: forall e a. X (E e ()) a -> Eor.Either e a
runX = evalX <<< x' @"try"

evalXA :: forall a. X (A ()) a -> Aff.Aff a
evalXA m = R.match { aff: \(AffCmd a) -> a } # R.run $ runXBase m

runXA :: forall e a. X (EA e ()) a -> Aff.Aff (Eor.Either e a)
runXA = evalXA <<< x' @"try"

--------------- OTHER ------------------------------------------------------

type Edit s = X (S s ()) Unit

edit :: forall a. a -> Edit a -> a
edit init m = R.extract $ RunS.execState init $
  runXBase m

type StrW = X (Wa String ()) Unit

joinStrW :: String -> StrW -> String
joinStrW s m = StrCommon.joinWith s $ evalX $ x' @"execW" m

--------------- E FNS -----------------------------------------------------

type Result w e a = { w :: (Array w), v :: (Eor.Either e a) }

--------------- A FNS -----------------------------------------------------

foreign import js_timeout :: Int -> Eff.Effect (Promise.Promise Unit)

promiseToAff :: forall a. Promise.Promise a -> Aff.Aff a
promiseToAff = Promise.toAff

effectPromiseToAff :: forall a. Eff.Effect (Promise.Promise a) -> Aff.Aff a
effectPromiseToAff e = EffC.liftEffect e >>= promiseToAff

xTimeout :: forall x. Int -> X (A x) Unit
xTimeout ms = Z.fDiscard $ x' @"try" $ x' @"runEffPromise" $ js_timeout ms

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
