module Z.Z.X.Core
  ( A
  , AFF
  , AffF
  , E
  , EA
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
  , StrW
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
  , XAsk
  , XBASE
  , XBaseF
  , XBase_
  , XBindE
  , XEvalS
  , XEvalW
  , XExecS
  , XExecW
  , XFail
  , XFromE
  , XGet
  , XGetWithT
  , XGetterReaderT(..)
  , XGetterStateT(..)
  , XHush
  , XImpl
  , XInvert
  , XMapE
  , XMapW
  , XMapWE
  , XModify
  , XOk
  , XOver
  , XOver_
  , XOver_T
  , XPlusS
  , XPlusST
  , XPreviewR
  , XPreviewR_(..)
  , XPreviewS
  , XPreviewS_(..)
  , XPreviewWithT
  , XPreview_WithT
  , XPut
  , XRun
  , XRunAff
  , XRunEffA
  , XRunEffPromise
  , XRunParser
  , XRunR
  , XRunResult
  , XRunS
  , XRunW
  , XRunWA
  , XSay
  , XSet
  , XSet_
  , XSet_T
  , XTell
  , XTellMappedHush
  , XTellMappedMHush
  , XToArrayOfR
  , XToArrayOfR_(..)
  , XToArrayOfS
  , XToArrayOfS_(..)
  , XToArrayOfWithT
  , XToArrayOf_WithT
  , XTry
  , XTryUntil
  , XUnresult
  , XUnwrap
  , XUnwrap'
  , XViewR
  , XViewR_(..)
  , XViewS
  , XViewS_(..)
  , XViewWithT
  , XView_WithT
  , XWithReturn
  , class GOrE
  , class GOrR
  , class GOrS
  , class GOrW
  , class XGetterTypes
  , edit
  , evalX
  , evalXA
  , joinStrW
  , runX
  , runXA
  , runXBase
  , xGetter
  , xInfo
  , xLogError
  , xLogWarning
  , xOut
  , xOutErr
  , xPass
  , xTimeout
  ) where

import Z.Z.X.UtilPrelude

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
import Data.String.Common as StrCommon
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
import Z.Z.Barlow as Bl
import Z.Z.Core as Z
import Z.Z.Defaultable
  ( class G2OrDefault
  , class GOrDefault
  , class GTagMap
  , class GenerableC
  , G1
  , GDefault
  , Generable
  , g
  , g1
  , z
  )
import Z.Z.Defaultable as ZD
import Z.Z.Defaultable.Generable (g')

class GOrR gspec p | gspec -> p
class GOrW gspec p | gspec -> p
class GOrS gspec p | gspec -> p
class GOrE gspec p | gspec -> p

instance (GOrDefault "reader" gspec p) => GOrR gspec p
instance (GOrDefault "writer" gspec p) => GOrW gspec p
instance (GOrDefault "state" gspec p) => GOrS gspec p
instance (GOrDefault "except" gspec p) => GOrE gspec p

data XImpl :: forall k. k -> Type
data XImpl xFn

------------------------------- e -------------------------------------

type XFromE = Generable (XImpl "fromE")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , IsSymbol baseE
  , Cons ep (RunE.Except e) x'' x'
  , Cons baseE (RunE.Except e) x' x
  , TypeEquals.TypeEquals baseE "except"
  ) =>
  GenerableC (XImpl "fromE") gspec (R.Run x a -> R.Run x' a) where
  mkGenerable m = do
    RunE.runExceptAt (px @baseE) m >>= onDone
    where
    onDone (Eor.Left e) = RunE.throwAt (px @ep) e
    onDone (Eor.Right v) = pure v

type XFail = Generable (XImpl "fail")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  GenerableC (XImpl "fail") gspec (e -> R.Run x a) where
  mkGenerable e = RunE.throwAt (px @ep) e

type XRunParser = Generable (XImpl "runParser")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.ParseError) x' x
  ) =>
  GenerableC (XImpl "runParser") gspec (s -> Parsing.Parser s a -> R.Run x a) where
  mkGenerable s pr = g1 @XOk @ep $ Z.runParser s pr

type XBindE = Generable (XImpl "bindE")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e2) x'' x'
  , Cons ep (RunE.Except e1) x' x
  ) =>
  GenerableC (XImpl "bindE")
    gspec
    ((e1 -> R.Run x' f) -> R.Run x f -> R.Run x' f) where
  mkGenerable be m = g1 @XTry @ep m >>= onDone
    where
    onDone (Eor.Left e) = be e
    onDone (Eor.Right v) = pure v

type XMapE = Generable (XImpl "mapE")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e2) x'' x'
  , Cons ep (RunE.Except e1) x' x
  ) =>
  GenerableC (XImpl "mapE") gspec ((e1 -> e2) -> R.Run x f -> R.Run x' f) where
  mkGenerable fe m = g1 @XBindE @ep (g1 @XFail @ep <<< fe) m

type XUnwrap = Generable (XImpl "unwrap")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  GenerableC (XImpl "unwrap") gspec (e -> May.Maybe a -> R.Run x a) where
  mkGenerable _ (May.Just a) = pure a
  mkGenerable e _ = g1 @XFail @ep e

type XUnwrap' = Generable (XImpl "unwrap'")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' x
  ) =>
  GenerableC (XImpl "unwrap'") gspec (May.Maybe a -> R.Run x a) where
  mkGenerable = g1 @XUnwrap @ep $ Z.jsError' "Nothing#unwrap"

type XHush = Generable (XImpl "hush")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' x
  , GenerableC d GDefault d
  ) =>
  GenerableC (XImpl "hush") gspec (R.Run x d -> R.Run x' d) where
  mkGenerable m = (<$>) ZD.orDefault $ g1 @XTry @ep m <#> Eor.hush

type XInvert = Generable (XImpl "invert")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x'' x'
  , Cons ep (RunE.Except r) x' x
  ) =>
  GenerableC (XImpl "invert") gspec (R.Run x e -> R.Run x' r) where
  mkGenerable m = g1 @XTry @ep m <#> Z.invert >>= g1 @XOk @ep

type XTryUntil = Generable (XImpl "tryUntil")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x''' x''
  , Cons ep (RunE.Except r) x'' x'
  , Cons ep (RunE.Except e) x' x
  ) =>
  GenerableC (XImpl "tryUntil")
    gspec
    ( R.Run x r
      -> Array (e -> R.Run x r)
      -> R.Run x'' r
    ) where
  mkGenerable try1 tryRest = g1 @XInvert @ep do
    e1 <- g1 @XInvert @ep try1
    Z.reduceM (\e tryN -> g1 @XInvert @ep $ tryN e) e1 tryRest

type XRunAff = Generable (XImpl "runAff")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' (A x)
  ) =>
  GenerableC (XImpl "runAff") gspec (Aff.Aff f -> R.Run (A x) f) where
  mkGenerable a = do
    res <- aff $ Aff.attempt a
    onDone res
    where
    onDone (Eor.Left e) = g1 @XFail @ep $ Z.JsError e
    onDone (Eor.Right v) = pure v

type XRunEffA = Generable (XImpl "runEffA")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' (A x)
  ) =>
  GenerableC (XImpl "runEffA") gspec (Eff.Effect f -> R.Run (A x) f) where
  mkGenerable eff = do
    res <- aff $ Aff.attempt $ EffC.liftEffect eff
    onDone res
    where
    onDone (Eor.Left e) = g1 @XFail @ep $ Z.JsError e
    onDone (Eor.Right v) = pure v

type XRunEffPromise = Generable (XImpl "runEffPromise")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except Z.JsError) x' (A x)
  ) =>
  GenerableC (XImpl "runEffPromise")
    gspec
    (Eff.Effect (Promise.Promise f) -> R.Run (A x) f) where
  mkGenerable = effectPromiseToAff >>> g1 @XRunAff @ep

type XTry = Generable (XImpl "try")

instance
  ( GOrE gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  GenerableC (XImpl "try") gspec (R.Run x a -> R.Run x' (Eor.Either e a)) where
  mkGenerable m = RunE.runExceptAt (px @ep) m

type XOk = Generable (XImpl "ok")

instance
  ( GOrDefault "except" gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except e) x' x
  ) =>
  GenerableC (XImpl "ok") gspec (Eor.Either e a -> R.Run x a) where
  mkGenerable (Eor.Left e) = RunE.throwAt (px @ep) e
  mkGenerable (Eor.Right a) = pure a

type XWithReturn = Generable (XImpl "withReturn")

instance
  ( GOrDefault "earlyReturn" gspec ep
  , IsSymbol ep
  , Cons ep (RunE.Except r) x' x
  ) =>
  GenerableC (XImpl "withReturn")
    gspec
    (((r -> R.Run x Unit) -> R.Run x r) -> R.Run x' r) where
  mkGenerable m = RunE.runExceptAt (px @ep) (m return) >>= onRes
    where
    return = RunE.throwAt (px @ep)
    onRes (Eor.Left ret) = pure ret
    onRes (Eor.Right ret) = pure ret

------------------------------ w/e ----------------------------------------

type XRunResult = Generable (XImpl "runResult")

instance
  ( GOrDefault "except" gspec ep
  , G2OrDefault "writer" gspec wp
  , IsSymbol ep
  , IsSymbol wp
  , Cons wp (RunW.Writer (Array w)) x'' x'
  , Cons ep (RunE.Except e) x' x
  ) =>
  GenerableC (XImpl "runResult") gspec (R.Run x a -> R.Run x'' (Result w e a)) where
  mkGenerable m = do
    w <- RunW.runWriterAt (px @wp) $ RunE.runExceptAt (px @ep) m
    pure $ { w: (Tup.fst w), v: (Tup.snd w) }

type XMapWE = Generable (XImpl "mapWE")

instance
  ( GOrDefault "except" gspec ep
  , G2OrDefault "writer" gspec wp
  , GenerableC (XImpl "mapE") (G1 ep) ((e1 -> e2) -> f'' -> f')
  , GenerableC (XImpl "mapW") (G1 wp) ((w1 -> w2) -> f' -> f)
  ) =>
  GenerableC (XImpl "mapWE") gspec ((w1 -> w2) -> (e1 -> e2) -> f'' -> f) where
  mkGenerable fw fe m = g1 @XMapW @wp fw $ g1 @XMapE @ep fe m

type XUnresult = Generable (XImpl "unresult")

instance
  ( GOrDefault "except" gspec ep
  , G2OrDefault "writer" gspec wp
  , IsSymbol ep
  , IsSymbol wp
  , Cons wp (RunW.Writer (Array w)) x'' x
  , Cons ep (RunE.Except e) x' x
  ) =>
  GenerableC (XImpl "unresult") gspec (Result w e a -> R.Run x a) where
  mkGenerable { w, v } = do
    g1 @XTell @wp w
    g1 @XOk @ep v

------------------------------ w ----------------------------------------

type XExecW = Generable (XImpl "execW")

instance
  ( GOrW gspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  GenerableC (XImpl "execW") gspec (R.Run x Unit -> R.Run x' w) where
  mkGenerable m = RunW.runWriterAt (px @wp) m <#> Tup.fst

type XRunW = Generable (XImpl "runW")

instance
  ( GOrW gspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  GenerableC (XImpl "runW") gspec (R.Run x f -> R.Run x' (w TupN./\ f)) where
  mkGenerable m = RunW.runWriterAt (px @wp) m

type XEvalW = Generable (XImpl "evalW")

instance
  ( GOrW gspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid.Monoid w
  ) =>
  GenerableC (XImpl "evalW") gspec (R.Run x f -> R.Run x' f) where
  mkGenerable m = RunW.runWriterAt (px @wp) m <#> Tup.snd

type XMapW = Generable (XImpl "mapW")

instance
  ( GOrW gspec wp
  , IsSymbol wp
  , Cons wp (RunW.Writer (m w2)) x'' x'
  , Cons wp (RunW.Writer (m w1)) x' x
  , Monoid.Monoid (m w2)
  , Monoid.Monoid (m w1)
  , Monad.Monad m
  ) =>
  GenerableC (XImpl "mapW") gspec ((w1 -> w2) -> R.Run x f -> R.Run x' f) where
  mkGenerable f m = do
    (w TupN./\ res) <- RunW.runWriterAt (px @wp) m
    RunW.tellAt (px @wp) $ map f w
    pure res

type XSay = Generable (XImpl "say")

instance
  ( IsSymbol wp
  , Cons wp (RunW.Writer (m w)) x' x
  , Monad m
  , GOrW gspec wp
  ) =>
  GenerableC (XImpl "say") gspec (w -> R.Run x Unit) where
  mkGenerable w = do
    RunW.tellAt (Proxy @wp) $ pure w
    pure unit

type XTell = Generable (XImpl "tell")

instance
  ( IsSymbol wp
  , Cons wp (RunW.Writer w) x' x
  , Monoid w
  , GOrW gspec wp
  ) =>
  GenerableC (XImpl "tell") gspec (w -> R.Run x Unit) where
  mkGenerable w = do
    RunW.tellAt (Proxy @wp) w
    pure unit

type XTellMappedHush = Generable (XImpl "tellMappedHush")

instance
  ( G2OrDefault "except" gspec ep
  , GOrW gspec wp
  , IsSymbol ep
  , IsSymbol wp
  , Cons wp (RunW.Writer (m w)) x'' x'
  , Cons ep (RunE.Except e) x' x
  , Monad.Monad m
  , GenerableC d GDefault d
  ) =>
  GenerableC (XImpl "tellMappedHush")
    gspec
    ((e -> w) -> R.Run x d -> R.Run x' d) where
  mkGenerable mapW m = g1 @XTry @ep m >>= onDone
    where
    onDone (Eor.Left e) = g1 @XSay @wp (mapW e) <#> const (ZD.default @d)
    onDone (Eor.Right r) = pure $ r

type XTellMappedMHush = Generable (XImpl "tellMappedMHush")

instance
  ( G2OrDefault "except" gspec ep
  , GOrDefault "writer" gspec wp
  , IsSymbol ep
  , IsSymbol wp
  , Cons wp (RunW.Writer (m w)) x'' x'
  , Cons ep (RunE.Except e) x' x
  , Monad.Monad m
  , Monoid.Monoid (m w)
  , GenerableC d GDefault d
  ) =>
  GenerableC (XImpl "tellMappedMHush")
    gspec
    ((e -> m w) -> R.Run x d -> R.Run x' d) where
  mkGenerable mapW m = g1 @XTry @ep m >>= onDone
    where
    onDone (Eor.Left e) = g1 @XTell @wp (mapW e) <#> const (ZD.default @d)
    onDone (Eor.Right r) = pure $ r

------------------------------- R ------------------------------------

type XRunR = Generable (XImpl "runR")

instance
  ( IsSymbol p
  , Cons p (RunR.Reader r) x' x
  , GOrR gspec p
  ) =>
  GenerableC (XImpl "runR") gspec (r -> R.Run x a -> R.Run x' a) where
  mkGenerable = RunR.runReaderAt (P.Proxy :: P.Proxy p)

------------------------------- S ------------------------------------

type XExecS = Generable (XImpl "execS")

instance
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , GOrS gspec sp
  ) =>
  GenerableC (XImpl "execS") gspec (s -> R.Run x f -> R.Run x' s) where
  mkGenerable initState m = RunS.execStateAt (px @sp) initState m

type XRunS = Generable (XImpl "runS")

instance
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , GOrS gspec sp
  ) =>
  GenerableC (XImpl "runS") gspec (s -> R.Run x f -> R.Run x' (s TupN./\ f)) where
  mkGenerable initState m = RunS.runStateAt (px @sp) initState m

type XEvalS = Generable (XImpl "evalS")

instance
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , GOrS gspec sp
  ) =>
  GenerableC (XImpl "evalS") gspec (s -> R.Run x f -> R.Run x' f) where
  mkGenerable initState m = RunS.evalStateAt (px @sp) initState m

type XPut = Generable (XImpl "put")

instance
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , GOrS gspec sp
  ) =>
  GenerableC (XImpl "put") gspec (s -> R.Run x Unit) where
  mkGenerable = RunS.putAt (P.Proxy :: P.Proxy sp)

type XModify = Generable (XImpl "modify")

instance
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , GOrS gspec sp
  ) =>
  GenerableC (XImpl "modify") gspec ((s -> s) -> R.Run x Unit) where
  mkGenerable = RunS.modifyAt (P.Proxy :: P.Proxy sp)

type XSet = Generable (XImpl "set")

instance
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , GOrS gspec sp
  ) =>
  GenerableC (XImpl "set")
    gspec
    (Lens.Optic Function s s a b -> b -> R.Run x Unit) where
  mkGenerable l v = do
    s <- RunS.getAt (P.Proxy :: P.Proxy sp)
    RunS.putAt (P.Proxy :: P.Proxy sp) $ Lens.set l v s

type XOver = Generable (XImpl "over")

instance
  ( IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , GOrS gspec sp
  ) =>
  GenerableC (XImpl "over")
    gspec
    (Lens.Optic Function s s a b -> (a -> b) -> R.Run x Unit) where
  mkGenerable l f = do
    s <- RunS.getAt (P.Proxy :: P.Proxy sp)
    RunS.putAt (P.Proxy :: P.Proxy sp) $ Lens.over l f s

data XSet_T str

instance
  ( GOrS gspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses Function s s a b
  , Bl.IsSymbol sym
  ) =>
  GenerableC (XSet_T sym) gspec (b -> R.Run x Unit) where
  mkGenerable v = do
    s <- RunS.getAt (px @sp)
    RunS.putAt (px @sp) $ Lens.set (Bl.barlow @sym) v s

data XSet_ str

instance GTagMap (XSet_ str) (XSet_T str)

data XOver_T str

instance
  ( GOrS gspec sp
  , IsSymbol sp
  , Cons sp (RunS.State s) x' x
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses Function s s a b
  , Bl.IsSymbol sym
  ) =>
  GenerableC (XOver_T sym) gspec ((a -> b) -> R.Run x Unit) where
  mkGenerable f = do
    s <- RunS.getAt (px @sp)
    RunS.putAt (px @sp) $ Lens.over (Bl.barlow @sym) f s

data XOver_ str

instance GTagMap (XOver_ str) (XOver_T str)

data XPlusST str

instance
  ( GOrS gspec sp
  , IsSymbol sp
  , IsSymbol sym
  , Row.Lacks sym r1
  , Cons sym a r1 r2
  , Cons sp (RunS.State { | r1 }) x'' x'
  , Cons sp (RunS.State { | r2 }) x' x
  ) =>
  GenerableC (XPlusST sym) gspec (a -> R.Run x f -> R.Run x' f) where
  mkGenerable v m = do
    curr <- RunS.getAt (px @sp)
    let next = Rec.insert (Proxy :: Proxy sym) v curr
    (s TupN./\ r) <- RunS.runStateAt (px @sp) next m
    RunS.putAt (px @sp) (Rec.delete (Proxy :: Proxy sym) s)
    pure r

data XPlusS str

instance GTagMap (XPlusS str) (XPlusST str)

------------------------- GETTERS ----------------------------

class XGetterTypes
  :: forall k1 k2
   . k1
  -> k2
  -> Symbol
  -> (Type -> Type -> Type)
  -> Type
  -> Constraint
class XGetterTypes getter gspec p m t | getter gspec -> p m t where
  xGetter :: forall x' x. IsSymbol p => Cons p (m t) x' x => R.Run x t

data XGetterStateT = XGetterStateT
data XGetterReaderT = XGetterReaderT

instance
  ( GOrDefault "reader" gspec p
  ) =>
  XGetterTypes XGetterReaderT gspec p RunR.Reader r where
  xGetter = RunR.askAt (Proxy @p)

instance
  ( GOrDefault "state" gspec p
  ) =>
  XGetterTypes XGetterStateT gspec p RunS.State s where
  xGetter = RunS.getAt (Proxy @p)

data XGetWithT :: forall k. k -> Type
data XGetWithT t = XGetWithT

instance
  ( XGetterTypes getter gspec p m s
  , IsSymbol p
  , Cons p (m s) x' x
  ) =>
  GenerableC (XGetWithT getter) gspec (R.Run x s) where
  mkGenerable = xGetter @getter @gspec

data XViewWithT :: forall k. k -> Type
data XViewWithT t = XViewWithT

instance
  ( XGetterTypes getter gspec p m s
  , IsSymbol p
  , Cons p (m s) x' x
  ) =>
  GenerableC (XViewWithT getter)
    gspec
    ((Lens.Optic (Forget a) s t a b) -> R.Run x a) where
  mkGenerable l = do
    v <- g' @(Generable (XGetWithT getter)) @gspec
    pure $ Lens.view l v

data XView_WithT :: forall @k. k -> Type -> Type
data XView_WithT sym t = XView_WithT

instance
  ( XGetterTypes getter gspec p m s
  , IsSymbol p
  , Cons p (m s) x' x
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget a) s t a b
  , Bl.IsSymbol sym
  ) =>
  GenerableC (XView_WithT sym getter)
    gspec
    (R.Run x a) where
  mkGenerable = do
    v <- g' @(Generable (XGetWithT getter)) @gspec
    pure $ Lens.view (Bl.barlow @sym) v

data XPreviewWithT :: forall k. k -> Type
data XPreviewWithT t = XPreviewWithT

instance
  ( XGetterTypes getter gspec p m s
  , IsSymbol p
  , Cons p (m s) x' x
  ) =>
  GenerableC (XPreviewWithT getter)
    gspec
    ((Lens.Optic (Forget (MayFirst.First a)) s t a b) -> R.Run x (May.Maybe a)) where
  mkGenerable l = do
    v <- g' @(Generable (XGetWithT getter)) @gspec
    pure $ Lens.preview l v

data XPreview_WithT :: forall @k. k -> Type -> Type
data XPreview_WithT sym t = XPreview_WithT

instance
  ( XGetterTypes getter gspec p m s
  , IsSymbol p
  , Cons p (m s) x' x
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget (MayFirst.First a)) s t a b
  , Bl.IsSymbol sym
  ) =>
  GenerableC (XPreview_WithT sym getter)
    gspec
    (R.Run x (May.Maybe a)) where
  mkGenerable = do
    v <- g' @(Generable (XGetWithT getter)) @gspec
    pure $ Lens.preview (Bl.barlow @sym) v

data XToArrayOfWithT :: forall k. k -> Type
data XToArrayOfWithT t = XToArrayOfWithT

instance
  ( XGetterTypes getter gspec p m s
  , IsSymbol p
  , Cons p (m s) x' x
  ) =>
  GenerableC (XToArrayOfWithT getter)
    gspec
    ( (Lens.Optic (Forget (Endo.Endo Function (ListT.List a))) s t a b)
      -> R.Run x (Array a)
    ) where
  mkGenerable l = do
    v <- g' @(Generable (XGetWithT getter)) @gspec
    pure $ Lens.toArrayOf l v

data XToArrayOf_WithT :: forall @k. k -> Type -> Type
data XToArrayOf_WithT sym t = XToArrayOf_WithT

instance
  ( XGetterTypes getter gspec p m s
  , IsSymbol p
  , Cons p (m s) x' x
  , Bl.ParseSymbol sym lenses
  , Bl.ConstructBarlow lenses (Bl.Forget (Endo.Endo Function (ListT.List a))) s
      t
      a
      b
  , Bl.IsSymbol sym
  ) =>
  GenerableC (XToArrayOf_WithT sym getter)
    gspec
    (R.Run x (Array a)) where
  mkGenerable = do
    v <- g' @(Generable (XGetWithT getter)) @gspec
    pure $ Lens.toArrayOf (Bl.barlow @sym) v

type XAsk = Generable (XGetWithT XGetterReaderT)
type XGet = Generable (XGetWithT XGetterStateT)
type XViewR = Generable (XViewWithT XGetterReaderT)
type XViewS = Generable (XViewWithT XGetterStateT)
type XPreviewR = Generable (XPreviewWithT XGetterReaderT)
type XPreviewS = Generable (XPreviewWithT XGetterStateT)
type XToArrayOfR = Generable (XToArrayOfWithT XGetterReaderT)
type XToArrayOfS = Generable (XToArrayOfWithT XGetterStateT)

data XViewR_ s
data XViewS_ s
data XPreviewR_ s
data XPreviewS_ s
data XToArrayOfR_ s
data XToArrayOfS_ s

instance GTagMap (XViewR_ s) (XView_WithT s XGetterReaderT)
instance GTagMap (XPreviewR_ s) (XPreview_WithT s XGetterReaderT)
instance GTagMap (XToArrayOfR_ s) (XToArrayOf_WithT s XGetterReaderT)

instance GTagMap (XViewS_ s) (XView_WithT s XGetterStateT)
instance GTagMap (XPreviewS_ s) (XPreview_WithT s XGetterStateT)
instance GTagMap (XToArrayOfS_ s) (XToArrayOf_WithT s XGetterStateT)

-----------------------------------------------------------------------------

px :: forall @k. P.Proxy k
px = P.Proxy

--------------- EVAL -------------------------------------------------------

evalX :: forall a. XRun () a -> a
evalX m = Unsafe.unsafePerformEffect $ R.runBaseEffect $ R.expand $ runXBase m

runX :: forall e a. XRun (E e ()) a -> Eor.Either e a
runX = evalX <<< z @XTry

evalXA :: forall a. XRun (A ()) a -> Aff.Aff a
evalXA m = R.match { aff: \(AffCmd a) -> a } # R.run $ runXBase m

runXA :: forall e a. XRun (EA e ()) a -> Aff.Aff (Eor.Either e a)
runXA = evalXA <<< z @XTry

--------------- OTHER ------------------------------------------------------

type Edit s = XRun (S s ()) Unit

edit :: forall a. a -> Edit a -> a
edit init m = R.extract $ RunS.execState init $
  runXBase m

type StrW = XRun (Wa String ()) Unit

joinStrW :: String -> StrW -> String
joinStrW s m = StrCommon.joinWith s $ evalX $ g @XExecW m

--------------- E FNS -----------------------------------------------------

type Result w e a = { w :: (Array w), v :: (Eor.Either e a) }

--------------- A FNS -----------------------------------------------------

foreign import js_timeout :: Int -> Eff.Effect (Promise.Promise Unit)

promiseToAff :: forall a. Promise.Promise a -> Aff.Aff a
promiseToAff = Promise.toAff

effectPromiseToAff :: forall a. Eff.Effect (Promise.Promise a) -> Aff.Aff a
effectPromiseToAff e = EffC.liftEffect e >>= promiseToAff

xTimeout :: forall x. Int -> XRun (A x) Unit
xTimeout ms = Z.fDiscard $ g @XTry $ g @XRunEffPromise $ js_timeout ms

--------------- CORE TYPE ---------------------------------------------------

type XRun x a = R.Run (XBASE x) a

type XRunWA w fx a = R.Run (XBASE + fx + Wa w ()) a

--------------- AFF -------------------------------------------------------

data AffF a = AffCmd (Aff.Aff a)

derive instance Functor AffF

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

derive instance Functor XBaseF

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

xPass :: forall x. XRun x Unit
xPass = R.lift _eff (PassCmd unit)

xOut :: forall l x. l -> XRun x Unit
xOut v = Z.fDiscard $ R.lift _eff (LogDirectCmd "log" (Z.jsAny v) unit)

xOutErr :: forall l x. l -> XRun x Unit
xOutErr v = Z.fDiscard $ R.lift _eff (LogDirectCmd "error" (Z.jsAny v) unit)

xLogCmd :: forall l x. String -> l -> XRun x Unit
xLogCmd k v = do
  let src = Unsafe.unsafePerformEffect js_getStack
  Z.fDiscard $ R.lift _eff (LogCmd k src (Z.jsAny v) unit)

xInfo :: forall l x. l -> XRun x Unit
xInfo = xLogCmd "log"

xLogWarning :: forall l x. l -> XRun x Unit
xLogWarning = xLogCmd "warn"

xLogError :: forall l x. l -> XRun x Unit
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
