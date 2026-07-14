module Z
  ( ModX
  , X_a
  , X_a_
  , X_e
  , X_e_
  , X_ea
  , X_ea_
  , X_r
  , X_r_
  , X_ra
  , X_ra_
  , X_re
  , X_re_
  , X_rs
  , X_rs_
  , X_rw
  , X_rw_
  , X_s
  , X_s_
  , X_sa
  , X_sa_
  , X_se
  , X_se_
  , X_w
  , X_w_
  , X_wa
  , X_wa_
  , X_we
  , X_we_
  , X_ws
  , X_ws_
  , Xa
  , Xa_
  , Xe
  , Xe_
  , Xea
  , Xea_
  , Xr
  , Xr_
  , Xra
  , Xra_
  , Xre
  , Xre_
  , Xrs
  , Xrs_
  , Xrw
  , Xrw_
  , Xs
  , Xs_
  , Xsa
  , Xsa_
  , Xse
  , Xse_
  , Xw
  , Xw_
  , Xwa
  , Xwa_
  , Xwe
  , Xwe_
  , Xws
  , Xws_
  , effectPromiseToAff
  , effectPromiseX
  , module Aff
  , module Core
  , module Effect
  , module EffectClass
  , module JSON
  , module Lens
  , module LensRecord
  , module Maybe
  , module Promise
  , module Proxy
  , module Record
  , module Run
  , module RunS
  , module X
  , promiseToAff
  , xMod
  , xUpdate
  ) where

import Prelude
import Core (JsError) as Core
import Data.Lens (Lens, Lens') as Lens
import Data.Lens.Record (prop) as LensRecord
import Data.Maybe (Maybe(..)) as Maybe
import Effect.Aff (Aff) as Aff
import JSON (JSON, null) as JSON
import Type.Proxy (Proxy(..)) as Proxy
import X (pass, tryAff, result, X, R, W, S, E, A, e_map, s_set) as X
import Control.Promise (Promise) as Promise
import Control.Promise (toAff)
import Effect (Effect) as Effect
import Effect.Class (liftEffect) as EffectClass
import Record (merge, get, set, modify) as Record
import Run (Run, extract) as Run
import Run.State (execState) as RunS

promiseToAff :: forall a. Promise.Promise a -> Aff.Aff a
promiseToAff = toAff

effectPromiseToAff :: forall a. Effect.Effect (Promise.Promise a) -> Aff.Aff a
effectPromiseToAff e = EffectClass.liftEffect e >>= promiseToAff

effectPromiseX
  :: forall a x
   . Effect.Effect (Promise.Promise a)
  -> Xea x Core.JsError a
effectPromiseX = X.tryAff <<< effectPromiseToAff

type Xr x r a = Run.Run (X.R r x) a
type Xw x w a = Run.Run (X.W w x) a
type Xs x s a = Run.Run (X.S s x) a
type Xe x e a = Run.Run (X.E e x) a
type Xa x a = Run.Run (X.A x) a
type Xrw x r w a = Run.Run (X.W w (X.R r x)) a
type Xrs x r s a = Run.Run (X.S s (X.R r x)) a
type Xre x r e a = Run.Run (X.E e (X.R r x)) a
type Xra x r a = Run.Run (X.A (X.R r x)) a
type Xws x w s a = Run.Run (X.S s (X.W w x)) a
type Xwe x w e a = Run.Run (X.E e (X.W w x)) a
type Xwa x w a = Run.Run (X.A (X.W w x)) a
type Xse x s e a = Run.Run (X.E e (X.S s x)) a
type Xsa x s a = Run.Run (X.A (X.S s x)) a
type Xea x e a = Run.Run (X.A (X.E e x)) a
type X_r r a = Run.Run (X.R r ()) a
type X_w w a = Run.Run (X.W w ()) a
type X_s s a = Run.Run (X.S s ()) a
type X_e e a = Run.Run (X.E e ()) a
type X_a a = Run.Run (X.A ()) a
type X_rw r w a = Run.Run (X.W w (X.R r ())) a
type X_rs r s a = Run.Run (X.S s (X.R r ())) a
type X_re r e a = Run.Run (X.E e (X.R r ())) a
type X_ra r a = Run.Run (X.A (X.R r ())) a
type X_ws w s a = Run.Run (X.S s (X.W w ())) a
type X_we w e a = Run.Run (X.E e (X.W w ())) a
type X_wa w a = Run.Run (X.A (X.W w ())) a
type X_se s e a = Run.Run (X.E e (X.S s ())) a
type X_sa s a = Run.Run (X.A (X.S s ())) a
type X_ea e a = Run.Run (X.A (X.E e ())) a

type Xr_ x r = Run.Run (X.R r x) Unit
type Xw_ x w = Run.Run (X.W w x) Unit
type Xs_ x s = Run.Run (X.S s x) Unit
type Xe_ x e = Run.Run (X.E e x) Unit
type Xa_ x = Run.Run (X.A x) Unit
type Xrw_ x r w = Run.Run (X.W w (X.R r x)) Unit
type Xrs_ x r s = Run.Run (X.S s (X.R r x)) Unit
type Xre_ x r e = Run.Run (X.E e (X.R r x)) Unit
type Xra_ x r = Run.Run (X.A (X.R r x)) Unit
type Xws_ x w s = Run.Run (X.S s (X.W w x)) Unit
type Xwe_ x w e = Run.Run (X.E e (X.W w x)) Unit
type Xwa_ x w = Run.Run (X.A (X.W w x)) Unit
type Xse_ x s e = Run.Run (X.E e (X.S s x)) Unit
type Xsa_ x s = Run.Run (X.A (X.S s x)) Unit
type Xea_ x e = Run.Run (X.A (X.E e x)) Unit
type X_r_ r = Run.Run (X.R r ()) Unit
type X_w_ w = Run.Run (X.W w ()) Unit
type X_s_ s = Run.Run (X.S s ()) Unit
type X_e_ e = Run.Run (X.E e ()) Unit
type X_a_ = Run.Run (X.A ()) Unit
type X_rw_ r w = Run.Run (X.W w (X.R r ())) Unit
type X_rs_ r s = Run.Run (X.S s (X.R r ())) Unit
type X_re_ r e = Run.Run (X.E e (X.R r ())) Unit
type X_ra_ r = Run.Run (X.A (X.R r ())) Unit
type X_ws_ w s = Run.Run (X.S s (X.W w ())) Unit
type X_we_ w e = Run.Run (X.E e (X.W w ())) Unit
type X_wa_ w = Run.Run (X.A (X.W w ())) Unit
type X_se_ s e = Run.Run (X.E e (X.S s ())) Unit
type X_sa_ s = Run.Run (X.A (X.S s ())) Unit
type X_ea_ e = Run.Run (X.A (X.E e ())) Unit

xUpdate :: forall a. a -> X_s a Unit -> a
xUpdate init m = Run.extract $ RunS.execState init m

type ModX a = X_s_ a

xMod :: forall a. a -> ModX a -> a
xMod init m = Run.extract $ RunS.execState init m