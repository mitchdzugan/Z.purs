module Z.Z.Run where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Effect (Effect)

foreign import data Rdr :: Row Type -> Type -> Type

-----------------------------------------------------------

instance Apply (Rdr x) where
  apply = js_r_apply

instance Applicative (Rdr x) where
  pure = js_r_pure

instance Functor (Rdr x) where
  map = js_r_map

instance Bind (Rdr x) where
  bind = js_r_bind

instance Monad (Rdr x)

instance MonadRec (Rdr x) where
  tailRecM = js_r_tailRecM useStep

-----------------------------------------------------------

foreign import js_r_pure :: forall v x. v -> Rdr x v

foreign import js_r_map :: forall v1 v2 x. (v1 -> v2) -> Rdr x v1 -> Rdr x v2

foreign import js_r_apply
  :: forall v1 v2 x. (Rdr x (v1 -> v2)) -> Rdr x v1 -> Rdr x v2

foreign import js_r_bind
  :: forall v1 v2 x. Rdr x v1 -> (v1 -> Rdr x v2) -> Rdr x v2

foreign import js_r_tailRecM
  :: forall a b x. T'useStep a b -> (a -> Rdr x (Step a b)) -> a -> Rdr x b

type T'useStep a b =
  (a -> Effect Unit) -> (b -> Effect Unit) -> Step a b -> Effect Unit

useStep :: forall a b. T'useStep a b
useStep useLoop _ (Loop a) = useLoop a
useStep _ useDone (Done b) = useDone b