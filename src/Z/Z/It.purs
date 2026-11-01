module Z.Z.It where

import Prelude

import Control.Monad.Rec.Class (Step(..))
import Data.Tuple.Nested (type (/\))
import Effect (Effect)

foreign import data Iterator :: Type -> Type

class Iterable it k v where
  iterable'mk :: it -> Iterator (k /\ v)

foreign import js_it_process
  :: forall carry el res
   . T'useStep carry res
  -> (carry -> el -> Step carry res)
  -> (carry -> res)
  -> carry
  -> Iterator el
  -> res

type T'useStep a b =
  (a -> Effect Unit) -> (b -> Effect Unit) -> Step a b -> Effect Unit

useStep :: forall a b. T'useStep a b
useStep useLoop _ (Loop a) = useLoop a
useStep _ useDone (Done b) = useDone b