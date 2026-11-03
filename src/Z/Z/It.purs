module Z.Z.It
  ( Iterator
  , class Iterable
  , it'process
  , iterable'mk
  , iterable'process
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..))
import Data.Tuple.Nested (type (/\))
import Effect (Effect)

foreign import data Iterator :: Type -> Type

class Iterable it k v where
  iterable'mk :: it -> Iterator (k /\ v)

it'process
  :: forall carry el res
   . (carry -> el -> Step carry res)
  -> (carry -> res)
  -> carry
  -> Iterator el
  -> res
it'process = js_it_process useStep

iterable'process
  :: forall it k v carry res
   . Iterable it k v
  => (carry -> (k /\ v) -> Step carry res)
  -> (carry -> res)
  -> carry
  -> it
  -> res
iterable'process onEl onDone init = it'process onEl onDone init <<< iterable'mk

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