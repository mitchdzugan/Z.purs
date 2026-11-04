module Z.Z.It
  ( Iterator
  , class Collectable
  , class Iterable
  , collect
  , it'forM
  , it'forM_
  , it'process
  , iterable'mk
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Unsafe.Coerce (unsafeCoerce)
import Z.Z.Core (eOr'_, may'_, may'__)

foreign import data Iterator :: Type -> Type

foreign import data Raw'Iterator :: Type -> Type

foreign import data Obj'Entry :: Type -> Type

instance Functor Iterator where
  map = js_it_map

class Iterable it el | it -> el where
  iterable'mk :: it -> Iterator el

instance Iterable (Iterator el) el where
  iterable'mk = identity

instance Iterable (Array el) el where
  iterable'mk = js_it_ofArray

instance Iterable (Object el) el where
  iterable'mk obj = js_it_ofObj (/\) obj <#> snd

class Iterable'KV it k v where
  iterable'kv'mk :: it -> Iterator (k /\ v)

instance Iterable'KV (Array a) Int a where
  iterable'kv'mk = js_it_withInd (/\) <<< js_it_ofArray
else instance Iterable'KV (Object a) String a where
  iterable'kv'mk = js_it_ofObj (/\)
else instance Iterable'KV (Iterator (k /\ v)) k v where
  iterable'kv'mk = identity
else instance Iterable'KV (Iterator a) Int a where
  iterable'kv'mk = js_it_withInd (/\)
else instance Iterable it (k /\ v) => Iterable'KV it k v where
  iterable'kv'mk = iterable'mk

class Collectable coll el | coll -> el where
  collect :: Iterator el -> coll

instance Collectable (Array a) a where
  collect = js_it_toArray

instance Collectable (Object a) (String /\ a) where
  collect = js_it_toObj <<< map (uncurry js_obj_entry)

rawIt'start :: forall a. Iterator a -> Effect (Raw'Iterator a)
rawIt'start = unsafeCoerce

rawIt'next :: forall a. Raw'Iterator a -> Effect (Maybe a)
rawIt'next rawIt = js_rawIt_next rawIt <#> \r ->
  if r.done then Nothing else Just r.value

rawIt'process
  :: forall m carry el res
   . MonadRec m
  => (carry -> el -> m (Either carry res))
  -> (carry -> m res)
  -> carry
  -> Raw'Iterator el
  -> m res
rawIt'process onEl onDone init rawIt = tailRecM loop (Left init)
  where
  loop resOr_ = eOr'_ loopStep (pure <<< Done) resOr_
  loopStep carry = may'__
    (\_ -> Done <$> onDone carry)
    (\el -> onEl carry el <#> eOr'_ (Loop <<< Left) Done)
    (unsafePerformEffect $ rawIt'next rawIt)

it'process
  :: forall it m carry el res
   . MonadRec m
  => Iterable it el
  => (carry -> el -> m (Either carry res))
  -> (carry -> m res)
  -> carry
  -> it
  -> m res
it'process onEl onDone init it =
  rawIt'process onEl onDone init
    (unsafePerformEffect $ rawIt'start $ iterable'mk it)

it'forM_
  :: forall it m el
   . MonadRec m
  => Iterable it el
  => it
  -> (el -> m Unit)
  -> m Unit
it'forM_ it f = it'process (\_ el -> Left <$> f el) pure unit it

it'forM
  :: forall it m el a
   . MonadRec m
  => Iterable it el
  => it
  -> (el -> m a)
  -> m (Array a)
it'forM it f = do
  partial <- pure unit <#> \_ -> unsafePerformEffect js_arrayPartial_new
  let cons a = unsafePerformEffect (js_arrayPartial_cons partial a <#> Left)
  let impl fLoop fDone = it'process fLoop fDone unit it
  impl (\_ el -> f el <#> cons) (\_ -> pure $ js_arrayPartial_complete partial)

foreign import js_it_ofArray :: forall a. Array a -> Iterator a

foreign import js_it_ofObj
  :: forall a
   . (String -> a -> String /\ a)
  -> Object a
  -> Iterator (String /\ a)

foreign import js_it_map :: forall a b. (a -> b) -> Iterator a -> Iterator b

foreign import js_it_withInd
  :: forall a. (Int -> a -> Int /\ a) -> Iterator a -> Iterator (Int /\ a)

foreign import js_rawIt_next
  :: forall a. Raw'Iterator a -> Effect { value :: a, done :: Boolean }

foreign import js_it_toArray :: forall a. Iterator a -> Array a

foreign import js_it_toObj :: forall a. Iterator (Obj'Entry a) -> Object a

foreign import js_obj_entry :: forall a. String -> a -> Obj'Entry a

foreign import data JsArrayPartial :: Type -> Type

foreign import js_arrayPartial_new :: forall a. Effect (JsArrayPartial a)

foreign import js_arrayPartial_cons
  :: forall a. JsArrayPartial a -> a -> Effect Unit

js_arrayPartial_complete :: forall a. JsArrayPartial a -> Array a
js_arrayPartial_complete = unsafeCoerce