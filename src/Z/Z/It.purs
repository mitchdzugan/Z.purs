module Z.Z.It
  ( Iterator
  , arr'collect
  , class Collectable
  , class Iterable
  , collect
  , forM
  , forM_
  , it'collect
  , it'fold
  , it'forM
  , it'forM_
  , it'intercalate
  , it'maximum
  , it'maximumWith
  , it'minimum
  , it'minimumWith
  , it'process
  , it'reduce
  , it'sum
  , it'sumBy
  , iterable'mk
  , list'collect
  , mapM
  , mapM_
  , obj'collect
  , reduce
  , reduceM
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Unsafe.Coerce (unsafeCoerce)
import Z.Z.Core (eOr'_, may'_, may'__, may'or)

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

instance Iterable (el /\ el) el where
  iterable'mk (l /\ r) = js_it_ofArray [ l, r ]

instance Iterable (Object el) el where
  iterable'mk obj = js_it_ofObj (/\) obj <#> snd

instance Iterable (List el) el where
  iterable'mk = js_it_ofList useList

type T'useList a =
  Effect Unit
  -> (a -> List a -> Effect Unit)
  -> List a
  -> Effect Unit

useList :: forall a. T'useList a
useList _ useCons (Cons head tail) = useCons head tail
useList useNil _ _ = useNil

class Iterable'KV it k v where
  iterable'kv'mk :: it -> Iterator (k /\ v)

instance Iterable'KV (Array a) Int a where
  iterable'kv'mk = js_it_withInd (/\) <<< js_it_ofArray
else instance Iterable'KV (List a) Int a where
  iterable'kv'mk = js_it_withInd (/\) <<< js_it_ofList useList
else instance Iterable'KV (Object a) String a where
  iterable'kv'mk = js_it_ofObj (/\)
else instance Iterable'KV (Iterator (k /\ v)) k v where
  iterable'kv'mk = identity
else instance Iterable'KV (Iterator a) Int a where
  iterable'kv'mk = js_it_withInd (/\)
else instance Iterable it (k /\ v) => Iterable'KV it k v where
  iterable'kv'mk = iterable'mk

class Collectable :: forall k. k -> Type -> Type -> Constraint
class Collectable coll el full where
  collect :: Iterator el -> full

instance Collectable Array a (Array a) where
  collect = js_it_toArray

instance Collectable Object (String /\ a) (Object a) where
  collect = js_it_toObj <<< map (uncurry js_obj_entry)

instance Collectable List a (List a) where
  collect = it'reduce (flip Cons) Nil

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

it'reduce :: forall it a b. Iterable it a => (b -> a -> b) -> b -> it -> b
it'reduce f s = unwrap <<< it'process (\c a -> wrap $ Left $ f c a) Identity s

it'sum :: forall it a. Iterable it a => Semiring a => it -> a
it'sum = it'reduce (+) zero

it'sumBy :: forall it b a. Iterable it b => Semiring a => (b -> a) -> it -> a
it'sumBy f it = it'sum $ iterable'mk it <#> f

it'extremeWith
  :: forall it b a
   . Iterable it b
  => Ord a
  => (a -> a -> Boolean)
  -> (b -> a)
  -> it
  -> Maybe b
it'extremeWith isMoreExtreme f it = it'reduce onEl Nothing it <#> fst
  where
  onEl best@(Just (_ /\ bestVal)) currEl = do
    let currVal = f currEl
    if isMoreExtreme currVal bestVal then Just (currEl /\ currVal) else best
  onEl _ currEl = Just (currEl /\ f currEl)

it'maximumWith
  :: forall it b a
   . Iterable it b
  => Ord a
  => (b -> a)
  -> it
  -> Maybe b
it'maximumWith = it'extremeWith (>)

it'minimumWith
  :: forall it b a
   . Iterable it b
  => Ord a
  => (b -> a)
  -> it
  -> Maybe b
it'minimumWith = it'extremeWith (<)

it'maximum
  :: forall it a
   . Iterable it a
  => Ord a
  => it
  -> Maybe a
it'maximum = it'maximumWith identity

it'minimum
  :: forall it a
   . Iterable it a
  => Ord a
  => it
  -> Maybe a
it'minimum = it'minimumWith identity

it'fold
  :: forall it m
   . Iterable it m
  => Monoid m
  => it
  -> m
it'fold = it'reduce (<>) mempty

it'intercalate
  :: forall it m
   . Iterable it m
  => Monoid m
  => m
  -> it
  -> m
it'intercalate inter = may'or mempty <<< flip it'reduce Nothing \carry el ->
  Just $ may'_ el (\els -> els <> inter <> el) carry

it'collect
  :: forall @tag it el full
   . Iterable it el
  => Collectable tag el full
  => it
  -> full
it'collect = collect @tag <<< iterable'mk

arr'collect :: forall it el. Iterable it el => it -> Array el
arr'collect = it'collect @Array

obj'collect :: forall it el. Iterable it (String /\ el) => it -> Object el
obj'collect = it'collect @Object

list'collect :: forall it el. Iterable it el => it -> List el
list'collect = it'collect @List

-------------------------------------------------------------------------------

mapM
  :: forall it m el a
   . MonadRec m
  => Iterable it el
  => (el -> m a)
  -> it
  -> m (Array a)
mapM = flip it'forM

forM
  :: forall it m el a
   . MonadRec m
  => Iterable it el
  => it
  -> (el -> m a)
  -> m (Array a)
forM = it'forM

mapM_
  :: forall it m el
   . MonadRec m
  => Iterable it el
  => (el -> m Unit)
  -> it
  -> m Unit
mapM_ = flip it'forM_

forM_
  :: forall it m el
   . MonadRec m
  => Iterable it el
  => it
  -> (el -> m Unit)
  -> m Unit
forM_ = it'forM_

reduce
  :: forall it el b
   . Iterable it el
  => (b -> el -> b)
  -> b
  -> it
  -> b
reduce = it'reduce

reduceM
  :: forall m it el b
   . MonadRec m
  => Iterable it el
  => (b -> el -> m b)
  -> b
  -> it
  -> m b
reduceM f s = it'process (\c a -> Left <$> f c a) pure s

-------------------------------------------------------------------------------

foreign import js_it_ofArray :: forall a. Array a -> Iterator a

foreign import js_it_ofList :: forall a. T'useList a -> List a -> Iterator a

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