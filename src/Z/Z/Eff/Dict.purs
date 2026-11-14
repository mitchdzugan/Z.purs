module Z.Z.Eff.Dict
  ( Dict
  , Dict'T
  , DictK'Hole
  , DictStage'Const
  , DictStage'Eff
  , Eff'Dict
  , _0
  , class DictK'Path
  , class Unidentity
  , dict'addDim
  , dict'collect
  , dict'entriesAt
  , dict'keysAt
  , dict'lookup
  , dict'sizeAt
  , dict'subDictAt
  , dictK'fullEntry
  , dictK'path
  , eff'dict'addAt
  , eff'dict'entriesAt
  , eff'dict'freezeAt
  , eff'dict'insert
  , eff'dict'keysAt
  , eff'dict'lookup
  , eff'dict'new
  , eff'dict'sizeAt
  , unidentity
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), over1, (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)
import Z.Z.Core (JsAny, T2'0, T2'1, ffmap)
import Z.Z.Defaultable (class Generable, GDefault)
import Z.Z.Id (class IdentP, class IdentP'Head, id'jsVal, ident'key, identP'els)
import Z.Z.It
  ( class Collectable
  , class Iterable
  , forM_
  , it'collect
  , iterable'mk
  )
import Z.Z.Util (arr'first)

toInternal :: forall k. IdentP k => k -> Array { s :: String, k :: JsAny }
toInternal k = identP'els @k k <#> \i -> { s: ident'key i, k: id'jsVal i }

eff'dict'new :: forall @k @v. IdentP k => Effect (Eff'Dict k v)
eff'dict'new = js_dict_fresh toInternal

eff'dict'insert :: forall @k @v. k -> v -> Eff'Dict k v -> Effect Unit
eff'dict'insert = js_dict_insert

eff'dict'addAt
  :: forall @param it @k @v wrap ent
   . IdentP k
  => Iterable it ent
  => DictK'Path param k wrap
  => Unidentity (wrap v) ent
  => param
  -> it
  -> Eff'Dict k v
  -> Effect Unit
eff'dict'addAt param it d = forM_ it \ent -> do
  let k /\ v = dictK'fullEntry param ent
  eff'dict'insert k v d

--------------------

instance
  ( IdentP k
  ) =>
  Iterable (Dict k v) (k /\ v) where
  iterable'mk = iterable'mk <<< dict'entriesAt @DictK'Hole @k _0

instance IdentP k => Collectable Dict (k /\ v) (Dict k v) where
  collect it = unsafePerformEffect do
    effDict <- eff'dict'new @k @v
    eff'dict'addAt _0 it effDict
    pure (js_dict_freeze effDict)

instance
  ( IdentP k
  ) =>
  Functor (Dict k) where
  map :: forall a b. (a -> b) -> Dict k a -> Dict k b
  map f d = dict'collect $ ffmap f $ iterable'mk d

instance
  ( IdentP k
  ) =>
  Generable (Dict k v) GDefault (Dict k v) where
  mkGenerable = dict'collect []

dict'collect
  :: forall it k v. IdentP k => Iterable it (k /\ v) => it -> Dict k v
dict'collect = it'collect @Dict

--------------------

type Dict'sel tsel k v = Dict'T (tsel DictStage'Eff DictStage'Const) k v

type DictRes'sel :: (Type -> Type -> Type) -> Type -> Type
type DictRes'sel tsel res = tsel (Effect res) res

type T'dict'lookup tsel k v =
  IdentP k => k -> Dict'sel tsel k v -> DictRes'sel tsel (Maybe v)

eff'dict'lookup :: forall k v. T'dict'lookup T2'0 k v
eff'dict'lookup k d = arr'first <$> entriesAtImpl @k @k @Identity k d

dict'lookup :: forall k v. T'dict'lookup T2'1 k v
dict'lookup k = dict'adaptQuery (eff'dict'lookup k)

--------------------

class Unidentity tIn tOut | tIn -> tOut where
  unidentity :: tIn -> tOut

instance Unidentity (Identity a) a where
  unidentity (Identity a) = a
else instance Unidentity a a where
  unidentity = identity

entriesAtImpl
  :: forall @param @k v @wrap ent
   . DictK'Path param k wrap
  => Unidentity (wrap v) ent
  => param
  -> Eff'Dict k v
  -> Effect (Array ent)
entriesAtImpl =
  js_dict_entriesAt over1 (/\) <<< dictK'path @param @k

type T'dict'entriesAt tsel param k v =
  forall restK
   . DictK'Path param k ((/\) restK)
  => param
  -> Dict'sel tsel k v
  -> DictRes'sel tsel (Array (restK /\ v))

eff'dict'entriesAt :: forall @param @k @v. T'dict'entriesAt T2'0 param k v
eff'dict'entriesAt = entriesAtImpl @param @k

dict'entriesAt :: forall @param @k @v. T'dict'entriesAt T2'1 param k v
dict'entriesAt param = dict'adaptQuery (eff'dict'entriesAt param)

--------------------

type T'dict'keysAt tsel param k v =
  forall nextK restK
   . DictK'Path param k ((/\) restK)
  => IdentP'Head restK nextK
  => (param -> Dict'sel tsel k v -> DictRes'sel tsel (Array nextK))

eff'dict'keysAt :: forall @param @k @v. T'dict'keysAt T2'0 param k v
eff'dict'keysAt param = js_dict_keysAt $ dictK'path @param @k param

dict'keysAt :: forall @param @k @v. T'dict'keysAt T2'1 param k v
dict'keysAt param = dict'adaptQuery (eff'dict'keysAt param)

--------------------

type T'dict'freezeAt tsel param k v =
  forall restK
   . DictK'Path param k ((/\) restK)
  => IdentP restK
  => (param -> Dict'sel tsel k v -> DictRes'sel tsel (Dict restK v))

eff'dict'freezeAt :: forall @param @k @v. T'dict'freezeAt T2'0 param k v
eff'dict'freezeAt param d = dict'collect <$> eff'dict'entriesAt param d

dict'subDictAt :: forall @param @k @v. T'dict'freezeAt T2'1 param k v
dict'subDictAt param = dict'adaptQuery (eff'dict'freezeAt param)

dict'addDim
  :: forall @k1 @k2 @v
   . IdentP k1
  => IdentP k2
  => DictK'Path k1 (k1 /\ k2) ((/\) k2)
  => IdentP'Head (k1 /\ k2) k1
  => Dict (k1 /\ k2) v
  -> Dict k1 (Dict k2 v)
dict'addDim dIn = dict'collect $ dict'keysAt _0 dIn <#> \k ->
  k /\ dict'subDictAt k dIn

--------------------

type T'dict'sizeAt tsel param k v =
  forall wrap
   . DictK'Path param k wrap
  => (param -> Dict'sel tsel k v -> tsel (Effect Int) Int)

eff'dict'sizeAt :: forall @param @k @v. T'dict'sizeAt T2'0 param k v
eff'dict'sizeAt param = js_dict_sizeAt $ dictK'path @param @k param

dict'sizeAt :: forall @param @k @v. T'dict'sizeAt T2'1 param k v
dict'sizeAt param = dict'adaptQuery (eff'dict'sizeAt param)

--------------------

dict'adaptQuery :: forall k v a. (Eff'Dict k v -> Effect a) -> Dict k v -> a
dict'adaptQuery q d = unsafePerformEffect (q (unsafeCoerce d))

--------------------

newtype DictK'Hole = DictK'Hole Unit

_0 :: DictK'Hole
_0 = DictK'Hole unit

class DictK'Path :: Type -> Type -> (Type -> Type) -> Constraint
class DictK'Path param k wrap | param k -> wrap where
  dictK'path :: param -> Array String
  dictK'fullEntry
    :: forall v ent. Unidentity (wrap v) ent => param -> ent -> k /\ v

instance DictK'Path DictK'Hole k ((/\) k) where
  dictK'path _ = []
  dictK'fullEntry
    :: forall v ent. Unidentity (k /\ v) ent => DictK'Hole -> ent -> k /\ v
  dictK'fullEntry _ = unsafeCoerce
else instance IdentP k => DictK'Path k k Identity where
  dictK'path = map ident'key <<< identP'els
  dictK'fullEntry
    :: forall v ent
     . Unidentity (Identity v) ent
    => k
    -> ent
    -> k /\ v
  dictK'fullEntry k ent = k /\ unsafeCoerce ent
else instance
  ( DictK'Path pr kr wrap
  , IdentP pl
  ) =>
  DictK'Path (pl /\ pr) (pl /\ kr) wrap where
  dictK'path (l /\ r) = map ident'key (identP'els l) <> dictK'path @pr @kr r
  dictK'fullEntry (pl /\ pr) ent =
    over1 ((/\) pl) $ dictK'fullEntry @pr @kr pr ent

-------------------------------------

foreign import data Dict'T :: Type -> Type -> Type -> Type

type Eff'Dict = Dict'T DictStage'Eff

type Dict = Dict'T DictStage'Const

foreign import data DictStage'Eff :: Type

foreign import data DictStage'Const :: Type

foreign import js_dict_fresh
  :: forall k v
   . (k -> Array { s :: String, k :: JsAny })
  -> Effect (Eff'Dict k v)

foreign import js_dict_freeze :: forall k v. Eff'Dict k v -> Dict k v

foreign import js_dict_entriesAt
  :: forall tl tl' tr k v ent
   . ((tl -> tl') -> tl /\ tr -> tl' /\ tr)
  -> (tl -> tr -> tl /\ tr)
  -> Array String
  -> Eff'Dict k v
  -> Effect (Array ent)

foreign import js_dict_sizeAt
  :: forall k v. Array String -> Eff'Dict k v -> Effect Int

foreign import js_dict_keysAt
  :: forall k v nextK. Array String -> Eff'Dict k v -> Effect (Array nextK)

foreign import js_dict_insert
  :: forall k v. k -> v -> Eff'Dict k v -> Effect Unit