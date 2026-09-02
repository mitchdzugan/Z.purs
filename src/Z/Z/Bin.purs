module Z.Z.Bin
  ( Bin'Eff'2d'R
  , Bin'Eff'2d'T
  , Bin'Eff'R
  , Bin'Eff'T
  , Bin(..)
  , bin'empty
  , bin'fromFoldable
  , bin'insert
  , bin'lookup
  , bin'size
  , bin'vals
  , xbin'clear
  , xbin'delete
  , xbin'freeze
  , xbin'insert
  , xbin'lookup
  , xbin'merge
  , xbin'run
  , xbin'setStart
  , xbin'size
  , xbin'start
  , xbin'vals
  , xbin2d'all
  , xbin2d'clear
  , xbin2d'clearAt
  , xbin2d'delete
  , xbin2d'freezeAt
  , xbin2d'insert
  , xbin2d'lookup
  , xbin2d'mergeAt
  , xbin2d'run
  , xbin2d'setStart
  , xbin2d'setStartAt
  , xbin2d'size
  , xbin2d'sizeAt
  , xbin2d'start
  , xbin2d'startAt
  , xbin2d'valsAt
  ) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Ref as Ref
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Foreign.Object as Fo
import Foreign.Object.ST as FoST
import Prim.Row (class Cons)
import Z.Z.Core (arr'fromFoldable, mapM)
import Z.Z.Defaultable (class Generable, g, g1)
import Z.Z.Ext (class IsSymbol, Maybe, Run, snd)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable, ident'key)
import Z.Z.X (class EffAdapter, Eff'At, XDoAsked, adapter'run, eff'tag)

newtype Bin a = Bin (Fo.Object a)

derive instance Z.Newtype (Bin a) _
derive instance Functor Bin

type EncodedBin v = Array { k :: String, v :: v }
newtype JsonEncodedBin = JsonEncodedBin (EncodedBin Z.Json)

bin'empty :: forall v. Bin v
bin'empty = Bin Fo.empty

bin'lookup :: forall k v. Identable k => k -> Bin v -> Z.Maybe v
bin'lookup k = Fo.lookup (ident'key k) <<< Z.unwrap

bin'insert :: forall k v. Identable k => k -> v -> Bin v -> Bin v
bin'insert k v (Bin o) = Bin $ Fo.insert (ident'key k) v o

bin'fromFoldable
  :: forall k v f. Identable k => Z.Foldable f => f (k Z./\ v) -> Bin v
bin'fromFoldable f = Bin $ Fo.fromFoldable $ flip map (arr'fromFoldable f)
  \(k Z./\ v) -> ident'key k Z./\ v

bin'size :: forall v. Bin v -> Int
bin'size = Fo.size <<< Z.unwrap

bin'vals :: forall v. Bin v -> Array v
bin'vals = Fo.values <<< Z.unwrap

derive instance Z.Generic JsonEncodedBin _

instance Z.DecodeJson JsonEncodedBin where
  decodeJson x = Z.genericDecodeJson x

instance Z.DecodeJson v => Z.DecodeJson (Bin v) where
  decodeJson x = do
    partial <- decodeJson x
    ty <- flip mapM (decodedKVs partial) $ \e -> decodeJson e.v <#> \f ->
      e.k Z./\ f
    pure $ bin'fromFoldable ty
    where
    decodedKVs (JsonEncodedBin els) = els

instance Z.EncodeJson v => Z.EncodeJson (Bin v) where
  encodeJson (Bin x) = Z.encodeJson $ Fo.toArrayWithKey (\k v -> { k, v }) x

instance Generable (Bin v) gdesc (Bin v) where
  mkGenerable = bin'empty

type Bin'Eff'R :: forall k. Type -> k -> Type
type Bin'Eff'R t p =
  { lookup :: String -> Eff'At p (Z.Maybe t)
  , insert :: String -> t -> Eff'At p Unit
  , size :: Eff'At p Int
  , vals :: Eff'At p (Array t)
  , clear :: Eff'At p Unit
  , delete :: String -> Eff'At p Unit
  , add :: Fo.Object t -> Eff'At p Unit
  , freeze :: Eff'At p (Fo.Object t)
  , start :: Eff'At p Int
  , setStart :: Int -> Eff'At p Unit
  }

data Bin'Eff'T :: forall k. k -> Type
data Bin'Eff'T t

data Bin'Eff'St :: forall k. k -> Type
data Bin'Eff'St t

foreign import js_binEff_new :: forall t. Z.Effect (Bin'Eff'St t)
foreign import js_binEff_lookup
  :: forall t
   . Z.Maybe t
  -> (t -> Z.Maybe t)
  -> String
  -> Bin'Eff'St t
  -> Z.Effect (Z.Maybe t)

foreign import js_binEff_insert
  :: forall t
   . Unit
  -> String
  -> t
  -> Bin'Eff'St t
  -> Z.Effect Unit

foreign import js_binEff_delete
  :: forall t
   . Unit
  -> String
  -> Bin'Eff'St t
  -> Z.Effect Unit

foreign import js_binEff_clear
  :: forall t
   . Unit
  -> Bin'Eff'St t
  -> Z.Effect Unit

foreign import js_binEff_addForeignObject
  :: forall t
   . Unit
  -> Fo.Object t
  -> Bin'Eff'St t
  -> Z.Effect Unit

foreign import js_binEff_toForeignObject
  :: forall t. Bin'Eff'St t -> Z.Effect (Fo.Object t)

foreign import js_binEff_size :: forall t. Bin'Eff'St t -> Z.Effect Int
foreign import js_binEff_start :: forall t. Bin'Eff'St t -> Z.Effect Int
foreign import js_binEff_setStart
  :: forall t. Unit -> Int -> Bin'Eff'St t -> Z.Effect Unit

foreign import js_binEff_2d_start :: forall t. Bin'Eff'2d'St t -> Z.Effect Int
foreign import js_binEff_2d_setStart
  :: forall t. Unit -> Int -> Bin'Eff'2d'St t -> Z.Effect Unit

foreign import js_binEff_2d_startAt
  :: forall t. String -> Bin'Eff'2d'St t -> Z.Effect Int

foreign import js_binEff_2d_setStartAt
  :: forall t. Unit -> String -> Int -> Bin'Eff'2d'St t -> Z.Effect Unit

foreign import js_binEff_vals :: forall t. Bin'Eff'St t -> Z.Effect (Array t)

instance EffAdapter (Bin'Eff'T t) p Unit (Bin'Eff'R t p) Unit where
  effAdapter'mk _ = js_binEff_new <#> \st ->
    { lookup: \k -> eff'tag @p $ js_binEff_lookup Z.Nothing Z.Just k st
    , insert: \k v -> eff'tag @p $ js_binEff_insert unit k v st
    , delete: \k -> eff'tag @p $ js_binEff_delete unit k st
    , clear: eff'tag @p $ js_binEff_clear unit st
    , size: eff'tag @p $ js_binEff_size st
    , vals: eff'tag @p $ js_binEff_vals st
    , add: \obj -> eff'tag @p $ js_binEff_addForeignObject unit obj st
    , freeze: eff'tag @p $ js_binEff_toForeignObject st
    , start: eff'tag @p $ js_binEff_start st
    , setStart: \start -> eff'tag @p $ js_binEff_setStart unit start st
    }
  effAdapter'res _ = eff'tag @p $ pure unit

type XBin_h' eff't p t x' x rest =
  IsSymbol p
  => Cons p (Z.Reader (Bin'Eff'R t p Z./\ Z.Proxy eff't)) x' x
  => rest

type XBin_hk eff't p t k x rest =
  forall x'
   . Identable k
  => IsSymbol p
  => Cons p (Z.Reader (Bin'Eff'R t p Z./\ Z.Proxy eff't)) x' x
  => rest

type XBin_h_ eff't p t x rest =
  forall x'
   . IsSymbol p
  => Cons p (Z.Reader (Bin'Eff'R t p Z./\ Z.Proxy eff't)) x' x
  => rest

xbin'run
  :: forall @p @t x' x a. XBin_h' (Bin'Eff'T t) p t x' x (Run x a -> Run x' a)
xbin'run m = adapter'run @(Bin'Eff'T t) @p unit m <#> snd

xbin'lookup
  :: forall @eff't @p t x k. XBin_hk eff't p t k x (k -> Run x (Z.Maybe t))
xbin'lookup k = g1 @XDoAsked @p \r -> (Z.fst r).lookup (ident'key k)

xbin'insert
  :: forall @eff't @p t x k. XBin_hk eff't p t k x (k -> t -> Run x Unit)
xbin'insert k v = g1 @XDoAsked @p \r -> (Z.fst r).insert (ident'key k) v

xbin'delete :: forall @eff't @p t x k. XBin_hk eff't p t k x (k -> Run x Unit)
xbin'delete k = g1 @XDoAsked @p \r -> (Z.fst r).delete (ident'key k)

xbin'clear :: forall @eff't @p t x. XBin_h_ eff't p t x (Run x Unit)
xbin'clear = g1 @XDoAsked @p \r -> (Z.fst r).clear

xbin'size :: forall @eff't @p t x. XBin_h_ eff't p t x (Run x Int)
xbin'size = g1 @XDoAsked @p (_.size <<< Z.fst)

xbin'vals :: forall @eff't @p t x. XBin_h_ eff't p t x (Run x (Array t))
xbin'vals = g1 @XDoAsked @p (_.vals <<< Z.fst)

xbin'merge :: forall @eff't @p t x. XBin_h_ eff't p t x (Bin t -> Run x Unit)
xbin'merge (Bin obj) = g1 @XDoAsked @p \r -> (Z.fst r).add obj

xbin'freeze :: forall @eff't @p t x. XBin_h_ eff't p t x (Run x (Bin t))
xbin'freeze = Bin <$> g1 @XDoAsked @p \r -> (Z.fst r).freeze

xbin'start :: forall @eff't @p t x. XBin_h_ eff't p t x (Run x Int)
xbin'start = g1 @XDoAsked @p \r -> (Z.fst r).start

xbin'setStart :: forall @eff't @p t x. XBin_h_ eff't p t x (Int -> Run x Unit)
xbin'setStart start = g1 @XDoAsked @p \r -> (Z.fst r).setStart start

data Bin'Eff'2d'T :: forall k. k -> Type
data Bin'Eff'2d'T t

data Bin'Eff'2d'St :: forall k. k -> Type
data Bin'Eff'2d'St t

type Bin'Eff'2d'R :: forall k. Type -> k -> Type
type Bin'Eff'2d'R t p =
  { lookup :: String -> String -> Eff'At p (Z.Maybe t)
  , insert :: String -> String -> t -> Eff'At p Unit
  , delete :: String -> String -> Eff'At p Unit
  , size :: Eff'At p Int
  , clear :: Eff'At p Unit
  , sizeAt :: String -> Eff'At p Int
  , valsAt :: String -> Eff'At p (Array t)
  , all :: Eff'At p (Array t)
  , clearAt :: String -> Eff'At p Unit
  , addAt :: String -> Fo.Object t -> Eff'At p Unit
  , freezeAt :: String -> Eff'At p (Fo.Object t)
  , start :: Eff'At p Int
  , setStart :: Int -> Eff'At p Unit
  , startAt :: String -> Eff'At p Int
  , setStartAt :: String -> Int -> Eff'At p Unit
  }

instance EffAdapter (Bin'Eff'2d'T t) p Unit (Bin'Eff'2d'R t p) Unit where
  effAdapter'mk _ = js_binEff_2d_new <#> \st ->
    { lookup:
        \k1 k2 -> eff'tag @p $ js_binEff_2d_lookup Z.Nothing Z.Just k1 k2 st
    , insert: \k1 k2 v -> eff'tag @p $ js_binEff_2d_insert unit k1 k2 v st
    , delete: \k1 k2 -> eff'tag @p $ js_binEff_2d_delete unit k1 k2 st
    , size: eff'tag @p $ js_binEff_2d_size st
    , clear: eff'tag @p $ js_binEff_2d_clear unit st
    , sizeAt: \k -> eff'tag @p $ js_binEff_2d_sizeAt k st
    , valsAt: \k -> eff'tag @p $ js_binEff_2d_vals k st
    , all: eff'tag @p $ js_binEff_2d_all st
    , clearAt: \k -> eff'tag @p $ js_binEff_2d_clearAt unit k st
    , addAt:
        \k obj -> eff'tag @p $ js_binEff_2d_addForeignObjectAt unit k obj st
    , freezeAt: \k -> eff'tag @p $ js_binEff_2d_toForeignObjectAt k st
    , start: eff'tag @p $ js_binEff_2d_start st
    , setStart: \start -> eff'tag @p $ js_binEff_2d_setStart unit start st
    , startAt: \k -> eff'tag @p $ js_binEff_2d_startAt k st
    , setStartAt:
        \k start -> eff'tag @p $ js_binEff_2d_setStartAt unit k start st
    }
  effAdapter'res _ = eff'tag @p $ pure unit

foreign import js_binEff_2d_new :: forall t. Z.Effect (Bin'Eff'2d'St t)
foreign import js_binEff_2d_lookup
  :: forall t
   . Z.Maybe t
  -> (t -> Z.Maybe t)
  -> String
  -> String
  -> Bin'Eff'2d'St t
  -> Z.Effect (Z.Maybe t)

foreign import js_binEff_2d_insert
  :: forall t
   . Unit
  -> String
  -> String
  -> t
  -> Bin'Eff'2d'St t
  -> Z.Effect Unit

foreign import js_binEff_2d_delete
  :: forall t
   . Unit
  -> String
  -> String
  -> Bin'Eff'2d'St t
  -> Z.Effect Unit

foreign import js_binEff_2d_clear
  :: forall t
   . Unit
  -> Bin'Eff'2d'St t
  -> Z.Effect Unit

foreign import js_binEff_2d_clearAt
  :: forall t
   . Unit
  -> String
  -> Bin'Eff'2d'St t
  -> Z.Effect Unit

foreign import js_binEff_2d_addForeignObjectAt
  :: forall t
   . Unit
  -> String
  -> Fo.Object t
  -> Bin'Eff'2d'St t
  -> Z.Effect Unit

foreign import js_binEff_2d_toForeignObjectAt
  :: forall t. String -> Bin'Eff'2d'St t -> Z.Effect (Fo.Object t)

foreign import js_binEff_2d_size :: forall t. Bin'Eff'2d'St t -> Z.Effect Int
foreign import js_binEff_2d_sizeAt
  :: forall t. String -> Bin'Eff'2d'St t -> Z.Effect Int

foreign import js_binEff_2d_vals
  :: forall t. String -> Bin'Eff'2d'St t -> Z.Effect (Array t)

foreign import js_binEff_2d_all
  :: forall t. Bin'Eff'2d'St t -> Z.Effect (Array t)

xbin2d'run
  :: forall @p @t x' x a
   . XBin2d_h' (Bin'Eff'2d'T t) p t x' x (Run x a -> Run x' a)
xbin2d'run m = adapter'run @(Bin'Eff'2d'T t) @p unit m <#> snd

xbin2d'lookup
  :: forall @eff't @p t x k1 k2
   . XBin2d_hk eff't p t k1 k2 x (k1 -> k2 -> Run x (Z.Maybe t))
xbin2d'lookup k1 k2 = g1 @XDoAsked @p \r -> (Z.fst r).lookup (ident'key k1)
  (ident'key k2)

xbin2d'insert
  :: forall @eff't @p t x k1 k2
   . XBin2d_hk eff't p t k1 k2 x (k1 -> k2 -> t -> Run x Unit)
xbin2d'insert k1 k2 v = g1 @XDoAsked @p \r -> (Z.fst r).insert (ident'key k1)
  (ident'key k2)
  v

xbin2d'delete
  :: forall @eff't @p t x k1 k2
   . XBin2d_hk eff't p t k1 k2 x (k1 -> k2 -> Run x Unit)
xbin2d'delete k1 k2 = g1 @XDoAsked @p \r -> (Z.fst r).delete (ident'key k1)
  (ident'key k2)

xbin2d'clear :: forall @eff't @p t x. XBin2d_h_ eff't p t x (Run x Unit)
xbin2d'clear = g1 @XDoAsked @p \r -> (Z.fst r).clear

xbin2d'size :: forall @eff't @p t x. XBin2d_h_ eff't p t x (Run x Int)
xbin2d'size = g1 @XDoAsked @p \r -> (Z.fst r).size

xbin2d'clearAt
  :: forall @eff't @p t k x. XBin2d_hk1 eff't p t k x (k -> Run x Unit)
xbin2d'clearAt k = g1 @XDoAsked @p \r -> (Z.fst r).clearAt (ident'key k)

xbin2d'sizeAt
  :: forall @eff't @p t k x. XBin2d_hk1 eff't p t k x (k -> Run x Int)
xbin2d'sizeAt k = g1 @XDoAsked @p \r -> (Z.fst r).sizeAt (ident'key k)

xbin2d'valsAt
  :: forall @eff't @p t k x. XBin2d_hk1 eff't p t k x (k -> Run x (Array t))
xbin2d'valsAt k = g1 @XDoAsked @p \r -> (Z.fst r).valsAt (ident'key k)

xbin2d'all :: forall @eff't @p t x. XBin2d_h_ eff't p t x (Run x (Array t))
xbin2d'all = g1 @XDoAsked @p \r -> (Z.fst r).all

xbin2d'mergeAt
  :: forall @eff't @p t k x. XBin2d_hk1 eff't p t k x (k -> Bin t -> Run x Unit)
xbin2d'mergeAt k (Bin obj) = g1 @XDoAsked @p \r -> (Z.fst r).addAt (ident'key k)
  obj

xbin2d'freezeAt
  :: forall @eff't @p t k x. XBin2d_hk1 eff't p t k x (k -> Run x (Bin t))
xbin2d'freezeAt k = Bin <$> g1 @XDoAsked @p \r -> (Z.fst r).freezeAt
  (ident'key k)

xbin2d'start :: forall @eff't @p t k x. XBin2d_hk1 eff't p t k x (Run x Int)
xbin2d'start = g1 @XDoAsked @p \r -> (Z.fst r).start

xbin2d'setStart
  :: forall @eff't @p t k x. XBin2d_hk1 eff't p t k x (Int -> Run x Unit)
xbin2d'setStart start = g1 @XDoAsked @p \r -> (Z.fst r).setStart start

xbin2d'startAt
  :: forall @eff't @p t k x. XBin2d_hk1 eff't p t k x (k -> Run x Int)
xbin2d'startAt k = g1 @XDoAsked @p \r -> (Z.fst r).startAt (ident'key k)

xbin2d'setStartAt
  :: forall @eff't @p t k x. XBin2d_hk1 eff't p t k x (k -> Int -> Run x Unit)
xbin2d'setStartAt k start = g1 @XDoAsked @p \r -> (Z.fst r).setStartAt
  (ident'key k)
  start

type XBin2d_h' eff't p t x' x rest =
  IsSymbol p
  => Cons p (Z.Reader (Bin'Eff'2d'R t p Z./\ Z.Proxy eff't)) x' x
  => rest

type XBin2d_hk eff't p t k1 k2 x rest =
  forall x'
   . Identable k1
  => Identable k2
  => IsSymbol p
  => Cons p (Z.Reader (Bin'Eff'2d'R t p Z./\ Z.Proxy eff't)) x' x
  => rest

type XBin2d_hk1 eff't p t k x rest =
  forall x'
   . Identable k
  => IsSymbol p
  => Cons p (Z.Reader (Bin'Eff'2d'R t p Z./\ Z.Proxy eff't)) x' x
  => rest

type XBin2d_h_ eff't p t x rest =
  forall x'
   . IsSymbol p
  => Cons p (Z.Reader (Bin'Eff'2d'R t p Z./\ Z.Proxy eff't)) x' x
  => rest