module Z.Z.Eff.Bin2D
  ( Eff'Bin2D
  , eff'bin2D'addAt
  , eff'bin2D'clear
  , eff'bin2D'clearAt
  , eff'bin2D'd1keys
  , eff'bin2D'delete
  , eff'bin2D'freezeAt
  , eff'bin2D'insert
  , eff'bin2D'lookup
  , eff'bin2D'new
  , eff'bin2D'size
  , eff'bin2D'sizeAt
  , eff'bin2D'valsAt
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.Object (Object)
import Z.Z.Bin (Bin(..))
import Z.Z.Id (class Identable, ident'key)

data Eff'Bin2D :: Type -> Type -> Type
data Eff'Bin2D k t

eff'bin2D'new :: forall k1 @t. Effect (Eff'Bin2D k1 t)
eff'bin2D'new = js_binEff_2d_new

eff'bin2D'lookup
  :: forall @t k1 k2
   . Identable k1
  => Identable k2
  => k1
  -> k2
  -> Eff'Bin2D k1 t
  -> Effect (Maybe t)
eff'bin2D'lookup k1 k2 =
  js_binEff_2d_lookup Nothing Just (ident'key k1) (ident'key k2)

eff'bin2D'insert
  :: forall @t k1 k2
   . Identable k1
  => Identable k2
  => k1
  -> k2
  -> t
  -> Eff'Bin2D k1 t
  -> Effect Unit
eff'bin2D'insert k1 k2 v =
  js_binEff_2d_insert unit k1 (ident'key k1) (ident'key k2) v

eff'bin2D'delete
  :: forall @t k1 k2
   . Identable k1
  => Identable k2
  => k1
  -> k2
  -> Eff'Bin2D k1 t
  -> Effect Unit
eff'bin2D'delete k1 k2 =
  js_binEff_2d_delete unit (ident'key k1) (ident'key k2)

eff'bin2D'clear :: forall @k1 @t. Eff'Bin2D k1 t -> Effect Unit
eff'bin2D'clear = js_binEff_2d_clear unit

eff'bin2D'clearAt
  :: forall @t k1. Identable k1 => k1 -> Eff'Bin2D k1 t -> Effect Unit
eff'bin2D'clearAt k1 = js_binEff_2d_clearAt unit (ident'key k1)

eff'bin2D'addAt
  :: forall @t k1. Identable k1 => k1 -> Bin t -> Eff'Bin2D k1 t -> Effect Unit
eff'bin2D'addAt k1 (Bin obj) =
  js_binEff_2d_addForeignObjectAt unit k1 (ident'key k1) obj

eff'bin2D'freezeAt
  :: forall @t k1. Identable k1 => k1 -> Eff'Bin2D k1 t -> Effect (Bin t)
eff'bin2D'freezeAt k1 =
  map Bin <<< js_binEff_2d_toForeignObjectAt (ident'key k1)

eff'bin2D'valsAt
  :: forall @t k1. Identable k1 => k1 -> Eff'Bin2D k1 t -> Effect (Array t)
eff'bin2D'valsAt k1 = js_binEff_2d_vals (ident'key k1)

eff'bin2D'sizeAt
  :: forall @t k1. Identable k1 => k1 -> Eff'Bin2D k1 t -> Effect Int
eff'bin2D'sizeAt k1 = js_binEff_2d_sizeAt (ident'key k1)

eff'bin2D'size :: forall k1 @t. Eff'Bin2D k1 t -> Effect Int
eff'bin2D'size = js_binEff_2d_size

eff'bin2D'd1keys :: forall k1 @t. Eff'Bin2D k1 t -> Effect (Array k1)
eff'bin2D'd1keys = js_binEff_2d_d1keys

foreign import js_binEff_2d_new :: forall k1 t. Effect (Eff'Bin2D k1 t)
foreign import js_binEff_2d_lookup
  :: forall k1 t
   . Maybe t
  -> (t -> Maybe t)
  -> String
  -> String
  -> Eff'Bin2D k1 t
  -> Effect (Maybe t)

foreign import js_binEff_2d_insert
  :: forall k1 t
   . Unit
  -> k1
  -> String
  -> String
  -> t
  -> Eff'Bin2D k1 t
  -> Effect Unit

foreign import js_binEff_2d_delete
  :: forall k1 t
   . Unit
  -> String
  -> String
  -> Eff'Bin2D k1 t
  -> Effect Unit

foreign import js_binEff_2d_clear
  :: forall k1 t
   . Unit
  -> Eff'Bin2D k1 t
  -> Effect Unit

foreign import js_binEff_2d_clearAt
  :: forall k1 t
   . Unit
  -> String
  -> Eff'Bin2D k1 t
  -> Effect Unit

foreign import js_binEff_2d_addForeignObjectAt
  :: forall k1 t
   . Unit
  -> k1
  -> String
  -> Object t
  -> Eff'Bin2D k1 t
  -> Effect Unit

foreign import js_binEff_2d_toForeignObjectAt
  :: forall k1 t. String -> Eff'Bin2D k1 t -> Effect (Object t)

foreign import js_binEff_2d_size :: forall k1 t. Eff'Bin2D k1 t -> Effect Int
foreign import js_binEff_2d_sizeAt
  :: forall k1 t. String -> Eff'Bin2D k1 t -> Effect Int

foreign import js_binEff_2d_vals
  :: forall k1 t. String -> Eff'Bin2D k1 t -> Effect (Array t)

foreign import js_binEff_2d_d1keys
  :: forall k1 t. Eff'Bin2D k1 t -> Effect (Array k1)