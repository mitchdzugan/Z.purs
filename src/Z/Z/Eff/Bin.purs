module Z.Z.Eff.Bin where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.Object (Object)
import Z.Z.Bin (Bin(..))
import Z.Z.Id (class Identable, ident'key)

data Eff'Bin :: forall k. k -> Type
data Eff'Bin t

foreign import js_binEff_new :: forall t. Effect (Eff'Bin t)
foreign import js_binEff_lookup
  :: forall t
   . Maybe t
  -> (t -> Maybe t)
  -> String
  -> Eff'Bin t
  -> Effect (Maybe t)

foreign import js_binEff_insert
  :: forall t
   . Unit
  -> String
  -> t
  -> Eff'Bin t
  -> Effect Unit

foreign import js_binEff_delete
  :: forall t
   . Unit
  -> String
  -> Eff'Bin t
  -> Effect Unit

foreign import js_binEff_clear
  :: forall t
   . Unit
  -> Eff'Bin t
  -> Effect Unit

foreign import js_binEff_addForeignObject
  :: forall t
   . Unit
  -> Object t
  -> Eff'Bin t
  -> Effect Unit

foreign import js_binEff_toForeignObject
  :: forall t. Eff'Bin t -> Effect (Object t)

foreign import js_binEff_size :: forall t. Eff'Bin t -> Effect Int

foreign import js_binEff_vals :: forall t. Eff'Bin t -> Effect (Array t)

eff'bin'new :: forall @t. Effect (Eff'Bin t)
eff'bin'new = js_binEff_new

eff'bin'lookup :: forall @t i. Identable i => i -> Eff'Bin t -> Effect (Maybe t)
eff'bin'lookup i = js_binEff_lookup Nothing Just $ ident'key i

eff'bin'insert :: forall @t i. Identable i => i -> t -> Eff'Bin t -> Effect Unit
eff'bin'insert i = js_binEff_insert unit $ ident'key i

eff'bin'delete :: forall @t i. Identable i => i -> Eff'Bin t -> Effect Unit
eff'bin'delete i = js_binEff_delete unit $ ident'key i

eff'bin'clear :: forall @t. Eff'Bin t -> Effect Unit
eff'bin'clear = js_binEff_clear unit

eff'bin'size :: forall @t. Eff'Bin t -> Effect Int
eff'bin'size = js_binEff_size

eff'bin'vals :: forall @t. Eff'Bin t -> Effect (Array t)
eff'bin'vals = js_binEff_vals

eff'bin'add :: forall @t. Bin t -> Eff'Bin t -> Effect Unit
eff'bin'add (Bin obj) = js_binEff_addForeignObject unit obj

eff'bin'freeze :: forall @t. Eff'Bin t -> Effect (Bin t)
eff'bin'freeze = map Bin <<< js_binEff_toForeignObject