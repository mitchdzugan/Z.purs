module Z.Z.Eff.Ref
  ( Eff'Ref
  , eff'ref'get
  , eff'ref'new
  , eff'ref'set
  ) where

import Prelude

import Effect (Effect)

foreign import data Eff'Ref :: forall k. k -> Type

foreign import js_ref_new :: forall t. t -> Effect (Eff'Ref t)
foreign import js_ref_get :: forall t. Eff'Ref t -> Effect t
foreign import js_ref_set :: forall t. Unit -> t -> Eff'Ref t -> Effect Unit

eff'ref'new :: forall t. t -> Effect (Eff'Ref t)
eff'ref'new = js_ref_new

eff'ref'get :: forall t. Eff'Ref t -> Effect t
eff'ref'get = js_ref_get

eff'ref'set :: forall t. t -> Eff'Ref t -> Effect Unit
eff'ref'set = js_ref_set unit