module Z.Z.Ref where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Prim.Row (class Cons)
import Run (Run)
import Run.Reader (Reader)
import Type.Proxy (Proxy(..))
import Z.Z.Defaultable (class Generable, GDefault, g, g1)
import Z.Z.X
  ( class EffAdapter
  , class M'st'put
  , class ST'Spec'Label
  , Eff'At
  , XDoAsked
  , XST'Init(..)
  , adapter'run
  , eff'tag
  , effAdapter'mk
  , effAdapter'res
  )

foreign import data Ref'Eff'T :: forall k. k -> Type
foreign import data Ref'Eff'St :: forall k. k -> Type

foreign import js_ref_new :: forall t. t -> Effect (Ref'Eff'St t)
foreign import js_ref_get :: forall t. Ref'Eff'St t -> Effect t
foreign import js_ref_set :: forall t. Unit -> t -> Ref'Eff'St t -> Effect Unit

type Ref'Eff'R :: forall k. Type -> k -> Type
type Ref'Eff'R t p =
  { get :: Eff'At p t
  , set :: t -> Eff'At p Unit
  }

instance EffAdapter (Ref'Eff'T t) p t (Ref'Eff'R t p) t where
  effAdapter'mk i = js_ref_new i <#> \st ->
    { get: eff'tag @p $ js_ref_get st
    , set: \v -> eff'tag @p $ js_ref_set unit v st
    }
  effAdapter'res = _.get

type Ref'Using p t x' x rest =
  IsSymbol p
  => Cons p (Reader (Ref'Eff'R t p /\ Proxy (Ref'Eff'T t))) x' x
  => EffAdapter (Ref'Eff'T t) p t (Ref'Eff'R t p) t
  => rest

xref'run
  :: forall @p t x' x a. Ref'Using p t x' x (t -> Run x a -> Run x' (t /\ a))
xref'run = adapter'run @(Ref'Eff'T t) @p

xref'eval :: forall @p t x' x a. Ref'Using p t x' x (t -> Run x a -> Run x' a)
xref'eval i m = xref'run @p i m <#> snd

xref'exec :: forall @p t x' x. Ref'Using p t x' x (t -> Run x Unit -> Run x' t)
xref'exec i m = xref'run @p i m <#> fst

xref'get :: forall @p t x' x. Ref'Using p t x' x (Run x t)
xref'get = g1 @XDoAsked @p \r -> (fst r).get

xref'set :: forall @p t x' x. Ref'Using p t x' x (t -> Run x Unit)
xref'set v = g1 @XDoAsked @p \r -> (fst r).set v

type Ref2'Using p t x' x rest =
  IsSymbol p
  => Cons p (Reader { a :: (Ref'Eff'R t p) }) x' x
  => EffAdapter (Ref'Eff'T t) p t (Ref'Eff'R t p) t
  => rest

st'Ref_ :: forall @tag t. Generable tag GDefault t => ST'_'Ref t
st'Ref_ = XST'Init $ Proxy @(Ref'Eff'T t) /\ g @tag

st'Ref :: forall t. t -> ST'_'Ref t
st'Ref t = XST'Init $ Proxy @(Ref'Eff'T t) /\ t

type ST'_'Ref t = XST'Init (Ref'Eff'T t) t

data ST'Ref t

instance ST'Spec'Label (ST'Ref t) p (Ref'Eff'T t) (Ref'Eff'R t p)

instance M'st'put (Ref'Eff'T t) p (Ref'Eff'R t p) t where
  m'st'put r v = r.set v

type ST'''Ref r t p = r (XST'Init (Ref'Eff'T t) t) (Ref'Eff'R t p)
