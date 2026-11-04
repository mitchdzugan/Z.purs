module Z.Z.Variant
  ( Var''Decode
  , Var''Encode
  , Var''Key
  , Var''Shown
  , class IsVariant
  , class Variant'Mk
  , class Variant'Mker
  , class Variant'Mker'rows
  , class Variant'On
  , class Variant'handle
  , class Variant'handle'rows
  , fromVariant
  , toVariant
  , var'case_
  , var'decode
  , var'default
  , var'encode
  , var'inj
  , var'inj_
  , var'key
  , var'match
  , var'on
  , var'show
  , variant'handle
  , variant'handle'rows
  , variant'mk
  , variant'mker
  , variant'mker'rows
  , variant'mkers
  , variant'on
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (class VariantMatchCases, Variant, case_, inj, match, on)
import Data.Variant as Var
import Foreign.Object (Object)
import Prim.Row (class Cons, class Union)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Z.Z.Core (may'_, obj'lookup)
import Z.Z.Defaultable (class Generable, GDefault, default)
import Z.Z.It (obj'collect)

class IsVariant t v | t -> v where
  toVariant :: t -> Variant v
  fromVariant :: Variant v -> t

instance IsVariant (Variant v) v where
  toVariant = identity
  fromVariant = identity
else instance (Newtype t (Variant v)) => IsVariant t v where
  toVariant = unwrap
  fromVariant = wrap

var'inj
  :: forall t v' v @l a. IsSymbol l => Cons l a v' v => IsVariant t v => a -> t
var'inj = fromVariant @t <<< inj (Proxy @l)

var'inj_
  :: forall t v' v @l a
   . IsSymbol l
  => Cons l a v' v
  => IsVariant t v
  => Generable a GDefault a
  => t
var'inj_ = var'inj @l default

var'match
  :: forall t rl r r1 r2 b
   . RL.RowToList r rl
  => VariantMatchCases rl r1 b
  => Union r1 () r2
  => IsVariant t r2
  => Record r
  -> t
  -> b
var'match r = match r <<< toVariant @t

var'default :: forall a r t. IsVariant t r => a -> t -> a
var'default defVal = Var.default defVal <<< toVariant @t

var'case_ :: forall a t. IsVariant t () => t -> a
var'case_ = Var.case_ <<< toVariant @t

var'on
  :: forall @sym t a b r1 r2
   . Cons sym a r1 r2
  => IsSymbol sym
  => IsVariant t r2
  => (a -> b)
  -> (Variant r1 -> b)
  -> t
  -> b
var'on f rest = Var.on (Proxy @sym) f rest <<< toVariant @t

class Variant'On :: forall k. k -> Type -> Type -> Constraint
class Variant'On tag arg res | tag arg -> res where
  variant'on :: forall p. IsSymbol p => Proxy p -> arg -> res

foreign import data Var''Key :: Type

instance Variant'On Var''Key arg String where
  variant'on proxy _ = reflectSymbol proxy

foreign import data Var''Shown :: Type

instance (Show arg) => Variant'On Var''Shown arg String where
  variant'on _ = show

foreign import data Var''Encode :: Type

instance (EncodeJson arg) => Variant'On Var''Encode arg Json where
  variant'on proxy v = encodeJson { at: reflectSymbol proxy, inj: v }

class Variant'handle :: forall k. k -> Type -> Type -> Constraint
class Variant'handle tag v res where
  variant'handle :: v -> res

class Variant'handle'rows
  :: forall k1 k2. k1 -> k2 -> Row Type -> Type -> Constraint
class Variant'handle'rows tag rowList row res | tag rowList -> row res where
  variant'handle'rows :: Proxy rowList -> Variant row -> res

instance Variant'handle'rows tag RL.Nil () a where
  variant'handle'rows _ = case_

instance
  ( IsSymbol key
  , Variant'On tag focus a
  , Cons key focus rowTail row
  , Variant'handle'rows tag rowListTail rowTail a
  ) =>
  Variant'handle'rows tag (RL.Cons key focus rowListTail) row a where
  variant'handle'rows _ = on (Proxy @key) (variant'on @tag $ Proxy @key)
    (variant'handle'rows @tag $ Proxy @rowListTail)

instance
  ( RL.RowToList row rowList
  , Variant'handle'rows tag rowList row res
  ) =>
  Variant'handle tag (Variant row) res where
  variant'handle = variant'handle'rows @tag $ Proxy @rowList
else instance
  ( RL.RowToList row rowList
  , Variant'handle'rows tag rowList row res
  , Newtype nt (Variant row)
  ) =>
  Variant'handle tag nt res where
  variant'handle = variant'handle'rows @tag (Proxy @rowList) <<< unwrap

class Variant'Mk :: forall k1 k2. k1 -> k2 -> Type -> (k2 -> Type) -> Constraint
class Variant'Mk tag to from m | tag to -> from m where
  variant'mk :: from -> m to

class Variant'Mker
  :: forall k1 k2. k1 -> k2 -> Type -> (k2 -> Type) -> Constraint
class Variant'Mker tag to from m | tag to -> from m where
  variant'mker :: List (String /\ (from -> m to))

class Variant'Mker'rows
  :: forall k1 k2. k1 -> k2 -> Row Type -> Type -> (Type -> Type) -> Constraint
class Variant'Mker'rows tag rowList row from m | tag rowList -> from row m where
  variant'mker'rows
    :: Proxy rowList -> List (String /\ (from -> m (Variant row)))

instance Variant'Mker'rows tag RL.Nil row from m where
  variant'mker'rows _ = Nil

instance
  ( IsSymbol key
  , Variant'Mk tag focus from m
  , Cons key focus rowTail row
  , Variant'Mker'rows tag rowListTail row from m
  , Functor m
  ) =>
  Variant'Mker'rows tag (RL.Cons key focus rowListTail) row from m where
  variant'mker'rows _ = Cons
    (reflectSymbol (Proxy @key) /\ map (var'inj @key) <<< variant'mk @tag)
    (variant'mker'rows @tag (Proxy @rowListTail))

instance
  ( RL.RowToList row rowList
  , Variant'Mker'rows tag rowList row from m
  , Functor m
  ) =>
  Variant'Mker tag (Variant row) from m where
  variant'mker = variant'mker'rows @tag (Proxy @rowList)
else instance
  ( RL.RowToList row rowList
  , Variant'Mker'rows tag rowList row from m
  , Newtype nt (Variant row)
  , Functor m
  ) =>
  Variant'Mker tag nt from m where
  variant'mker = variant'mker'rows @tag (Proxy @rowList)
    <#> \(k /\ mkVar) -> k /\ \int -> wrap <$> mkVar int

variant'mkers
  :: forall @tag @to from m
   . Variant'Mker tag to from m
  => Object (from -> m to)
variant'mkers = obj'collect (variant'mker @tag @to)

foreign import data Var''Decode :: Type

instance
  ( DecodeJson focus
  ) =>
  Variant'Mk Var''Decode focus Json (Either JsonDecodeError) where
  variant'mk json = decodeJson json

var'decode
  :: forall @to
   . Variant'Mker Var''Decode to Json (Either JsonDecodeError)
  => Json
  -> Either JsonDecodeError to
var'decode json = do
  decodeJson json >>= onDecodeGeneric
  where
  onDecodeGeneric :: { inj :: Json, at :: String } -> Either JsonDecodeError to
  onDecodeGeneric { inj, at } = may'_
    (Left $ AtKey at $ UnexpectedValue inj)
    ((#) inj)
    (obj'lookup at decoders)
  decoders = variant'mkers @Var''Decode @to

var'encode :: forall @from. Variant'handle Var''Encode from Json => from -> Json
var'encode = variant'handle @Var''Encode

var'key :: forall @from. Variant'handle Var''Key from String => from -> String
var'key = variant'handle @Var''Key

var'show
  :: forall @from
   . Variant'handle Var''Key from String
  => Variant'handle Var''Shown from String
  => from
  -> String
var'show v = "Var@" <> vAt <> "[= " <> vInj <> " =]"
  where
  vAt = variant'handle @Var''Key v
  vInj = variant'handle @Var''Shown v