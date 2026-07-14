module Core
  ( JsError
  , c_propOptionalDefault
  , c_propOptionalWithDefault
  , c_stringOrNum
  , stringOrNumN
  , stringOrNumS
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Codec as DC
import Data.Argonaut.Core as Arg
import Data.Codec.Argonaut as CA
import Data.Default (class DefaultValue, defaultValue)
import Data.Functor as Func
import Data.Profunctor as Prof
import Record as Record
import Effect.Exception as Exc
import Type.Proxy (Proxy)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)

type JsError = Exc.Error

type StringOrNum = { s :: String, mn :: Maybe Number }

stringOrNumS :: String -> StringOrNum
stringOrNumS s = { s: s, mn: Nothing }

stringOrNumN :: Number -> StringOrNum
stringOrNumN n = { s: show n, mn: Just n }

readStringOrNum :: Arg.Json -> Maybe StringOrNum
readStringOrNum j = Arg.caseJsonString
  (Arg.caseJsonNumber Nothing (Just <<< stringOrNumN) j)
  (Just <<< stringOrNumS)
  j

jsonStringOrNum :: StringOrNum -> Arg.Json
jsonStringOrNum { mn: Just n } = Arg.fromNumber n
jsonStringOrNum { s } = Arg.fromString s

c_stringOrNum :: CA.JsonCodec StringOrNum
c_stringOrNum = CA.prismaticCodec "String/or/Num" readStringOrNum
  jsonStringOrNum
  CA.json

propOptionalDefault_to_prop
  :: forall r' r rm l a
   . DefaultValue a
  => IsSymbol l
  => Cons l a r' r
  => Cons l (Maybe a) r' rm
  => Proxy l
  -> Record rm
  -> Record r
propOptionalDefault_to_prop p = Record.modify p getv
  where
  getv (Just v) = v
  getv _ = defaultValue

prop_to_propOptional
  :: forall r' r rm l a
   . IsSymbol l
  => Cons l a r' r
  => Cons l (Maybe a) r' rm
  => Proxy l
  -> Record r
  -> Record rm
prop_to_propOptional p = Record.modify p Just

c_propOptionalDefault
  :: forall r' r rm l a cerr cin
   . DefaultValue a
  => IsSymbol l
  => Cons l a r' r
  => Cons l (Maybe a) r' rm
  => Func.Functor cerr
  => Proxy l
  -> DC.Codec' cerr cin (Record rm)
  -> DC.Codec' cerr cin (Record r)
c_propOptionalDefault p = Prof.dimap (prop_to_propOptional p)
  (propOptionalDefault_to_prop p)

propOptionalWithDefault_to_prop
  :: forall r' r rm l a
   . IsSymbol l
  => Cons l a r' r
  => Cons l (Maybe a) r' rm
  => Proxy l
  -> a
  -> Record rm
  -> Record r
propOptionalWithDefault_to_prop p d = Record.modify p getv
  where
  getv (Just v) = v
  getv _ = d

c_propOptionalWithDefault
  :: forall r' r rm l a cerr cin
   . IsSymbol l
  => Cons l a r' r
  => Cons l (Maybe a) r' rm
  => Func.Functor cerr
  => Proxy l
  -> a
  -> DC.Codec' cerr cin (Record rm)
  -> DC.Codec' cerr cin (Record r)
c_propOptionalWithDefault p d = Prof.dimap (prop_to_propOptional p)
  (propOptionalWithDefault_to_prop p d)

