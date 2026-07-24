module Z.Z.Module
  ( PairKey(..)
  , module Aff
  , module Arg
  , module Array
  , module CA
  , module DC
  , module DateTime
  , module Dec
  , module DecodeGeneric
  , module Effect
  , module EffectClass
  , module Either
  , module Enc
  , module EncodeGeneric
  , module Enum
  , module Exists
  , module Foldable
  , module Generic
  , module Int
  , module Lens
  , module LensAt
  , module LensIndex
  , module LensRecord
  , module LensT
  , module Map
  , module Maybe
  , module MaybeFirst
  , module Pair
  , module Parsing
  , module Prc
  , module Promise
  , module Proxy
  , module Record
  , module Row
  , module Run
  , module RunS
  , module Str
  , module Symbol
  , module Tup
  , module TupNested
  , module TypeEquals
  , module ZBl
  , module ZCore
  , module ZDefaultable
  , module ZUtil
  , module ZX
  , or
  , p2
  , preview_
  , strJoinWith
  , strSplit
  , view_
  ) where

import Prelude

import Control.Promise (Promise) as Promise
import Data.Argonaut.Core (Json, caseJsonString, caseJsonNumber, fromString, jsonNull) as Arg
import Data.Argonaut.Decode (class DecodeJson, fromJsonString) as Dec
import Data.Argonaut.Decode.Generic (genericDecodeJson) as DecodeGeneric
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Enc
import Data.Argonaut.Encode.Generic (genericEncodeJson) as EncodeGeneric
import Data.Array (slice) as Array
import Data.Codec (Codec, Codec') as DC
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.DateTime (DateTime(..), Month(..), Hour, Year, Day, Second, Minute, Millisecond, canonicalDate, Time(..), Date) as DateTime
import Data.DateTime.Instant (Instant, instant, toDateTime) as DateTime
import Data.Either (Either(..), either, hush) as Either
import Data.Enum (toEnum) as Enum
import Data.Exists (Exists, mkExists, runExists) as Exists
import Data.Foldable (fold, class Foldable) as Foldable
import Data.Generic.Rep (class Generic) as Generic
import Data.Int (ceil, floor, round, trunc, toNumber, pow) as Int
import Data.Lens (Fold, Optic, Lens, Lens', Prism, Prism', view, preview, previewOn, viewOn, lastOf, toArrayOf, review, over, set, _Just) as Lens
import Data.Lens.At (at, class At) as LensAt
import Data.Lens.Index (ix, class Index) as LensIndex
import Data.Lens.Record (prop) as LensRecord
import Data.Lens.Types (AffineTraversal) as LensT
import Data.Map (Map) as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing) as Maybe
import Data.Maybe.First (First) as MaybeFirst
import Data.Pair (Pair(..), (~)) as Pair
import Data.String (Pattern(..)) as Str
import Data.String.Common as StrCommon
import Data.Symbol (class IsSymbol, reifySymbol, reflectSymbol) as Symbol
import Data.Time.Duration (Milliseconds(..), Hours(..)) as DateTime
import Data.Tuple (Tuple(..), fst, snd) as Tup
import Data.Tuple.Nested ((/\), type (/\)) as TupNested
import Effect (Effect) as Effect
import Effect.Aff (Aff, launchAff, launchAff_, runAff, runAff_) as Aff
import Effect.Class (liftEffect) as EffectClass
import Parsing (ParserT) as Parsing
import Parsing.Combinators ((<|>)) as Prc
import Prim.Row (class Cons, class Lacks) as Row
import Record (merge) as Record
import Run (Run, extract) as Run
import Run.State (execState) as RunS
import Type.Equality (class TypeEquals) as TypeEquals
import Type.Proxy (Proxy(..)) as Proxy
import Z.Z.Barlow (class Barlow, class ConstructBarlow, class IsSymbol, class ParseSymbol, class Strong, First, Forget, barlow) as ZBl
import Z.Z.Core as ZCore
import Z.Z.Defaultable as ZDefaultable
import Z.Z.Util as ZUtil
import Z.Z.X as ZX

or :: forall a. a -> Maybe.Maybe a -> a
or = Maybe.fromMaybe

p2 :: Int -> Int
p2 = Int.pow 2

strJoinWith :: String -> Array String -> String
strJoinWith = StrCommon.joinWith

strSplit ∷ Str.Pattern -> String -> Array String
strSplit = StrCommon.split

view_
  :: forall @sym lenses s t a b
   . ZBl.ParseSymbol sym lenses
  => ZBl.ConstructBarlow lenses (ZBl.Forget a) s t a b
  => ZBl.IsSymbol sym
  => s
  -> a
view_ = Lens.view (ZBl.barlow @sym)

preview_
  :: forall @sym lenses s t a b
   . ZBl.ParseSymbol sym lenses
  => ZBl.ConstructBarlow lenses (ZBl.Forget (ZBl.First a)) s t a b
  => ZBl.IsSymbol sym
  => s
  -> Maybe.Maybe a
preview_ = Lens.preview (ZBl.barlow @sym)

data PairKey = Up | Down

derive instance eqUser :: Eq PairKey
derive instance ordUser :: Ord PairKey
derive instance genericT :: Generic.Generic PairKey _

instance decodeJsonT :: Dec.DecodeJson PairKey where
  decodeJson x = DecodeGeneric.genericDecodeJson x

instance encodeJsonT :: Enc.EncodeJson PairKey where
  encodeJson x = EncodeGeneric.genericEncodeJson x
