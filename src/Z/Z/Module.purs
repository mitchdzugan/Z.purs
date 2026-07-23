module Z.Z.Module
  ( firstOfOn
  , l
  , lpt
  , module Aff
  , module Arg
  , module Array
  , module CA
  , module Core
  , module DC
  , module Dec
  , module DecodeGeneric
  , module Effect
  , module EffectClass
  , module Either
  , module Enc
  , module EncodeGeneric
  , module Exists
  , module Foldable
  , module Generic
  , module Lens
  , module LensAt
  , module LensIndex
  , module LensRecord
  , module LensT
  , module Map
  , module Maybe
  , module MaybeFirst
  , module Promise
  , module Proxy
  , module Record
  , module Row
  , module Run
  , module RunS
  , module Str
  , module StrCommon
  , module Symbol
  , module Tup
  , module TupNested
  , module TypeEquals
  , module XX
  , module ZUtil
  , strJoinWith
  , strSplit
  ) where

import Control.Promise (Promise) as Promise
import Data.Argonaut.Core
  ( Json
  , caseJsonString
  , caseJsonNumber
  , fromString
  , jsonNull
  ) as Arg
import Data.Argonaut.Decode (class DecodeJson, fromJsonString) as Dec
import Data.Argonaut.Decode.Generic (genericDecodeJson) as DecodeGeneric
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Enc
import Data.Argonaut.Encode.Generic (genericEncodeJson) as EncodeGeneric
import Data.Array (slice) as Array
import Data.Codec (Codec, Codec') as DC
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (Either(..), either) as Either
import Data.Exists (Exists, mkExists, runExists) as Exists
import Data.Foldable (fold, class Foldable) as Foldable
import Data.Generic.Rep (class Generic) as Generic
import Data.Lens (Fold, Optic, Lens, Lens', Prism, Prism', view, firstOf, lastOf, toArrayOf, review, over, set, _Just) as Lens
import Data.Lens (previewOn)
import Data.Lens.Barlow as Barlow
import Data.Lens.Barlow.Construction as BarlowCons
import Data.Lens.Barlow.Parser as BarlowParse
import Data.Lens.Types (AffineTraversal) as LensT
import Data.Lens.At (at, class At) as LensAt
import Data.Lens.Index (ix, class Index) as LensIndex
import Data.Lens.Record (prop) as LensRecord
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing) as Maybe
import Data.Maybe.First (First) as MaybeFirst
import Data.String (Pattern(..)) as Str
import Data.String.Common as StrCommon
import Data.Symbol (class IsSymbol, reifySymbol, reflectSymbol) as Symbol
import Data.Tuple (Tuple(..), fst, snd) as Tup
import Data.Tuple.Nested ((/\), type (/\)) as TupNested
import Effect (Effect) as Effect
import Effect.Aff (Aff, launchAff, launchAff_, runAff, runAff_) as Aff
import Effect.Class (liftEffect) as EffectClass
import Prim.Row (class Cons, class Lacks) as Row
import Record (merge) as Record
import Run (Run, extract) as Run
import Run.State (execState) as RunS
import Type.Equality (class TypeEquals) as TypeEquals
import Type.Proxy (Proxy(..)) as Proxy
import Z.Z.Core as Core
import Z.Z.X as XX
import Z.Z.Util as ZUtil
import Data.Map (Map) as Map
import Prelude

l
  :: forall @string lenses p s t a b
   . BarlowParse.ParseSymbol string lenses
  => BarlowCons.ConstructBarlow lenses p s t a b
  => Symbol.IsSymbol string
  => Lens.Optic p s t a b
l = Barlow.barlow @string

lpt
  :: ∀ (@l :: Symbol) r' r @a
  . Symbol.IsSymbol l
  ⇒ Row.Cons l a r' r
  ⇒ Lens.Lens' (Record r) a
lpt = LensRecord.prop (Proxy.Proxy @l)

firstOfOn
  :: forall s t a b
   . s
  -> Lens.Fold (MaybeFirst.First a) s t a b
  -> Maybe.Maybe a
firstOfOn = previewOn

strJoinWith :: String -> Array String -> String
strJoinWith = StrCommon.joinWith

strSplit ∷ Str.Pattern → String → Array String
strSplit = StrCommon.split
