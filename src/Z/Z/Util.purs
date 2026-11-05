module Z.Z.Util
  ( Type_Ap
  , Type_Ap_R
  , arg2'
  , arg3'
  , arg4'
  , arr'first
  , arr'last
  , arr'reverse
  , arr'sort
  , arr'sortBy
  , arr'sortWith
  , arr'splitHead
  , arr'splitTail
  , class RevSym
  , class SplitSp1
  , class SplitSp1Impl
  , class UpCat
  , class UpCf
  , class UpCt
  , encode
  , id
  , jsonKeys
  , jsonLookup
  , jsonPairs
  , jsonSortedPairs
  , jsonVals
  , nth
  , type (#)
  , type ($)
  ) where

import Prelude

import Data.Argonaut.Core (stringify) as AArg
import Data.Argonaut.Core as Arg
import Data.Argonaut.Decode (JsonDecodeError(..)) as JDE
import Data.Argonaut.Decode as ADec
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Enc
import Data.Array as Array
import Data.Either as Either
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord as Ord
import Data.Ordering as Ordering
import Data.Tuple (fst)
import Data.Tuple as Tup
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Foreign as Foreign
import Foreign.Object as FO
import Prim.Symbol as Symbol
import Type.Proxy as Proxy
import Z.Z.Core (arr'drop, arr'size, arr'take, eOr'_)
import Z.Z.Core as Z
import Z.Z.Id (class Identable, id'of)
import Z.Z.It (arr'collect, iterable'kv'mk, obj'collect)
import Z.Z.Variant
  ( var'default
  , var'encode
  , var'inj
  , var'inj_
  , var'on
  )

nth :: forall a. Array a -> Int -> Maybe.Maybe a
nth = Array.index

arr'first :: forall a. Array a -> Maybe.Maybe a
arr'first a = nth a 0

arr'splitHead :: forall a. Array a -> Maybe.Maybe (a /\ Array a)
arr'splitHead a = arr'first a <#> flip (/\) (arr'drop 1 a)

arr'splitTail :: forall a. Array a -> Maybe.Maybe (a /\ Array a)
arr'splitTail a = arr'last a <#> flip (/\) (arr'take ((arr'size a) - 1) a)

arr'last :: forall a. Array a -> Maybe.Maybe a
arr'last a = nth a ((Array.length a) - 1)

arr'sort :: forall a. Ord.Ord a => Array a -> Array a
arr'sort = Array.sort

arr'sortBy :: forall a. (a -> a -> Ordering.Ordering) -> Array a -> Array a
arr'sortBy = Array.sortBy

arr'sortWith :: forall a b. Ord.Ord b => (a -> b) -> Array a -> Array a
arr'sortWith = Array.sortWith

arr'reverse :: forall a. Array a -> Array a
arr'reverse = Array.reverse

jsonKeys :: Arg.Json -> Array String
jsonKeys = Arg.caseJsonObject [] FO.keys

jsonVals :: Arg.Json -> Array Arg.Json
jsonVals = Arg.caseJsonObject [] FO.values

jsonPairs :: Arg.Json -> Array (Tup.Tuple String Arg.Json)
jsonPairs = Arg.caseJsonObject [] (arr'collect <<< iterable'kv'mk)

jsonSortedPairs :: Arg.Json -> Array (Tup.Tuple String Arg.Json)
jsonSortedPairs a = arr'sortWith fst $ jsonPairs a

jsonLookup :: String -> Arg.Json -> Maybe.Maybe Arg.Json
jsonLookup k = Arg.caseJsonObject Maybe.Nothing (FO.lookup k)

id :: forall a. a -> a
id a = a

encode
  :: forall v
   . Enc.EncodeJson v
  => v
  -> String
encode v = AArg.stringify $ Enc.encodeJson v

type Type_Ap :: forall k1 k2. (k1 -> k2) -> k1 -> k2
type Type_Ap f x = f x

type Type_Ap_R :: forall k1 k2. k1 -> (k1 -> k2) -> k2
type Type_Ap_R x f = f x

infixr 0 type Type_Ap as $
infixr 0 type Type_Ap_R as #

arg2' :: forall a1 a2 r. a2 -> (a1 -> a2 -> r) -> (a1 -> r)
arg2' a2 f a1 = f a1 a2

arg3' :: forall a1 a2 a3 r. a3 -> (a1 -> a2 -> a3 -> r) -> (a1 -> a2 -> r)
arg3' a3 f a1 a2 = f a1 a2 a3

arg4'
  :: forall a1 a2 a3 a4 r
   . a4
  -> (a1 -> a2 -> a3 -> a4 -> r)
  -> (a1 -> a2 -> a3 -> r)
arg4' a4 f a1 a2 a3 = f a1 a2 a3 a4

class SplitSp1 :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class SplitSp1 i o1 o2 | i -> o1 o2

instance (SplitSp1Impl i "" "" "f" o1 o2) => SplitSp1 i o1 o2

class SplitSp1Impl
  :: forall k1 k2 k3 k4 k5 k6. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> Constraint
class SplitSp1Impl sym cat cf ct tat tf | sym cat cf ct -> tat tf

class UpCat :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Constraint
class UpCat c cat ct cat' | c cat ct -> cat'

instance UpCat " " cat ct cat
else instance UpCat c cat "t" cat
else instance (Symbol.Cons c cat cat') => UpCat c cat "f" cat'

class UpCf :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Constraint
class UpCf c cf ct cf' | c cf ct -> cf'

instance UpCf c cf "f" cf
else instance (Symbol.Cons c cf cf') => UpCf c cf "t" cf'

class UpCt :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class UpCt c ct ct' | c ct -> ct'

instance UpCt " " ct "t"
else instance UpCt c ct ct

class RevSym :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class RevSym s c s' | s c -> s'

instance RevSym "" c c
else instance
  ( Symbol.Cons c1 s' s
  , Symbol.Cons c1 c c'
  , RevSym s' c' f
  ) =>
  RevSym s c f

instance
  ( RevSym cf "" tf
  , RevSym cat "" tat
  ) =>
  SplitSp1Impl "" cat cf ct tat tf
else instance
  ( Symbol.Cons c s' s
  , UpCat c cat ct cat'
  , UpCf c cf ct cf'
  , UpCt c ct ct'
  , SplitSp1Impl s' cat' cf' ct' tat tf
  ) =>
  SplitSp1Impl s cat cf ct tat tf

