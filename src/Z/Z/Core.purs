module Z.Z.Core
  ( (<##>)
  , (<$$>)
  , AntiUnit
  , HashSet
  , JsAny
  , JsError(..)
  , P
  , ParseError
  , T'_
  , T'comp
  , T'flip
  , T'useAsSym
  , antiUnit
  , arrDrop
  , arrEmpty
  , arrFilter
  , arrFromFoldable
  , arrSize
  , arrSlice
  , class Resulting
  , class RtError
  , class SText
  , dec
  , encodeForeign
  , encodeOpts
  , fDiscard
  , ffmap
  , ffmapFlipped
  , forM
  , forM_
  , inc
  , intFromString
  , invert
  , jsAny
  , jsError
  , jsError'
  , jsErrorMessage
  , jsErrorName
  , jsErrorStack
  , jsonRmNils
  , jsonStr
  , listFromFoldable
  , mapEmpty
  , mapFromFoldable
  , mapL
  , mapM
  , mapSet
  , mapSize
  , p
  , p2
  , parseEof
  , parseFail
  , parseFailWithPosition
  , parseInt
  , parseNumber
  , parseRest
  , parseString
  , parseStringAs
  , parseStringEofAs
  , parseString_
  , parseTry
  , pureF
  , rec'get
  , rec'insert
  , rec'merge
  , rec'set
  , rec'union
  , reduce
  , reduceM
  , resultVal
  , routeParse
  , routePrint
  , rtErrExtra
  , rtErrMessage
  , rtErrName
  , runParser
  , setAdd
  , setEmpty
  , setFromFoldable
  , setHas
  , setSize
  , simpleHash
  , stext
  , tryParseInt
  , whenNot
  ) where

import Prelude

import Control.Applicative as Applicative
import Control.Monad as Monad
import Data.Argonaut.Core as Arg
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array as Arr
import Data.Either as Eor
import Data.Foldable as Foldable
import Data.Functor as F
import Data.Functor.Flip (Flip)
import Data.Int as Int
import Data.List as List
import Data.Map as Map
import Data.Maybe as May
import Data.Ord as Ord
import Data.Ring as Ring
import Data.Semiring as Semiring
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Data.Traversable as Traversable
import Data.Tuple.Nested as TupN
import Effect.Exception as Exc
import Foreign as Foreign
import Parsing as Parsing
import Parsing.Combinators as Prc
import Parsing.String as Prs
import Parsing.String.Basic as Prsb
import Record as Record
import Routing.Duplex as Dup
import Routing.Duplex.Parser as DupP
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..)) as Proxy

rec'get = Record.get
rec'set = Record.set
rec'insert = Record.insert
rec'merge = Record.merge
rec'union = Record.union

whenNot :: forall m. Monad m => Boolean -> m Unit -> m Unit
whenNot b = when (not b)

routePrint :: forall i o. Dup.RouteDuplex i o -> i -> String
routePrint = Dup.print

routeParse
  :: forall i o. Dup.RouteDuplex i o -> String -> Eor.Either DupP.RouteError o
routeParse = Dup.parse

type T'id :: forall k. k -> k
type T'id a = a

type T'comp :: forall k1 k2 k3. (k1 -> k2) -> (k3 -> k1) -> k3 -> k2
type T'comp f g x = f (g x)

type T'flip :: forall k1 k2 k3. (k1 -> k2 -> k3) -> k2 -> k1 -> k3
type T'flip f a b = f b a

type T'_ (f :: Type -> Type) = f Unit

type T'useAsSym s p f = TypeEquals s p => IsSymbol p => f p

class SText a where
  stext :: a -> String

instance SText String where
  stext s = s
else instance SText Unit where
  stext _ = ""
else instance Show s => SText s where
  stext = show

class Applicative f <= Resulting f where
  resultVal :: forall a. f a -> May.Maybe a

instance Resulting May.Maybe where
  resultVal m = m

instance Resulting (Eor.Either e) where
  resultVal = Eor.hush

foreign import data JsAny :: Type

foreign import js_JsAny :: forall a. a -> JsAny

foreign import js_JsAnyToForeign :: Arg.Json -> Foreign.Foreign

foreign import js_simpleHash :: String -> Int

foreign import js_jsonStr :: Arg.Json -> String

foreign import js_removeNils :: Arg.Json -> Arg.Json

newtype AntiUnit = AntiUnit Unit

instance Eq AntiUnit where
  eq _ _ = false

antiUnit :: AntiUnit
antiUnit = AntiUnit unit

jsAny :: forall a. a -> JsAny
jsAny = js_JsAny

encodeForeign :: forall d. EncodeJson d => d -> Foreign.Foreign
encodeForeign = js_JsAnyToForeign <<< encodeJson

jsonStr :: Arg.Json -> String
jsonStr = js_jsonStr

jsonRmNils :: Arg.Json -> Arg.Json
jsonRmNils = js_removeNils

encodeOpts :: forall d. EncodeJson d => d -> Arg.Json
encodeOpts = jsonRmNils <<< encodeJson

simpleHash :: String -> Int
simpleHash = js_simpleHash

newtype JsError = JsError Exc.Error

type PureJsError =
  { "_" :: String, name :: String, message :: String }

fromPureJsError :: PureJsError -> JsError
fromPureJsError e = JsError $ Exc.errorWithName e.message e.name

instance DecodeJson JsError where
  decodeJson j = map fromPureJsError $ decodeJson j

instance EncodeJson JsError where
  encodeJson (JsError e) = encodeJson
    { "_": "JsError", name: Exc.name e, message: Exc.message e }

jsErrorName :: JsError -> String
jsErrorName (JsError e) = Exc.name e

jsErrorMessage :: JsError -> String
jsErrorMessage (JsError e) = Exc.message e

jsErrorStack :: JsError -> May.Maybe String
jsErrorStack (JsError e) = Exc.stack e

jsError :: String -> String -> JsError
jsError name message = fromPureJsError $ { name, message, "_": "" }

jsError' :: String -> JsError
jsError' = flip jsError ""

class RtError a where
  rtErrName :: a -> String
  rtErrMessage :: a -> String
  rtErrExtra :: a -> Arg.Json

instance RtError JsError where
  rtErrName = jsErrorName
  rtErrMessage = jsErrorMessage
  rtErrExtra e = encodeJson { stack: jsErrorStack e }

instance RtError Void where
  rtErrName _ = "unreachable error"
  rtErrMessage _ = "should never see this"
  rtErrExtra _ = encodeJson {}

fDiscard :: forall f i. F.Functor f => f i -> f Unit
fDiscard = map $ const unit

ffmap
  :: forall f g a b
   . F.Functor f
  => F.Functor g
  => (a -> b)
  -> (g (f a))
  -> (g (f b))
ffmap f r = map (map f) r

infixl 2 ffmap as <$$>

ffmapFlipped
  :: forall f g a b
   . F.Functor f
  => F.Functor g
  => (g (f a))
  -> (a -> b)
  -> (g (f b))
ffmapFlipped = flip ffmap

infixl 2 ffmapFlipped as <##>

type P :: forall k. k -> Type
type P a = Proxy.Proxy a

p ∷ ∀ (@a ∷ Symbol). Proxy.Proxy a
p = Proxy.Proxy

pureF :: forall a x y. Applicative a => (x -> y) -> x -> a y
pureF f = pure <<< f

inc :: forall s. Semiring s => s -> s
inc s = Semiring.add s Semiring.one

dec :: forall r. Ring r => r -> r
dec s = Ring.sub s Semiring.one

mapEmpty :: forall @k @v. Ord k => Map.Map k v
mapEmpty = Map.empty

mapSize :: forall k v. Map.Map k v -> Int
mapSize = Map.size

mapSet :: forall @k @v. Ord k => k -> v -> Map.Map k v -> Map.Map k v
mapSet = Map.insert

mapFromFoldable
  :: forall @k @v f
   . Foldable.Foldable f
  => Ord k
  => f (k TupN./\ v)
  -> Map.Map k v
mapFromFoldable = Map.fromFoldable

type HashSet a = Set.Set a

setEmpty :: forall @a. HashSet a
setEmpty = Set.empty

setHas :: forall a. Ord.Ord a => a -> HashSet a -> Boolean
setHas = Set.member

setAdd :: forall a. Ord.Ord a => a -> HashSet a -> HashSet a
setAdd = Set.insert

setSize :: forall a. HashSet a -> Int
setSize = Set.size

setFromFoldable
  :: forall a f. Foldable.Foldable f => Ord.Ord a => f a -> HashSet a
setFromFoldable = Set.fromFoldable

arrSlice :: forall a. Int -> Int -> Array a -> Array a
arrSlice = Arr.slice

arrDrop :: forall a. Int -> Array a -> Array a
arrDrop n a = Arr.slice n (arrSize a) a

arrSize :: forall a. Array a -> Int
arrSize = Arr.length

arrFilter :: forall a. (a -> Boolean) -> Array a -> Array a
arrFilter = Arr.filter

arrEmpty :: forall @a. Array a
arrEmpty = []

arrFromFoldable :: forall a f. Foldable.Foldable f => f a -> Array a
arrFromFoldable = Arr.fromFoldable

listFromFoldable :: forall a f. Foldable.Foldable f => f a -> List.List a
listFromFoldable = List.fromFoldable

invert :: forall e r. Eor.Either e r -> Eor.Either r e
invert (Eor.Left e) = Eor.Right e
invert (Eor.Right r) = Eor.Left r

mapM
  :: forall t a b m
   . Traversable.Traversable t
  => Applicative.Applicative m
  => (a -> m b)
  -> t a
  -> m (t b)
mapM = Traversable.traverse

forM
  :: forall t a b m
   . Traversable.Traversable t
  => Applicative.Applicative m
  => t a
  -> (a -> m b)
  -> m (t b)
forM = flip Traversable.traverse

forM_
  :: forall t a m
   . Traversable.Traversable t
  => Applicative.Applicative m
  => t a
  -> (a -> m Unit)
  -> m Unit
forM_ = flip Traversable.traverse_

reduceM
  :: forall f m a b
   . Foldable.Foldable f
  => Monad.Monad m
  => (b -> a -> m b)
  -> b
  -> f a
  -> m b
reduceM = Foldable.foldM

reduce
  :: forall f a b
   . Foldable.Foldable f
  => (b -> a -> b)
  -> b
  -> f a
  -> b
reduce = Foldable.foldl

newtype ParseError = ParseError Parsing.ParseError

type PureParseError =
  { "_" :: String, column :: Int, index :: Int, line :: Int, message :: String }

fromPureParseError :: PureParseError -> ParseError
fromPureParseError e = ParseError $ Parsing.ParseError e.message $
  Parsing.Position { column: e.column, index: e.index, line: e.line }

instance DecodeJson ParseError where
  decodeJson j = map fromPureParseError $ decodeJson j

instance EncodeJson ParseError where
  encodeJson
    ( ParseError
        (Parsing.ParseError message (Parsing.Position { column, index, line }))
    ) = encodeJson { "_": "ParseError", column, index, line, message }

runParser :: forall s a. s -> Parsing.Parser s a -> Eor.Either ParseError a
runParser s pr = mapL ParseError $ Parsing.runParser s pr

mapL :: forall l1 l2 r. (l1 -> l2) -> Eor.Either l1 r -> Eor.Either l2 r
mapL f = Eor.either (\x -> Eor.Left $ f x) Eor.Right

parseFail :: forall m s a. String -> Parsing.ParserT s m a
parseFail = Parsing.fail

parseFailWithPosition
  :: forall m s a. String -> Parsing.Position -> Parsing.ParserT s m a
parseFailWithPosition = Parsing.failWithPosition

parseTry
  :: forall m s a. Parsing.ParserT s m a -> Parsing.ParserT s m a
parseTry = Prc.try

parseEof :: forall m. Parsing.ParserT String m Unit
parseEof = Prs.eof

parseRest :: forall m. Parsing.ParserT String m String
parseRest = Prs.rest

parseString :: forall m. String -> Parsing.ParserT String m String
parseString = Prs.string

parseStringAs :: forall m v. String -> v -> Parsing.ParserT String m v
parseStringAs s v = Prs.string s <#> const v

parseStringEofAs :: forall m v. String -> v -> Parsing.ParserT String m v
parseStringEofAs s v = do
  res <- parseStringAs s v
  parseEof
  pure res

parseString_ :: forall m. String -> Parsing.ParserT String m Unit
parseString_ s = parseStringAs s unit

parseNumber :: forall m. Parsing.ParserT String m Number
parseNumber = Prsb.number

parseInt :: forall m. Parsing.ParserT String m Int
parseInt = do
  n <- Prsb.number
  let i = Int.trunc n
  let ni = Int.toNumber i
  when (not (n == ni)) do
    parseFail "Integer Number Expected"
  pure i

tryParseInt :: String -> May.Maybe Int
tryParseInt s = Eor.hush $ runParser s parseInt

p2 :: Int -> Int
p2 = Int.pow 2

intFromString :: String -> May.Maybe Int
intFromString = Int.fromString