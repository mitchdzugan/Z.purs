module Z.Z.Core
  ( JsAny
  , JsError(..)
  , P
  , ParseError
  , Set
  , adjustDateTime
  , arrEmpty
  , arrFilter
  , arrFromFoldable
  , arrSize
  , arrSlice
  , auto
  , class Defaultable
  , dec
  , def
  , default
  , encodeOpts
  , fDiscard
  , forM
  , forM_
  , inc
  , invert
  , jsAny
  , jsError
  , jsError'
  , jsErrorMessage
  , jsErrorName
  , jsErrorStack
  , jsonRmNils
  , jsonStr
  , mapEmpty
  , mapL
  , mapM
  , mapSet
  , mapSize
  , orDefault
  , p
  , parseFail
  , parseFailWithPosition
  , parseInt
  , parseNumber
  , parseString
  , parseStringAs
  , parseString_
  , parseTry
  , pureF
  , reduce
  , reduceM
  , runParser
  , setEmpty
  , setFromFoldable
  , setHas
  , setSize
  , simpleHash
  , whenJust
  ) where

import Prelude

import Control.Applicative as Applicative
import Control.Monad as Monad
import Data.Argonaut.Core as Arg
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array as Arr
import Data.DateTime as DateTime
import Data.Time.Duration as TimeDuration
import Data.Either as Eor
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Traversable as Traversable
import Data.Functor as F
import Data.Map as Map
import Data.Maybe as May
import Data.Ord as Ord
import Data.Ring as Ring
import Data.Semiring as Semiring
import Data.Set as Set
import Effect.Exception as Exc
import Type.Proxy (Proxy(..)) as Proxy
import Parsing as Parsing
import Parsing.Combinators as Prc
import Parsing.String as Prs
import Parsing.String.Basic as Prsb

foreign import data JsAny :: Type

foreign import js_JsAny :: forall a. a -> JsAny

foreign import js_simpleHash :: String -> Int

foreign import js_jsonStr :: Arg.Json -> String

foreign import js_removeNils :: Arg.Json -> Arg.Json

jsAny :: forall a. a -> JsAny
jsAny = js_JsAny

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

instance decodeJsError :: DecodeJson JsError where
  decodeJson j = map fromPureJsError $ decodeJson j

instance encodeJsError :: EncodeJson JsError where
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

fDiscard :: forall f i. F.Functor f => f i -> f Unit
fDiscard = map $ const unit

type P :: forall k. k -> Type
type P a = Proxy.Proxy a

p ∷ ∀ (@a ∷ Symbol). Proxy.Proxy a
p = Proxy.Proxy

class Defaultable a where
  default :: a

instance defaultUnit :: Defaultable Unit where
  default = unit

else instance defaultArray :: Defaultable (Array a) where
  default = []

else instance defaultJust :: Defaultable (May.Maybe a) where
  default = May.Nothing

else instance defaultApplicable ::
  ( Defaultable v
  , Applicative a
  ) =>
  Defaultable (a v) where
  default = pure default

def :: forall @d. Defaultable d => d
def = default

auto :: forall d r. Defaultable d => (d -> r) -> r
auto f = f default

orDefault :: forall d. Defaultable d => May.Maybe d -> d
orDefault = auto <<< flip May.fromMaybe

whenJust
  :: forall m d a
   . Monad.Monad m
  => Defaultable d
  => May.Maybe a
  -> (a -> m d)
  -> m d
whenJust m f = May.maybe (pure default) f m

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

type Set a = Set.Set a

setEmpty :: forall @a. Set a
setEmpty = Set.empty

setHas :: forall a. Ord.Ord a => a -> Set a -> Boolean
setHas = Set.member

setSize :: forall a. Set a -> Int
setSize = Set.size

setFromFoldable :: forall a f. Foldable.Foldable f => Ord.Ord a => f a -> Set a
setFromFoldable = Set.fromFoldable

arrSlice :: forall a. Int -> Int -> Array a -> Array a
arrSlice = Arr.slice

arrSize :: forall a. Array a -> Int
arrSize = Arr.length

arrFilter :: forall a. (a -> Boolean) -> Array a -> Array a
arrFilter = Arr.filter

arrEmpty :: forall @a. Array a
arrEmpty = []

arrFromFoldable :: forall a f. Foldable.Foldable f => f a -> Array a
arrFromFoldable = Arr.fromFoldable

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

instance decodeParseError :: DecodeJson ParseError where
  decodeJson j = map fromPureParseError $ decodeJson j

instance encodeParseError :: EncodeJson ParseError where
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

parseString :: forall m. String -> Parsing.ParserT String m String
parseString = Prs.string

parseStringAs :: forall m v. String -> v -> Parsing.ParserT String m v
parseStringAs s v = Prs.string s <#> const v

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

adjustDateTime
  :: forall d
   . TimeDuration.Duration d
  => d
  -> DateTime.DateTime
  -> May.Maybe DateTime.DateTime
adjustDateTime = DateTime.adjust