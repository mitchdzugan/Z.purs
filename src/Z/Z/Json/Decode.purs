module Z.Z.Json.Decode
  ( DecoderX
  , EitherJson
  , JsonDecodeError'row
  , JsonDecodeError(..)
  , JsonDecodeErrorMap
  , Rec''XDecode
  , RunDecodeX
  , Var''XDecode
  , X''XDecode
  , class XDecodeJson
  , dec'atIndex
  , dec'atKey
  , dec'custom
  , dec'fail
  , dec'missingValue
  , dec'named
  , dec'ok
  , dec'parseStringErr
  , dec'typeMismatch
  , dec'unexpectedValue
  , dec'wrapErrs
  , decode
  , decodeForeign
  , decodeJson
  , decodeViaArray
  , js_parseStringToJson
  , xDecodeJson
  , xDecodeViaString
  ) where

import Prelude

import Data.Argonaut.Core
  ( Json
  , caseJsonArray
  , caseJsonBoolean
  , caseJsonNull
  , caseJsonNumber
  , caseJsonObject
  , caseJsonString
  )
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..), hush)
import Data.Int (fromNumber)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Foreign (Foreign)
import Foreign.Object (Object)
import Z.Z.Core
  ( JsError
  , PureJsError
  , PureParseError
  , eOr'_
  , fromPureJsError
  , fromPureParseError
  , jsonFromForeign
  , mapL
  , may'_
  , may'filter
  , obj'lookup
  )
import Z.Z.Core as Z
import Z.Z.It (forM, iterable'kv'mk, list'collect, obj'collect)
import Z.Z.Passable (pass)
import Z.Z.Row (class Row'Mk, class Row'Mker, row'mker)
import Z.Z.Run (ctx'expand)
import Z.Z.SorN (SorN)
import Z.Z.Util (nth)
import Z.Z.Variant
  ( class Variant'Mk
  , class Variant'Mker
  , var'encode
  , var'inj
  , var'inj_
  , variant'mkers
  )
import Z.Z.X.Core (Run)
import Z.Z.X.Index
  ( type (@@->)
  , E'
  , R'
  , X'R
  , e'fail
  , e'try
  , r'ask
  , r'run
  , r'run''
  , sync'x
  )
import Z.Z.X.Method (x'extract)

type JsonDecodeError'row =
  ( typeMismatch :: String
  , custom :: String
  , unexpectedValue :: Json
  , atIndex :: Int /\ JsonDecodeError
  , atKey :: String /\ JsonDecodeError
  , named :: String /\ JsonDecodeError
  , missingValue :: Unit
  , parseStringErr :: JsError
  )

dec'typeMismatch :: String -> JsonDecodeError
dec'typeMismatch = var'inj @"typeMismatch"

dec'custom :: String -> JsonDecodeError
dec'custom = var'inj @"custom"

dec'unexpectedValue :: Json -> JsonDecodeError
dec'unexpectedValue = var'inj @"unexpectedValue"

dec'atIndex :: Int -> JsonDecodeError -> JsonDecodeError
dec'atIndex = curry (var'inj @"atIndex")

dec'atKey :: String -> JsonDecodeError -> JsonDecodeError
dec'atKey = curry (var'inj @"atKey")

dec'named :: String -> JsonDecodeError -> JsonDecodeError
dec'named = curry (var'inj @"named")

dec'missingValue :: JsonDecodeError
dec'missingValue = var'inj_ @"missingValue"

dec'parseStringErr :: JsError -> JsonDecodeError
dec'parseStringErr = var'inj @"parseStringErr"

newtype JsonDecodeError = JsonDecodeError (Variant JsonDecodeError'row)

derive instance Newtype JsonDecodeError _

type JsonDecodeErrorMap = JsonDecodeError -> JsonDecodeError

type DecoderX x = E' JsonDecodeError (R' JsonDecodeErrorMap x)

class XDecodeJson t where
  xDecodeJson :: forall x. Json -> DecoderX x @@-> t

decodeJson :: forall @t. XDecodeJson t => Json -> Either JsonDecodeError t
decodeJson json = sync'x $ e'try $ r'run identity $ xDecodeJson @t json

decode :: forall @t. XDecodeJson t => String -> Either JsonDecodeError t
decode s =
  js_parseStringToJson (Left <<< dec'parseStringErr) Right s >>= decodeJson

decodeForeign :: forall @v. XDecodeJson v => Foreign -> Either JsonDecodeError v
decodeForeign = jsonFromForeign >>> decodeJson @v

---------------------------------

xDecodeViaString
  :: forall a x
   . (String -> Either String a)
  -> Json
  -> DecoderX x @@-> a
xDecodeViaString ofString json = do
  xDecodeJson @String json >>= dec'ok <<< mapL dec'custom <<< ofString

---------------------------------

dec'wrapErrs
  :: forall x res
   . JsonDecodeErrorMap
  -> Run (R' JsonDecodeErrorMap (DecoderX x)) res
  -> Run (DecoderX x) res
dec'wrapErrs eMap m = do
  currMap <- r'ask
  r'run (currMap <<< eMap) m

dec'fail :: forall x res. JsonDecodeError -> Run (DecoderX x) res
dec'fail e = r'ask >>= \eMap -> e'fail (eMap e)

dec'ok :: forall x res. Either JsonDecodeError res -> Run (DecoderX x) res
dec'ok = eOr'_ dec'fail pure

instance XDecodeJson Json where
  xDecodeJson json = pure json

instance XDecodeJson Unit where
  xDecodeJson json = do
    {-
    let nullFail = var'inj @"typeMismatch" "null"
    s <- caseJsonString (dec'fail nullFail) pure json
    when (s /= "") (dec'fail nullFail)
    -}
    caseAble caseJsonNull "null" json <#> const unit

caseAble
  :: forall x a
   . ( (DecoderX x @@-> a)
       -> (a -> DecoderX x @@-> a)
       -> Json
       -> DecoderX x @@-> a
     )
  -> String
  -> Json
  -> DecoderX x @@-> a
caseAble caser tname = caser (dec'fail $ dec'typeMismatch tname) pure

instance XDecodeJson String where
  xDecodeJson = caseAble caseJsonString "String"

instance XDecodeJson Boolean where
  xDecodeJson = caseAble caseJsonBoolean "Boolean"

instance XDecodeJson Number where
  xDecodeJson = caseAble caseJsonNumber "Number"

instance XDecodeJson SorN where
  xDecodeJson json = wrap <$> do
    e'try (xDecodeJson @String json) >>= flip eOr'_ (pure <<< Left) \_ -> do
      Right <$> xDecodeJson @Int json

instance XDecodeJson Int where
  xDecodeJson json = do
    let intErr = dec'typeMismatch "Integer"
    numOrE <- e'try $ xDecodeJson @Number json
    may'_ (dec'fail intErr) pure $ hush numOrE >>= fromNumber

type EitherJson l r = Variant (l :: l, r :: r)

instance XDecodeJson JsError where
  xDecodeJson json = map fromPureJsError $ xDecodeJson @PureJsError json

instance XDecodeJson Z.ParseError where
  xDecodeJson json = xDecodeJson @PureParseError json <#> fromPureParseError

instance XDecodeJson el => XDecodeJson (Maybe el) where
  xDecodeJson json = dec'wrapErrs (dec'named "Maybe") do
    caseJsonNull (Just <$> xDecodeJson @el json) (const pass) json

instance (XDecodeJson l, XDecodeJson r) => XDecodeJson (Either l r) where
  xDecodeJson json = dec'wrapErrs (dec'named "Either") do
    xDecodeJson @{ tag :: String, value :: Json } json >>= \i ->
      case i.tag of
        "Left" -> Left <$> xDecodeJson @l i.value
        "Right" -> Right <$> xDecodeJson @r i.value
        _ -> dec'fail $ dec'unexpectedValue json

instance (XDecodeJson l, XDecodeJson r) => XDecodeJson (l /\ r) where
  xDecodeJson json = dec'wrapErrs (dec'named "Tuple") do
    xDecodeJson @(Array Json) json >>= \els -> do
      let lOr_ = nth els 0
      let rOr_ = nth els 1
      case (lOr_ /\ rOr_) of
        (Just lj /\ Just rj) -> do
          l <- xDecodeJson @l lj
          r <- xDecodeJson @r rj
          pure $ l /\ r
        (Just _ /\ _) -> dec'fail $ dec'atIndex 1 dec'missingValue
        _ -> dec'fail $ dec'atIndex 0 dec'missingValue

instance XDecodeJson el => XDecodeJson (Object el) where
  xDecodeJson json = dec'wrapErrs (dec'named "ForeignObject") do
    obj <- caseJsonObject (dec'fail $ dec'typeMismatch "Object") pure json
    obj'collect <$> forM (iterable'kv'mk obj) \(k /\ vJson) -> do
      dec'wrapErrs (dec'atKey k) do
        v <- xDecodeJson @el vJson
        pure (k /\ v)

decodeViaArray
  :: forall el dest x
   . XDecodeJson el
  => (Array el -> dest)
  -> String
  -> Json
  -> DecoderX x @@-> dest
decodeViaArray ofArr name json = dec'wrapErrs (dec'named name) do
  arr <- caseJsonArray (dec'fail $ dec'typeMismatch "Array") pure json
  ofArr <$> forM (iterable'kv'mk arr) \(ind /\ vJson) -> do
    dec'wrapErrs (dec'atIndex ind) $ xDecodeJson @el vJson

instance XDecodeJson el => XDecodeJson (Array el) where
  xDecodeJson = decodeViaArray identity "Array"

instance XDecodeJson el => XDecodeJson (List el) where
  xDecodeJson = decodeViaArray (list'collect) "List"

instance
  Row'Mker (Rec''XDecode) (Record r) (X''XDecode) =>
  XDecodeJson (Record r) where
  xDecodeJson json = do
    obj <- caseJsonObject (dec'fail $ dec'typeMismatch "Object") pure json
    r'run'' @"obj" obj
      (ctx'expand (row'mker @(Rec''XDecode) @(Record r)))

instance
  Variant'Mker Var''XDecode (Variant r) Json ((@@->) (DecoderX ())) =>
  XDecodeJson (Variant r) where
  xDecodeJson json = var'decodeImpl json

instance XDecodeJson JsonDecodeError where
  xDecodeJson json = var'decodeImpl json

instance EncodeJson JsonDecodeError where
  encodeJson x = var'encode x

instance Show JsonDecodeError where
  show x = "" -- TODO

-------------------------------------------------------------------

foreign import data Rec''XDecode :: Row Type -> Type

type X''XDecode = (@@->) (DecoderX (obj :: X'R (Object Json)))

instance
  ( XDecodeJson focus
  ) =>
  Row'Mk (Rec''XDecode) (Maybe focus) (X''XDecode) where
  row'mk key = do
    jsonObj <- x'extract @"obj"
    dec'wrapErrs (dec'atKey key) do
      let removeNull = caseJsonNull true (const false)
      let jsonOr_ = may'filter removeNull $ obj'lookup key jsonObj
      may'_ (pure Nothing) (map Just <<< xDecodeJson @focus) jsonOr_
else instance
  ( XDecodeJson focus
  ) =>
  Row'Mk (Rec''XDecode) focus (X''XDecode) where
  row'mk key = do
    jsonObj <- x'extract @"obj"
    dec'wrapErrs (dec'atKey key) do
      let jsonOr_ = obj'lookup key jsonObj
      may'_
        (dec'fail $ dec'unexpectedValue $ encodeJson jsonObj)
        (xDecodeJson @focus)
        jsonOr_

foreign import data Var''XDecode :: Type

type RunDecodeX x = (@@->) (DecoderX x)

instance
  ( XDecodeJson focus
  ) =>
  Variant'Mk Var''XDecode focus Json ((@@->) (DecoderX ())) where
  variant'mk json = xDecodeJson @focus json

var'decodeImpl
  :: forall @to x
   . Variant'Mker Var''XDecode to Json ((@@->) (DecoderX ()))
  => Json
  -> RunDecodeX x to
var'decodeImpl json = do
  xDecodeJson json >>= onDecodeGeneric
  where
  onDecodeGeneric :: { inj :: Json, at :: String } -> RunDecodeX x to
  onDecodeGeneric { inj, at } = may'_
    (dec'fail $ dec'atKey at $ dec'unexpectedValue inj)
    (\f -> ctx'expand (f inj))
    (obj'lookup at decoders)
  decoders = variant'mkers @Var''XDecode @to

----------------------------------------------------------------------

foreign import js_parseStringToJson
  :: (JsError -> Either JsonDecodeError Json)
  -> (Json -> Either JsonDecodeError Json)
  -> String
  -> Either JsonDecodeError Json

