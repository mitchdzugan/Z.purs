module Z.Z.Json.Decode
  ( DecoderX
  , EitherJson
  , JsonDecodeError'row
  , JsonDecodeError(..)
  , JsonDecodeErrorMap
  , Rec''XDecode
  , RunDecodeX
  , Var''XDecode
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
  , decodeJson'sync
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
import Data.Argonaut.Encode (class EncodeJson)
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
  , obj'lookup
  )
import Z.Z.Core as Z
import Z.Z.DateTime (DateTime)
import Z.Z.It (forM, iterable'kv'mk, list'collect, obj'collect)
import Z.Z.Passable (pass)
import Z.Z.Row (class Row'Mk, class Row'Mker, row'mker)
import Z.Z.SorN (SorN)
import Z.Z.Variant
  ( class Variant'Mk
  , class Variant'Mker
  , var'case_
  , var'encode
  , var'inj
  , var'inj_
  , var'on
  , variant'mkers
  )
import Z.Z.X.Core (Run)
import Z.Z.X.Index (type (@@>), E', R', e'fail, e'try, r'ask, r'run, sync'x)

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
  xDecodeJson :: forall x. Json -> DecoderX x @@> t

decodeJson'sync :: forall t. DecoderX () @@> t -> Either JsonDecodeError t
decodeJson'sync m = sync'x $ e'try $ r'run identity m

decodeJson :: forall @t. XDecodeJson t => Json -> Either JsonDecodeError t
decodeJson json = decodeJson'sync $ xDecodeJson @t json

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
  -> DecoderX x @@> a
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
    let nullFail = var'inj @"typeMismatch" "null"
    s <- caseJsonString (dec'fail nullFail) pure json
    when (s /= "") (dec'fail nullFail)

instance XDecodeJson String where
  xDecodeJson = caseJsonString (dec'fail $ dec'typeMismatch "String") pure

instance XDecodeJson Boolean where
  xDecodeJson = caseJsonBoolean (dec'fail $ dec'typeMismatch "Boolean") pure

instance XDecodeJson Number where
  xDecodeJson = caseJsonNumber (dec'fail $ dec'typeMismatch "Number") pure

instance XDecodeJson DateTime where
  xDecodeJson json = wrap <$> xDecodeJson @Number json

instance XDecodeJson SorN where
  xDecodeJson json = wrap <$> do
    e'try (xDecodeJson @String json) >>= flip eOr'_ (pure <<< Left) \_ -> do
      Right <$> xDecodeJson @Int json

instance XDecodeJson Int where
  xDecodeJson json = do
    let intErr = dec'typeMismatch "Integer"
    numOrE <- e'try $ xDecodeJson @Number json
    may'_ (dec'fail intErr) pure $ hush numOrE >>= fromNumber

instance XDecodeJson JsError where
  xDecodeJson json = map fromPureJsError $ xDecodeJson @PureJsError json

instance XDecodeJson Z.ParseError where
  xDecodeJson json = xDecodeJson @PureParseError json <#> fromPureParseError

instance XDecodeJson el => XDecodeJson (Maybe el) where
  xDecodeJson json = dec'wrapErrs (dec'named "Maybe") do
    caseJsonNull (Just <$> xDecodeJson @el json) (const pass) json

type EitherJson l r = Variant (l :: l, r :: r)

instance (XDecodeJson (EitherJson l r)) => XDecodeJson (Either l r) where
  xDecodeJson json = dec'wrapErrs (dec'named "Either") do
    xDecodeJson @(EitherJson l r) json <#>
      (var'case_ # var'on @"l" Left # var'on @"r" Right)

instance (XDecodeJson { l :: l, r :: r }) => XDecodeJson (l /\ r) where
  xDecodeJson json = dec'wrapErrs (dec'named "Tuple") do
    xDecodeJson @{ l :: l, r :: r } json <#> \t -> t.l /\ t.r

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
  -> DecoderX x @@> dest
decodeViaArray ofArr name json = dec'wrapErrs (dec'named name) do
  arr <- caseJsonArray (dec'fail $ dec'typeMismatch "Array") pure json
  ofArr <$> forM (iterable'kv'mk arr) \(ind /\ vJson) -> do
    dec'wrapErrs (dec'atIndex ind) $ xDecodeJson @el vJson

instance XDecodeJson el => XDecodeJson (Array el) where
  xDecodeJson = decodeViaArray identity "Array"

instance XDecodeJson el => XDecodeJson (List el) where
  xDecodeJson = decodeViaArray (list'collect) "List"

instance
  Row'Mker Rec''XDecode (Record r) (Object Json) (Either JsonDecodeError) =>
  XDecodeJson (Record r) where
  xDecodeJson json = do
    obj <- caseJsonObject (dec'fail $ dec'typeMismatch "Object") pure json
    dec'ok (row'mker @Rec''XDecode obj)

instance
  Variant'Mker Var''XDecode (Variant r) Json (Either JsonDecodeError) =>
  XDecodeJson (Variant r) where
  xDecodeJson json = var'decodeImpl json

instance XDecodeJson JsonDecodeError where
  xDecodeJson json = var'decodeImpl json

instance EncodeJson JsonDecodeError where
  encodeJson x = var'encode x

instance Show JsonDecodeError where
  show x = "" -- TODO

-------------------------------------------------------------------

foreign import data Rec''XDecode :: Type

instance
  ( XDecodeJson focus
  ) =>
  Row'Mk Rec''XDecode (Maybe focus) (Object Json) (Either JsonDecodeError) where
  row'mk key jsonObj = decodeJson'sync $ dec'wrapErrs (dec'atKey key) do
    let jsonOr_ = obj'lookup key jsonObj
    may'_ (pure Nothing) (map Just <<< xDecodeJson @focus) jsonOr_
else instance
  ( XDecodeJson focus
  ) =>
  Row'Mk Rec''XDecode focus (Object Json) (Either JsonDecodeError) where
  row'mk key jsonObj = decodeJson'sync $ dec'wrapErrs (dec'atKey key) do
    let jsonOr_ = obj'lookup key jsonObj
    may'_ (dec'fail dec'missingValue) (xDecodeJson @focus) jsonOr_

foreign import data Var''XDecode :: Type

type RunDecodeX x = (@@>) (DecoderX x)

instance
  ( XDecodeJson focus
  ) =>
  Variant'Mk Var''XDecode focus Json (Either JsonDecodeError) where
  variant'mk json = decodeJson'sync $ xDecodeJson @focus json

var'decodeImpl
  :: forall @to x
   . Variant'Mker Var''XDecode to Json (Either JsonDecodeError)
  => Json
  -> RunDecodeX x to
var'decodeImpl json = do
  xDecodeJson json >>= onDecodeGeneric
  where
  onDecodeGeneric :: { inj :: Json, at :: String } -> RunDecodeX x to
  onDecodeGeneric { inj, at } = may'_
    (dec'fail $ dec'atKey at $ dec'unexpectedValue inj)
    (\f -> dec'ok $ f inj)
    (obj'lookup at decoders)
  decoders = variant'mkers @Var''XDecode @to

----------------------------------------------------------------------

foreign import js_parseStringToJson
  :: (JsError -> Either JsonDecodeError Json)
  -> (Json -> Either JsonDecodeError Json)
  -> String
  -> Either JsonDecodeError Json

