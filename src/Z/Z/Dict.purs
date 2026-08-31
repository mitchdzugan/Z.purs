module Z.Z.Dict
  ( Dict
  , dictEmpty
  , dictInsert
  , dictLookup
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Z.Z.Core (mapM)
import Z.Z.Ext
  ( class EncodeJson
  , class Generic
  , Json
  , Maybe(..)
  , encodeJson
  , genericDecodeJson
  ) as Z
import Z.Z.Key (class HasKey, keyStr)

foreign import data Dict :: Type -> Type

foreign import js_empty :: forall v. Dict v

foreign import js_insert :: forall v. String -> v -> Dict v -> Dict v

foreign import js_encode :: forall v. (v -> Z.Json) -> Dict v -> Z.Json

foreign import js_lookup
  :: forall v. (v -> Z.Maybe v) -> Z.Maybe v -> String -> Dict v -> Z.Maybe v

foreign import js_fromEncodedDict :: forall v. EncodedDict v -> Dict v

type EncodedDict v = Array { k :: String, v :: v }
newtype JsonEncodedDict = JsonEncodedDict (EncodedDict Z.Json)

dictEmpty :: forall v. Dict v
dictEmpty = js_empty

dictLookup :: forall k v. HasKey k => k -> Dict v -> Z.Maybe v
dictLookup k = js_lookup Z.Just Z.Nothing (keyStr k)

dictInsert :: forall k v. HasKey k => k -> v -> Dict v -> Dict v
dictInsert k = js_insert (keyStr k)

derive instance Z.Generic JsonEncodedDict _

instance DecodeJson JsonEncodedDict where
  decodeJson x = Z.genericDecodeJson x

instance DecodeJson v => DecodeJson (Dict v) where
  decodeJson x = do
    partial <- decodeJson x
    ty <- flip mapM (decodedKVs partial) $ \e -> decodeJson e.v <#> \f ->
      { k: e.k, v: f }
    pure $ js_fromEncodedDict ty
    where
    decodedKVs (JsonEncodedDict els) = els

instance Z.EncodeJson v => Z.EncodeJson (Dict v) where
  encodeJson x = js_encode (Z.encodeJson) x