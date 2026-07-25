module Z.Z.Dict
  ( Dict
  , dictEmpty
  , dictInsert
  , dictLookup
  ) where

import Prelude
import Z.Z.Key (class Keyed, keyStr)
import Z.Z.Core (mapM)
import Z.Z.Ext (class EncodeJson, class Generic, Json, Maybe(..), encodeJson, genericDecodeJson) as Z
import Data.Argonaut.Decode (decodeJson, class DecodeJson)

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

dictLookup :: forall k v. Keyed k => k -> Dict v -> Z.Maybe v
dictLookup k = js_lookup Z.Just Z.Nothing (keyStr k)

dictInsert :: forall k v. Keyed k => k -> v -> Dict v -> Dict v
dictInsert k = js_insert (keyStr k)

derive instance genericEncodedDict :: Z.Generic JsonEncodedDict _

instance decodeEncodedDict :: DecodeJson JsonEncodedDict where
  decodeJson x = Z.genericDecodeJson x

instance decodeDict :: DecodeJson v => DecodeJson (Dict v) where
  decodeJson x = do
    partial <- decodeJson x
    ty <- flip mapM (decodedKVs partial) $ \e -> decodeJson e.v <#> \f ->
      { k: e.k, v: f }
    pure $ js_fromEncodedDict ty
    where
    decodedKVs (JsonEncodedDict els) = els

instance encodeDict :: Z.EncodeJson v => Z.EncodeJson (Dict v) where
  encodeJson x = js_encode (Z.encodeJson) x