module Z.Z.Bin
  ( Bin
  , bin'empty
  , bin'fromFoldable
  , bin'insert
  , bin'lookup
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Z.Z.Core (arr'fromFoldable, mapM)
import Z.Z.Ext
  ( class EncodeJson
  , class Generic
  , type (/\)
  , Json
  , Maybe(..)
  , encodeJson
  , genericDecodeJson
  , (/\)
  ) as Z
import Z.Z.Ext (class Foldable)
import Z.Z.Key (class Keyed, keyStr)

foreign import data Bin :: Type -> Type

foreign import js_empty :: forall v. Bin v

foreign import js_insert :: forall v. String -> v -> Bin v -> Bin v

foreign import js_encode :: forall v. (v -> Z.Json) -> Bin v -> Z.Json

foreign import js_lookup
  :: forall v. (v -> Z.Maybe v) -> Z.Maybe v -> String -> Bin v -> Z.Maybe v

foreign import js_fromKVs :: forall v. EncodedBin v -> Bin v

type EncodedBin v = Array { k :: String, v :: v }
newtype JsonEncodedBin = JsonEncodedBin (EncodedBin Z.Json)

bin'empty :: forall v. Bin v
bin'empty = js_empty

bin'lookup :: forall k v. Keyed k => k -> Bin v -> Z.Maybe v
bin'lookup k = js_lookup Z.Just Z.Nothing (keyStr k)

bin'insert :: forall k v. Keyed k => k -> v -> Bin v -> Bin v
bin'insert k = js_insert (keyStr k)

bin'fromFoldable :: forall k v f. Keyed k => Foldable f => f (k Z./\ v) -> Bin v
bin'fromFoldable =
  js_fromKVs <<< map (\(k Z./\ v) -> { k: keyStr k, v }) <<< arr'fromFoldable

derive instance Z.Generic JsonEncodedBin _

instance DecodeJson JsonEncodedBin where
  decodeJson x = Z.genericDecodeJson x

instance DecodeJson v => DecodeJson (Bin v) where
  decodeJson x = do
    partial <- decodeJson x
    ty <- flip mapM (decodedKVs partial) $ \e -> decodeJson e.v <#> \f ->
      { k: e.k, v: f }
    pure $ js_fromKVs ty
    where
    decodedKVs (JsonEncodedBin els) = els

instance Z.EncodeJson v => Z.EncodeJson (Bin v) where
  encodeJson x = js_encode (Z.encodeJson) x