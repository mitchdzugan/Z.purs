module Z.Z.Bin
  ( Bin(..)
  , bin'empty
  , bin'fromFoldable
  , bin'insert
  , bin'lookup
  , bin'size
  , bin'vals
  ) where

import Prelude

import Data.Argonaut.Decode (decodeJson)
import Foreign.Object as Fo
import Z.Z.Core (arr'fromFoldable, mapM)
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable, ident'key)

newtype Bin a = Bin (Fo.Object a)

derive instance Z.Newtype (Bin a) _
derive instance Functor Bin

type EncodedBin v = Array { k :: String, v :: v }
newtype JsonEncodedBin = JsonEncodedBin (EncodedBin Z.Json)

bin'empty :: forall v. Bin v
bin'empty = Bin Fo.empty

bin'lookup :: forall k v. Identable k => k -> Bin v -> Z.Maybe v
bin'lookup k = Fo.lookup (ident'key k) <<< Z.unwrap

bin'insert :: forall k v. Identable k => k -> v -> Bin v -> Bin v
bin'insert k v (Bin o) = Bin $ Fo.insert (ident'key k) v o

bin'fromFoldable
  :: forall k v f. Identable k => Z.Foldable f => f (k Z./\ v) -> Bin v
bin'fromFoldable f = Bin $ Fo.fromFoldable $ flip map (arr'fromFoldable f)
  \(k Z./\ v) -> ident'key k Z./\ v

bin'size :: forall v. Bin v -> Int
bin'size = Fo.size <<< Z.unwrap

bin'vals :: forall v. Bin v -> Array v
bin'vals = Fo.values <<< Z.unwrap

derive instance Z.Generic JsonEncodedBin _

instance Z.DecodeJson JsonEncodedBin where
  decodeJson x = Z.genericDecodeJson x

instance Z.DecodeJson v => Z.DecodeJson (Bin v) where
  decodeJson x = do
    partial <- decodeJson x
    ty <- flip mapM (decodedKVs partial) $ \e -> decodeJson e.v <#> \f ->
      e.k Z./\ f
    pure $ bin'fromFoldable ty
    where
    decodedKVs (JsonEncodedBin els) = els

instance Z.EncodeJson v => Z.EncodeJson (Bin v) where
  encodeJson (Bin x) = Z.encodeJson $ Fo.toArrayWithKey (\k v -> { k, v }) x

instance Generable (Bin v) gdesc (Bin v) where
  mkGenerable = bin'empty

