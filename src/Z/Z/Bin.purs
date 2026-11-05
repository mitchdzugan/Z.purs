module Z.Z.Bin
  ( Bin(..)
  , EncodedBin
  , JsonEncodedBin(..)
  , bin'empty
  , bin'insert
  , bin'lookup
  , bin'size
  , bin'vals
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Decode (decodeJson)
import Foreign.Object as Fo
import Z.Z.Defaultable (class Generable)
import Z.Z.Ext (wrap)
import Z.Z.Ext as Z
import Z.Z.Id (class Identable, ident'key)
import Z.Z.It (forM, mapM, obj'collect)
import Z.Z.Json.Decode (class XDecodeJson, xDecodeJson)

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

bin'size :: forall v. Bin v -> Int
bin'size = Fo.size <<< Z.unwrap

bin'vals :: forall v. Bin v -> Array v
bin'vals = Fo.values <<< Z.unwrap

derive instance Z.Newtype JsonEncodedBin _

derive newtype instance Z.EncodeJson JsonEncodedBin

derive newtype instance XDecodeJson JsonEncodedBin

instance XDecodeJson el => XDecodeJson (Bin el) where
  xDecodeJson json = do
    partial <- xDecodeJson @JsonEncodedBin json
    ty <- forM (Z.unwrap partial) \e -> xDecodeJson @el e.v <#> \f -> e.k Z./\ f
    pure $ Z.wrap $ obj'collect ty

instance Z.EncodeJson v => Z.EncodeJson (Bin v) where
  encodeJson (Bin x) = Z.encodeJson $ JsonEncodedBin $
    Fo.toArrayWithKey (\k v -> { k, v: Z.encodeJson v }) x

instance Generable (Bin v) gdesc (Bin v) where
  mkGenerable = bin'empty

