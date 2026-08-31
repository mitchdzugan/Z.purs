module Z.Z.Key
  ( Key
  , Keyed(..)
  , class HasKey
  , key
  , keyStr
  , keyed'k
  , keyed'v
  ) where

import Prelude

import Z.Z.Ext as Z
import Z.Z.Pair as ZP
import Z.Z.String (str'length)

foreign import js_keyOfStr :: String -> String

foreign import js_keyOfInt :: Int -> String

foreign import js_keyOfAKeys :: Array String -> String

foreign import js_keyOfBytes :: Array Int -> String

newtype Key = Key String

class HasKey a where
  key :: a -> Key

instance HasKey Key where
  key k = k

instance HasKey Boolean where
  key true = Key $ "T"
  key false = Key $ "F"

instance HasKey String where
  key s = Key $ "S" <> js_keyOfStr s

instance HasKey Number where
  key n = Key $ "N" <> js_keyOfStr (show n)

instance HasKey Int where
  key i = Key $ "I" <> js_keyOfInt i

instance HasKey a => HasKey (Z.Maybe a) where
  key Z.Nothing = Key "_"
  key (Z.Just v) = key v

instance (HasKey a, HasKey b) => HasKey (a Z./\ b) where
  key (a Z./\ b) =
    let ka = keyStr a in Key $ "P" <> show (str'length ka) <> ka <> keyStr b

instance (HasKey a) => HasKey (ZP.Pair a) where
  key (a ZP.~ b) =
    let ka = keyStr a in Key $ "P" <> show (str'length ka) <> ka <> keyStr b

instance HasKey (Array Z.Byte) where
  key a = Key $ "X" <> js_keyOfBytes (a <#> Z.fromByte)

else instance (HasKey a) => HasKey (Array a) where
  key a = Key $ "A" <> js_keyOfAKeys (a <#> keyStr)

keyStr :: forall k. HasKey k => k -> String
keyStr = _str <<< key
  where
  _str (Key s) = s

newtype Keyed k v = Keyed (k Z./\ v)

derive instance Z.Newtype (Keyed k v) _

keyed'k :: forall k v. Keyed k v -> k
keyed'k (Keyed (k Z./\ _)) = k

keyed'v :: forall k v. Keyed k v -> v
keyed'v (Keyed (_ Z./\ v)) = v

instance (HasKey k) => HasKey (Keyed k v) where
  key = key <<< keyed'k