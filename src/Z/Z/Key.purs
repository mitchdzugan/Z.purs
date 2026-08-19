module Z.Z.Key
  ( Key
  , class Keyed
  , key
  , keyStr
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

class Keyed a where
  key :: a -> Key

instance Keyed Key where
  key k = k

instance Keyed Boolean where
  key true = Key $ "T"
  key false = Key $ "F"

instance Keyed String where
  key s = Key $ "S" <> js_keyOfStr s

instance Keyed Number where
  key n = Key $ "N" <> js_keyOfStr (show n)

instance Keyed Int where
  key i = Key $ "I" <> js_keyOfInt i

instance Keyed a => Keyed (Z.Maybe a) where
  key Z.Nothing = Key "_"
  key (Z.Just v) = key v

instance (Keyed a, Keyed b) => Keyed (a Z./\ b) where
  key (a Z./\ b) =
    let ka = keyStr a in Key $ "P" <> show (str'length ka) <> ka <> keyStr b

instance (Keyed a) => Keyed (ZP.Pair a) where
  key (a ZP.~ b) =
    let ka = keyStr a in Key $ "P" <> show (str'length ka) <> ka <> keyStr b

instance Keyed (Array Z.Byte) where
  key a = Key $ "X" <> js_keyOfBytes (a <#> Z.fromByte)

else instance (Keyed a) => Keyed (Array a) where
  key a = Key $ "A" <> js_keyOfAKeys (a <#> keyStr)

keyStr :: forall k. Keyed k => k -> String
keyStr = _str <<< key
  where
  _str (Key s) = s