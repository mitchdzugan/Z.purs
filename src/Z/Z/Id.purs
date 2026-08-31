module Z.Z.Id
  ( IdV
  , IdVF(..)
  , Idented
  , class Identable
  , class IsId
  , id'bytes
  , id'bytesImpl
  , id'char
  , id'key
  , id'keyImpl
  , id'of
  , id'via
  , ident'bytes
  , ident'get
  , ident'key
  , ident'uuid
  , idented'mk
  , idented'v
  ) where

import Prelude

import Data.ByteString (fromByte)
import Data.Char (toCharCode)
import Data.String.CodeUnits (singleton)
import Z.Z.Core (arr'concat)
import Z.Z.Ext (Exists, mkExists, runExists)
import Z.Z.Ext as Z
import Z.Z.Pair as ZP
import Z.Z.String (str'joinWith)

foreign import js_bytesOfInt :: (Int -> Z.Byte) -> Int -> Array Z.Byte
foreign import js_bytesOfString :: (Int -> Z.Byte) -> String -> Array Z.Byte
foreign import js_keyOfByteInts :: Array Int -> String

byte'ofChar :: Char -> Z.Byte
byte'ofChar = Z.byte <<< toCharCode

newtype IdVF a = IdVF
  { id'char :: a -> Char
  , id'keyImpl :: a -> String
  , id'bytesImpl :: a -> Array Z.Byte
  , v :: a
  }

type IdV = Exists IdVF

id'of :: forall i. IsId i => i -> IdV
id'of v = mkExists $ IdVF { id'char, id'keyImpl, id'bytesImpl, v }

id'via :: forall t. Identable t => t -> IdV
id'via = id'of <<< ident'get

class IsId a where
  id'char :: a -> Char
  id'keyImpl :: a -> String
  id'bytesImpl :: a -> Array Z.Byte

instance IsId IdV where
  id'char = runExists handleIdVF
    where
    handleIdVF :: forall a. IdVF a -> Char
    handleIdVF (IdVF e) = e.id'char e.v
  id'keyImpl = runExists handleIdVF
    where
    handleIdVF :: forall a. IdVF a -> String
    handleIdVF (IdVF e) = e.id'keyImpl e.v
  id'bytesImpl = runExists handleIdVF
    where
    handleIdVF :: forall a. IdVF a -> Array Z.Byte
    handleIdVF (IdVF e) = e.id'bytesImpl e.v

instance IsId Unit where
  id'char _ = 'u'
  id'keyImpl _ = ""
  id'bytesImpl _ = []

instance IsId Boolean where
  id'char true = 't'
  id'char false = 'f'
  id'keyImpl _ = ""
  id'bytesImpl _ = []

instance IsId Int where
  id'char _ = 'i'
  id'keyImpl = show
  id'bytesImpl i =
    if i >= 0 && i < 100 then js_bytesOfString Z.byte $ show i
    else js_bytesOfInt Z.byte i

instance IsId String where
  id'char _ = 's'
  id'keyImpl = identity
  id'bytesImpl = js_bytesOfString Z.byte

instance IsId Number where
  id'char _ = 'n'
  id'keyImpl = show
  id'bytesImpl = js_bytesOfString Z.byte <<< show

instance IsId t => IsId (Z.Maybe t) where
  id'char Z.Nothing = '_'
  id'char _ = 'j'
  id'keyImpl Z.Nothing = ""
  id'keyImpl (Z.Just v) = id'keyImpl v
  id'bytesImpl Z.Nothing = []
  id'bytesImpl (Z.Just v) = id'bytesImpl v

instance (IsId l, IsId r) => IsId (l Z./\ r) where
  id'char _ = 'p'
  id'keyImpl (l Z./\ r) = id'keyImpl l <> id'keyImpl r
  id'bytesImpl (l Z./\ r) = id'bytesImpl l <> id'bytesImpl r

instance (IsId t) => IsId (ZP.Pair t) where
  id'char _ = 'p'
  id'keyImpl (l ZP.~ r) = id'keyImpl l <> id'keyImpl r
  id'bytesImpl (l ZP.~ r) = id'bytesImpl l <> id'bytesImpl r

instance IsId (Array Z.Byte) where
  id'char _ = 'b'
  id'keyImpl = key'ofBytes
  id'bytesImpl = identity
else instance (IsId t) => IsId (Array t) where
  id'char _ = 'a'
  id'keyImpl = str'joinWith "" <<< map id'keyImpl
  id'bytesImpl = arr'concat <<< map id'bytesImpl

id'key :: forall i. IsId i => i -> String
id'key i = singleton (id'char i) <> id'keyImpl i

id'bytes :: forall i. IsId i => i -> Array Z.Byte
id'bytes i = [ byte'ofChar $ id'char i ] <> id'bytesImpl i

class Identable t where
  ident'get :: t -> IdV

instance Identable IdV where
  ident'get = identity

instance Identable Unit where
  ident'get = id'of

instance Identable Boolean where
  ident'get = id'of

instance Identable Int where
  ident'get = id'of

instance Identable String where
  ident'get = id'of

instance Identable Number where
  ident'get = id'of

instance Identable t => Identable (Z.Maybe t) where
  ident'get = id'of <<< map ident'get

instance (Identable l, Identable r) => Identable (l Z./\ r) where
  ident'get (l Z./\ r) = id'of $ ident'get l Z./\ ident'get r

instance (Identable t) => Identable (ZP.Pair t) where
  ident'get (l ZP.~ r) = id'of $ ident'get l ZP.~ ident'get r

instance Identable (Array Z.Byte) where
  ident'get = id'of
else instance (Identable t) => Identable (Array t) where
  ident'get = id'of <<< map ident'get

instance IsId id => Identable (Idented id v) where
  ident'get (Idented (id Z./\ _)) = id'of id

ident'key :: forall i. Identable i => i -> String
ident'key = id'key <<< ident'get

ident'bytes :: forall i. Identable i => i -> Array Z.Byte
ident'bytes = id'bytes <<< ident'get

key'ofBytes :: Array Z.Byte -> String
key'ofBytes = js_keyOfByteInts <<< map fromByte

ident'uuid :: forall i. Identable i => i -> String
ident'uuid = key'ofBytes <<< ident'bytes

newtype Idented id v = Idented (id Z./\ v)

idented'mk :: forall id v. IsId id => id -> v -> Idented id v
idented'mk id v = Idented $ id Z./\ v

idented'v :: forall id v. Idented id v -> v
idented'v (Idented (_ Z./\ v)) = v
