module Z.Z.PairKey
  ( PairKey(..)
  , pairKey'inv
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson) as Dec
import Data.Argonaut.Decode.Generic (genericDecodeJson) as DecodeGeneric
import Data.Argonaut.Encode (class EncodeJson) as Enc
import Data.Argonaut.Encode.Generic (genericEncodeJson) as EncodeGeneric
import Data.Generic.Rep (class Generic) as Generic
-- import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

data PairKey = Pos | Neg

derive instance Eq PairKey
derive instance Ord PairKey
derive instance Generic.Generic PairKey _

instance Show PairKey where
  show = genericShow

instance Dec.DecodeJson PairKey where
  decodeJson x = DecodeGeneric.genericDecodeJson x

instance Enc.EncodeJson PairKey where
  encodeJson x = EncodeGeneric.genericEncodeJson x

pairKey'inv :: PairKey -> PairKey
pairKey'inv Pos = Neg
pairKey'inv Neg = Pos

{-

newtype Pairkey = Pairkey Boolean

derive instance Newtype Pairkey _
derive newtype instance Eq Pairkey
derive newtype instance Ord Pairkey
derive newtype instance Dec.DecodeJson Pairkey
derive newtype instance Enc.EncodeJson Pairkey

-}