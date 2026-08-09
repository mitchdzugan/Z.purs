module Z.Z.PairKey
  ( PairKey(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson) as Dec
import Data.Argonaut.Decode.Generic (genericDecodeJson) as DecodeGeneric
import Data.Argonaut.Encode (class EncodeJson) as Enc
import Data.Argonaut.Encode.Generic (genericEncodeJson) as EncodeGeneric
import Data.Generic.Rep (class Generic) as Generic

data PairKey = Pos | Neg

derive instance Eq PairKey
derive instance Ord PairKey
derive instance Generic.Generic PairKey _
instance Dec.DecodeJson PairKey where
  decodeJson x = DecodeGeneric.genericDecodeJson x

instance Enc.EncodeJson PairKey where
  encodeJson x = EncodeGeneric.genericEncodeJson x