module Z.Z.PairKey
  ( PairKey(..)
  , neg
  , pairKey'inv
  , pos
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson) as Dec
import Data.Argonaut.Encode (class EncodeJson) as Enc
import Data.Newtype (class Newtype)

newtype PairKey = PairKey Boolean

derive instance Newtype PairKey _
derive newtype instance Eq PairKey
derive newtype instance Ord PairKey
derive newtype instance Show PairKey
derive newtype instance Dec.DecodeJson PairKey
derive newtype instance Enc.EncodeJson PairKey

pairKey'inv :: PairKey -> PairKey
pairKey'inv pk = if pk == pos then neg else pos

pos :: PairKey
pos = PairKey true

neg :: PairKey
neg = PairKey false
