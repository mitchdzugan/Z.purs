module Z.Z.PairKey
  ( PairKey(..)
  , neg
  , pairKey'inv
  , pos
  ) where

import Prelude

import Data.Argonaut.Encode (class EncodeJson) as Enc
import Data.Newtype (class Newtype, unwrap)
import Z.Z.Id (class Identable, id'of)
import Z.Z.Json.Decode (class XDecodeJson)

newtype PairKey = PairKey Boolean

derive instance Newtype PairKey _
derive newtype instance Eq PairKey
derive newtype instance Ord PairKey
derive newtype instance Show PairKey
derive newtype instance XDecodeJson PairKey
derive newtype instance Enc.EncodeJson PairKey

pairKey'inv :: PairKey -> PairKey
pairKey'inv pk = if pk == pos then neg else pos

pos :: PairKey
pos = PairKey true

neg :: PairKey
neg = PairKey false

instance Identable PairKey where
  ident'get = id'of <<< unwrap
