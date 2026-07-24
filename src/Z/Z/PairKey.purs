module Z.Z.PairKey
  ( PairKey(..)
  ) where

import Prelude
import Data.Argonaut.Decode (class DecodeJson) as Dec
import Data.Argonaut.Decode.Generic (genericDecodeJson) as DecodeGeneric
import Data.Argonaut.Encode (class EncodeJson) as Enc
import Data.Argonaut.Encode.Generic (genericEncodeJson) as EncodeGeneric
import Data.Generic.Rep (class Generic) as Generic

data PairKey = Up | Down

derive instance eqUser :: Eq PairKey
derive instance ordUser :: Ord PairKey
derive instance genericT :: Generic.Generic PairKey _

instance decodeJsonT :: Dec.DecodeJson PairKey where
  decodeJson x = DecodeGeneric.genericDecodeJson x

instance encodeJsonT :: Enc.EncodeJson PairKey where
  encodeJson x = EncodeGeneric.genericEncodeJson x