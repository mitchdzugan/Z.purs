module Z.SSBM.Slp.Port
  ( T(..)
  ) where

import Prelude
import Z as Z

data T = P1 | P2 | P3 | P4 | NonOEM Int

derive instance eqT :: Eq T
derive instance ordT :: Ord T

derive instance genericT :: Z.Generic T _

instance decodeJsonT :: Z.DecodeJson T where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonT :: Z.EncodeJson T where
  encodeJson x = Z.genericEncodeJson x

instance showT :: Show T where
  show P1 = "p1"
  show P2 = "p2"
  show P3 = "p3"
  show P4 = "p4"
  show (NonOEM x) = "p{x}" <> show x

instance boundedT :: Bounded T where
  top = P4
  bottom = P1

instance enumT :: Z.Enum T where
  succ P4 = Z.Nothing
  succ P3 = Z.Just P4
  succ P2 = Z.Just P3
  succ P1 = Z.Just P2
  succ _ = Z.Nothing
  pred P4 = Z.Just P3
  pred P3 = Z.Just P2
  pred P2 = Z.Just P1
  pred P1 = Z.Nothing
  pred _ = Z.Nothing

instance boundedEnumT :: Z.BoundedEnum T where
  cardinality = Z.defaultCardinality
  toEnum = Z.defaultToEnum
  fromEnum = Z.defaultFromEnum
