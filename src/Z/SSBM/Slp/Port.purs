module Z.SSBM.Slp.Port
  ( T(..)
  , asInt
  , ofInt
  ) where

import Z.Prelude

data T = P1 | P2 | P3 | P4 | NonOEM Int

ofInt :: Int -> T
ofInt 1 = P1
ofInt 2 = P2
ofInt 3 = P3
ofInt 4 = P4
ofInt n = NonOEM n

asInt :: T -> Int
asInt P1 = 1
asInt P2 = 2
asInt P3 = 3
asInt P4 = 4
asInt (NonOEM n) = n

derive instance eqT :: Eq T
derive instance ordT :: Ord T

derive instance genericT :: Generic T _

instance decodeJsonT :: DecodeJson T where
  decodeJson x = genericDecodeJson x

instance encodeJsonT :: EncodeJson T where
  encodeJson x = genericEncodeJson x

instance showT :: Show T where
  show P1 = "p1"
  show P2 = "p2"
  show P3 = "p3"
  show P4 = "p4"
  show (NonOEM x) = "p{x}" <> show x

instance boundedT :: Bounded T where
  top = P4
  bottom = P1

instance enumT :: Enum T where
  succ P4 = Nothing
  succ P3 = Just P4
  succ P2 = Just P3
  succ P1 = Just P2
  succ _ = Nothing
  pred P4 = Just P3
  pred P3 = Just P2
  pred P2 = Just P1
  pred P1 = Nothing
  pred _ = Nothing

instance boundedEnumT :: BoundedEnum T where
  cardinality = defaultCardinality
  toEnum = defaultToEnum
  fromEnum = defaultFromEnum
