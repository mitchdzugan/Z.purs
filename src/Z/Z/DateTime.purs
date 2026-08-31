module Z.Z.DateTime
  ( DateTime(..)
  , adjustDateTime
  , dateTime'fromMS
  , dateTime'fromMS'Int
  , dateTime'month'i0
  , dateTime'toMS
  , dateTime'year
  , fromRawDateTime
  , toDateTime
  ) where

import Prelude

import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson) as Dec
import Data.DateTime as DateTime
import Data.DateTime.Instant as DateTimeInst
import Data.Enum (fromEnum)
import Data.Maybe as May
import Data.Time.Duration as TimeDuration
import Z.Z.Ext as Z

data DateTime = DateTime DateTime.Date DateTime.Time

fromRawDateTime :: DateTime.DateTime -> DateTime
fromRawDateTime = data_to_impl

data_to_impl :: DateTime.DateTime -> DateTime
data_to_impl dt = DateTime (DateTime.date dt) (DateTime.time dt)

impl_to_data :: DateTime -> DateTime.DateTime
impl_to_data (DateTime date time) = DateTime.DateTime date time

toDateTime :: DateTimeInst.Instant -> DateTime
toDateTime = data_to_impl <<< DateTimeInst.toDateTime

adjustDateTime
  :: forall d
   . TimeDuration.Duration d
  => d
  -> DateTime
  -> May.Maybe DateTime
adjustDateTime dur dt = impl_to_data dt # DateTime.adjust dur <#> data_to_impl

derive instance Z.Generic DateTime _

derive instance Eq DateTime

instance Ord DateTime where
  compare d1 d2 = compare (dateTime'toMS d1) (dateTime'toMS d2)

instance Z.DecodeJson DateTime where
  decodeJson x = Dec.decodeJson x <#> Z.Milliseconds <#> DateTimeInst.instant
    >>= fromInst
    where
    fromInst Z.Nothing = Z.Left $ Dec.TypeMismatch "Invalid date"
    fromInst (Z.Just i) = Z.Right $ data_to_impl $ DateTimeInst.toDateTime i

instance Z.EncodeJson DateTime where
  encodeJson = impl_to_data
    >>> DateTimeInst.fromDateTime
    >>> DateTimeInst.unInstant
    >>> Z.unwrap
    >>> Z.encodeJson

dateTime'toMS :: DateTime -> Number
dateTime'toMS = impl_to_data
  >>> DateTimeInst.fromDateTime
  >>> DateTimeInst.unInstant
  >>> Z.unwrap

dateTime'fromMS :: Number -> May.Maybe DateTime
dateTime'fromMS n = Z.Milliseconds n # DateTimeInst.instant
  <#> DateTimeInst.toDateTime
  <#> data_to_impl

dateTime'fromMS'Int :: Int -> May.Maybe DateTime
dateTime'fromMS'Int = dateTime'fromMS <<< Z.toNumber

dateTime'month'i0 :: DateTime -> Int
dateTime'month'i0 (DateTime date _) =
  case (DateTime.month date) of
    DateTime.January -> 0
    DateTime.February -> 1
    DateTime.March -> 2
    DateTime.April -> 3
    DateTime.May -> 4
    DateTime.June -> 5
    DateTime.July -> 6
    DateTime.August -> 7
    DateTime.September -> 8
    DateTime.October -> 9
    DateTime.November -> 10
    DateTime.December -> 11

dateTime'year :: DateTime -> Int
dateTime'year (DateTime date _) = fromEnum $ DateTime.year date