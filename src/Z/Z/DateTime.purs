module Z.Z.DateTime
  ( DateTime(..)
  , adjustDateTime
  , toDateTime
  ) where

import Prelude

import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson) as Dec
import Data.DateTime as DateTime
import Data.DateTime.Instant as DateTimeInst
import Data.Maybe as May
import Data.Time.Duration as TimeDuration
import Z.Z.Ext as Z

data DateTime = DateTime DateTime.Date DateTime.Time

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

derive instance genericDateTime :: Z.Generic DateTime _

derive instance eqDateTime ∷ Eq a => Eq DateTime

instance decodeDateTime :: Z.DecodeJson DateTime where
  decodeJson x = Dec.decodeJson x <#> Z.Milliseconds <#> DateTimeInst.instant
    >>= fromInst
    where
    fromInst Z.Nothing = Z.Left $ Dec.TypeMismatch "Invalid date"
    fromInst (Z.Just i) = Z.Right $ data_to_impl $ DateTimeInst.toDateTime i

instance encodeDateTime :: Z.EncodeJson DateTime where
  encodeJson = impl_to_data
    >>> DateTimeInst.fromDateTime
    >>> DateTimeInst.unInstant
    >>> Z.unwrap
    >>> Z.encodeJson