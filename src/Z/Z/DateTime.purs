module Z.Z.DateTime
  ( (:+:)
  , (:~:)
  , DateTime(..)
  , TS'hours(..)
  , TS'milliseconds(..)
  , TS'minutes(..)
  , TS'seconds(..)
  , class TimeSpan
  , dateTime'alter
  , dateTime'day
  , dateTime'day'2dig
  , dateTime'eff'now
  , dateTime'fromMS
  , dateTime'month
  , dateTime'month'2dig
  , dateTime'month'i0
  , dateTime'year
  , ts'add
  , ts'sub
  , ts'toMS
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson) as Dec
import Data.Argonaut.Encode (class EncodeJson) as Enc
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Z.Z.Core (class Numberable, encodeOpts, numberable'toNumber)
import Z.Z.Id (class Identable)

newtype DateTime = DateTime Number

derive instance Newtype DateTime _
derive newtype instance Eq DateTime
derive newtype instance Ord DateTime
derive newtype instance Show DateTime
derive newtype instance Semiring DateTime
derive newtype instance Identable DateTime
derive newtype instance Ring DateTime
derive newtype instance Dec.DecodeJson DateTime
derive newtype instance Enc.EncodeJson DateTime

dateTime'fromMS :: forall num. Numberable num => num -> DateTime
dateTime'fromMS num = wrap $ numberable'toNumber num

dateTime'eff'now :: Effect DateTime
dateTime'eff'now = js_effNow <#> wrap

dateTime'alter :: forall ts. TimeSpan ts => DateTime -> ts -> DateTime
dateTime'alter dt ts = wrap $ (unwrap dt) + (ts'toMS ts)

infixl 5 dateTime'alter as :~:

dateTime'month :: DateTime -> Maybe String -> Int
dateTime'month dt tz = js_dateMonth (unwrap dt) (encodeOpts { timeZone: tz })

dateTime'month'i0 :: DateTime -> Maybe String -> Int
dateTime'month'i0 dt tz = dateTime'month dt tz - 1

dateTime'year :: DateTime -> Maybe String -> Int
dateTime'year dt tz = js_dateYear (unwrap dt) (encodeOpts { timeZone: tz })

dateTime'day :: DateTime -> Maybe String -> Int
dateTime'day dt tz = js_dateDay (unwrap dt) (encodeOpts { timeZone: tz })

dateTime'day'2dig :: DateTime -> Maybe String -> String
dateTime'day'2dig dt tz = mk'2dig (dateTime'day dt tz)

dateTime'month'2dig :: DateTime -> Maybe String -> String
dateTime'month'2dig dt tz = mk'2dig (dateTime'month dt tz)

mk'2dig :: Int -> String
mk'2dig i = (if i < 10 then "0" else "") <> show i

---------------------------------------------------------

class TimeSpan ts where
  ts'toMS :: ts -> Number

instance TimeSpan DateTime where
  ts'toMS = ts'helper 1.0

type TS'op2'fn ts1 ts2 = ts1 -> ts2 -> TS'milliseconds Number
type TS'op2 = forall ts1 ts2. TimeSpan ts1 => TimeSpan ts2 => TS'op2'fn ts1 ts2

ts'add :: TS'op2
ts'add ts1 ts2 = wrap $ (ts'toMS ts1) + (ts'toMS ts2)

ts'sub :: TS'op2
ts'sub ts1 ts2 = wrap $ (ts'toMS ts1) - (ts'toMS ts2)

infixl 6 ts'add as :+:

infixl 6 ts'sub as :-:

ts'helper
  :: forall num nt. Numberable num => Newtype nt num => Number -> nt -> Number
ts'helper mult nt = mult * (numberable'toNumber $ unwrap nt)

newtype TS'milliseconds num = TS'milliseconds num

derive instance Newtype (TS'milliseconds num) _
instance (Numberable num) => TimeSpan (TS'milliseconds num) where
  ts'toMS = ts'helper 1.0

newtype TS'seconds num = TS'seconds num

derive instance Newtype (TS'seconds num) _
instance (Numberable num) => TimeSpan (TS'seconds num) where
  ts'toMS = ts'helper 1000.0

newtype TS'minutes num = TS'minutes num

derive instance Newtype (TS'minutes num) _
instance (Numberable num) => TimeSpan (TS'minutes num) where
  ts'toMS = ts'helper (1000.0 * 60.0)

newtype TS'hours num = TS'hours num

derive instance Newtype (TS'hours num) _
instance (Numberable num) => TimeSpan (TS'hours num) where
  ts'toMS = ts'helper (1000.0 * 60.0 * 60.0)

---------------------------------------------------------

foreign import js_effNow :: Effect Number
foreign import js_dateMonth :: Number -> Json -> Int
foreign import js_dateYear :: Number -> Json -> Int
foreign import js_dateDay :: Number -> Json -> Int