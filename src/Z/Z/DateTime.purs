module Z.Z.DateTime
  ( (:+:)
  , (:~:)
  , DateTime(..)
  , TS_hours(..)
  , TS_milliseconds(..)
  , TS_minutes(..)
  , TS_seconds(..)
  , class TimeSpan
  , dateTime_alter
  , dateTime_day
  , dateTime_day'2dig
  , dateTime_eff'now
  , dateTime_fromMS
  , dateTime_mkDate
  , dateTime_month
  , dateTime_month'2dig
  , dateTime_month'i0
  , dateTime_year
  , ts_add
  , ts_sub
  , ts_toMS
  ) where

import Prelude

import Data.Argonaut.Core (Json)
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
derive newtype instance Enc.EncodeJson DateTime

dateTime_fromMS :: forall num. Numberable num => num -> DateTime
dateTime_fromMS num = wrap $ numberable'toNumber num

dateTime_mkDate :: Int -> Int -> Int -> Maybe String -> DateTime
dateTime_mkDate year month day timeZone = wrap $ js_makeDate year month day $
  encodeOpts { timeZone }

dateTime_eff'now :: Effect DateTime
dateTime_eff'now = js_effNow <#> wrap

dateTime_alter :: forall ts. TimeSpan ts => DateTime -> ts -> DateTime
dateTime_alter dt ts = wrap $ (unwrap dt) + (ts_toMS ts)

infixl 5 dateTime_alter as :~:

dateTime_month :: DateTime -> Maybe String -> Int
dateTime_month dt tz = js_dateMonth (unwrap dt) (encodeOpts { timeZone: tz })

dateTime_month'i0 :: DateTime -> Maybe String -> Int
dateTime_month'i0 dt tz = dateTime_month dt tz - 1

dateTime_year :: DateTime -> Maybe String -> Int
dateTime_year dt tz = js_dateYear (unwrap dt) (encodeOpts { timeZone: tz })

dateTime_day :: DateTime -> Maybe String -> Int
dateTime_day dt tz = js_dateDay (unwrap dt) (encodeOpts { timeZone: tz })

dateTime_day'2dig :: DateTime -> Maybe String -> String
dateTime_day'2dig dt tz = mk'2dig (dateTime_day dt tz)

dateTime_month'2dig :: DateTime -> Maybe String -> String
dateTime_month'2dig dt tz = mk'2dig (dateTime_month dt tz)

mk'2dig :: Int -> String
mk'2dig i = (if i < 10 then "0" else "") <> show i

---------------------------------------------------------

class TimeSpan ts where
  ts_toMS :: ts -> Number

instance TimeSpan DateTime where
  ts_toMS = ts'helper 1.0

type TS_op2'fn ts1 ts2 = ts1 -> ts2 -> TS_milliseconds Number
type TS_op2 = forall ts1 ts2. TimeSpan ts1 => TimeSpan ts2 => TS_op2'fn ts1 ts2

ts_add :: TS_op2
ts_add ts1 ts2 = wrap $ (ts_toMS ts1) + (ts_toMS ts2)

ts_sub :: TS_op2
ts_sub ts1 ts2 = wrap $ (ts_toMS ts1) - (ts_toMS ts2)

infixl 6 ts_add as :+:

infixl 6 ts_sub as :-:

ts'helper
  :: forall num nt. Numberable num => Newtype nt num => Number -> nt -> Number
ts'helper mult nt = mult * (numberable'toNumber $ unwrap nt)

newtype TS_milliseconds num = TS_milliseconds num

derive instance Newtype (TS_milliseconds num) _
instance (Numberable num) => TimeSpan (TS_milliseconds num) where
  ts_toMS = ts'helper 1.0

newtype TS_seconds num = TS_seconds num

derive instance Newtype (TS_seconds num) _
instance (Numberable num) => TimeSpan (TS_seconds num) where
  ts_toMS = ts'helper 1000.0

newtype TS_minutes num = TS_minutes num

derive instance Newtype (TS_minutes num) _
instance (Numberable num) => TimeSpan (TS_minutes num) where
  ts_toMS = ts'helper (1000.0 * 60.0)

newtype TS_hours num = TS_hours num

derive instance Newtype (TS_hours num) _
instance (Numberable num) => TimeSpan (TS_hours num) where
  ts_toMS = ts'helper (1000.0 * 60.0 * 60.0)

---------------------------------------------------------

foreign import js_effNow :: Effect Number
foreign import js_dateMonth :: Number -> Json -> Int
foreign import js_dateYear :: Number -> Json -> Int
foreign import js_dateDay :: Number -> Json -> Int
foreign import js_makeDate :: Int -> Int -> Int -> Json -> Number