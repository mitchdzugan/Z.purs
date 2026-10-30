module Z.Z.X.Readables.Base
  ( LogLevel(..)
  , Loggable
  , R'X'Base(..)
  , X'BaseM
  , x'base
  , x'now''
  , x'out''
  ) where

import Z.Z.X.UtilPrelude

import Unsafe.Coerce (unsafeCoerce)
import Z.Z.DateTime (DateTime, dateTime'eff'now)
import Z.Z.X.Core
  ( class X'Readable
  , X'Evaluable
  , x'evaluable_
  )
import Z.Z.X.Method (x'effCall, x'effGet)

data LogLevel = LogLevel'Info | LogLevel'Warning | LogLevel'Error

logLevel'consoleKey :: LogLevel -> String
logLevel'consoleKey LogLevel'Info = "log"
logLevel'consoleKey LogLevel'Warning = "warn"
logLevel'consoleKey LogLevel'Error = "error"

foreign import js_consoleOut :: Unit -> String -> Loggable -> Effect Unit

foreign import data Loggable :: Type

loggable :: forall a. a -> Loggable
loggable = unsafeCoerce

newtype R'X'Base = R'X'Base
  { now :: Effect DateTime
  , out :: LogLevel /\ Loggable -> Effect Unit
  }

derive instance Newtype R'X'Base _

instance X'Readable R'X'Base Unit where
  x'readable'mk _ = pure $ R'X'Base
    { now: dateTime'eff'now
    , out: \(ll /\ v) -> js_consoleOut unit (logLevel'consoleKey ll) v
    }

type X'BaseM = Reader R'X'Base

x'base :: X'Evaluable X'BaseM
x'base = x'evaluable_ @X'BaseM

x'now'' :: forall @p x' x. ConsSymbol p X'BaseM x' x => Run x DateTime
x'now'' = x'effGet @p @"now"

x'out''
  :: forall @p x' x a. ConsSymbol p X'BaseM x' x => LogLevel -> a -> Run x Unit
x'out'' ll = x'effCall @p @"out" <<< (/\) ll <<< loggable