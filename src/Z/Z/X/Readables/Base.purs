module Z.Z.X.Readables.Base
  ( LogLevel(..)
  , Loggable
  , R'X'Base(..)
  , X'BaseM
  , x_base
  , x'out''
  ) where

import Z.Z.X.UtilPrelude

import Unsafe.Coerce (unsafeCoerce)
import Z.Z.X.Core (class X'Readable, Run, X'Evaluable, x_evaluable_)
import Z.Z.X.Method (x_effCall)

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
  { out :: LogLevel /\ Loggable -> Effect Unit
  }

derive instance Newtype R'X'Base _

instance X'Readable R'X'Base Unit where
  x'readable'mk _ = pure $ wrap
    { out: \(ll /\ v) -> js_consoleOut unit (logLevel'consoleKey ll) v
    }

type X'BaseM = R'X'Base

x_base :: X'Evaluable X'BaseM
x_base = x_evaluable_ @X'BaseM

x'out''
  :: forall @p x' x a. ConsSymbol p X'BaseM x' x => LogLevel -> a -> Run x Unit
x'out'' ll = x_effCall @p @"out" <<< (/\) ll <<< loggable