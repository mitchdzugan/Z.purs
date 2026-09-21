module Z.Z.X6.Readables.Base
  ( LogLevel(..)
  , R'X'Base
  , X'BaseM
  , x'base
  , x'now''
  , x'out''
  ) where

import Z.Z.X6.UtilPrelude

import Effect.Now (nowDateTime)
import Unsafe.Coerce (unsafeCoerce)
import Z.Z.DateTime (DateTime, fromRawDateTime)
import Z.Z.X6.Core
  ( class X'R'RespondsTo
  , class X'Readable
  , X'Evaluable
  , x'evaluable_
  , x'respondTo
  , x'respondTo_
  )
import Z.Z.X6.Responds
  ( Responds
  , Responds'Const
  , responds'const'eff
  , responds'run'eff
  )

data LogLevel = LogLevel'Info | LogLevel'Warning | LogLevel'Error

foreign import data Loggable :: Type

loggable :: forall a. a -> Loggable
loggable = unsafeCoerce

newtype R'X'Base = R'X'Base {}

instance X'Readable R'X'Base Unit where
  x'readable'mk _ = pure $ R'X'Base {}

type R'X'Base'RespondsTo'R = VariantF
  ( now :: Responds'Const DateTime
  , out :: Responds (LogLevel /\ Loggable) Unit
  )

instance
  X'R'RespondsTo R'X'Base R'X'Base'RespondsTo'R () where
  x'r'mkResponds'types = Proxy
  x'r'mkResponds (R'X'Base _) = match
    { now: responds'const'eff $ fromRawDateTime <$> nowDateTime
    , out: responds'run'eff \_ -> pure unit
    }

type X'BaseM = Reader R'X'Base

x'base :: X'Evaluable X'BaseM
x'base = x'evaluable_ @X'BaseM

x'now'' :: forall @p x' x. ConsSymbol p X'BaseM x' x => Run x DateTime
x'now'' = x'respondTo_ @p @"now"

x'out''
  :: forall @p x' x a. ConsSymbol p X'BaseM x' x => LogLevel -> a -> Run x Unit
x'out'' ll = x'respondTo @p @"out" <<< (/\) ll <<< loggable