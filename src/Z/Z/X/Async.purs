module Z.Z.X.Async
  ( AsyncF(..)
  , x'async''
  ) where

import Prelude

import Type.Proxy (Proxy(..))
import Z.EXTERNAL.RUN.Run (Run, lift)
import Z.Z.Core (class ConsSymbol)
import Z.Z.Promise (Future)

data AsyncF a = AsyncSend (Future a)

derive instance Functor AsyncF

x'async''
  :: forall @p f x' x. ConsSymbol p AsyncF x' x => (Future f) -> Run x f
x'async'' future = lift (Proxy @p) (AsyncSend future)

