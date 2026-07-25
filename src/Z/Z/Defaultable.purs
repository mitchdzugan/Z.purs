module Z.Z.Defaultable
  ( auto
  , class Defaultable
  , default
  , default'
  , orDefault
  , whenJust
  ) where

import Prelude
import Control.Monad as Monad
import Data.Maybe as May

class Defaultable a where
  default :: a

instance defaultString :: Defaultable String where
  default = ""

instance defaultUnit :: Defaultable Unit where
  default = unit

else instance defaultArray :: Defaultable (Array a) where
  default = []

else instance defaultJust :: Defaultable (May.Maybe a) where
  default = May.Nothing

else instance defaultApplicable ::
  ( Defaultable v
  , Applicative a
  ) =>
  Defaultable (a v) where
  default = pure default

default' :: forall @d. Defaultable d => d
default' = default

auto :: forall d r. Defaultable d => (d -> r) -> r
auto f = f default

orDefault :: forall d. Defaultable d => May.Maybe d -> d
orDefault = auto <<< flip May.fromMaybe

whenJust
  :: forall m d a
   . Monad.Monad m
  => Defaultable d
  => May.Maybe a
  -> (a -> m d)
  -> m d
whenJust m f = May.maybe (pure default) f m