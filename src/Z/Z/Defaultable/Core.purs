module Z.Z.Defaultable.Core
  ( class Defaultable
  , default
  , orDefault
  , whenJust
  ) where

import Prelude

import Control.Monad as Monad
import Data.Maybe as May

class Defaultable a where
  default :: a

instance Defaultable String where
  default = ""

instance Defaultable Unit where
  default = unit

instance Defaultable (Array a) where
  default = []

instance Defaultable (May.Maybe a) where
  default = May.Nothing

orDefault :: forall d. Defaultable d => May.Maybe d -> d
orDefault m = May.fromMaybe default m

whenJust
  :: forall m d a
   . Monad.Monad m
  => Defaultable d
  => May.Maybe a
  -> (a -> m d)
  -> m d
whenJust m f = May.maybe (pure default) f m
