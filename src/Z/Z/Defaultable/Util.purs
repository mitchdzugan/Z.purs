module Z.Z.Defaultable.Util
  ( WithDefaultable
  , default
  , orDefault
  , whenJust
  , whenTrue
  ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe, maybe)
import Z.Z.Defaultable.Generable
  ( class Generable
  , GDefault
  , mkGenerable
  )

type WithDefaultable d t = Generable d GDefault d => t

orDefault :: forall d. WithDefaultable d (Maybe d -> d)
orDefault m = fromMaybe (default @d) m

whenJust
  :: forall m d a
   . Monad m
  => WithDefaultable d (Maybe a -> (a -> m d) -> m d)
whenJust m f = maybe (pure $ default @d) f m

whenTrue
  :: forall m d
   . Monad m
  => WithDefaultable d (Boolean -> m d -> m d)
whenTrue true m = m
whenTrue _ _ = pure $ default @d

default :: forall @d. WithDefaultable d d
default = mkGenerable @d @GDefault