module Z.Z.Defaultable.Util
  ( WithDefaultable
  , default
  , orDefault
  , whenJust
  , z
  ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe, maybe)
import Z.Z.Defaultable.Generable
  ( class GTaggedDefaultable
  , class GenerableC
  , GDefault
  , gTaggedDefault
  , mkGenerable
  )

z :: forall @tag a. GTaggedDefaultable tag a => a
z = gTaggedDefault @tag

type WithDefaultable d t = GenerableC d GDefault d => t

orDefault :: forall d. WithDefaultable d (Maybe d -> d)
orDefault m = fromMaybe (default @d) m

whenJust
  :: forall m d a
   . Monad m
  => WithDefaultable d (Maybe a -> (a -> m d) -> m d)
whenJust m f = maybe (pure $ default @d) f m

default :: forall @d. WithDefaultable d d
default = mkGenerable @d @GDefault