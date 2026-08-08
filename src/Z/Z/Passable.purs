module Z.Z.Passable where

import Prelude
import Z.Z.Defaultable as Z

class Passable m where
  pass :: m

instance
  ( Applicative m
  , Z.Defaultable d
  ) =>
  Passable (m d) where
  pass = pure Z.default

