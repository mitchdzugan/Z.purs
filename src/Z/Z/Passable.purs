module Z.Z.Passable where

import Prelude

import Z.Z.Defaultable as Z

pass :: forall m d. Applicative m => Z.WithDefaultable d (m d)
pass = pure $ Z.default @d