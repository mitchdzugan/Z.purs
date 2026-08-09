module Z.Z.Defaultable.Util
  ( z
  ) where

import Z.Z.Defaultable.Generable (class GTaggedDefaultable, gTaggedDefault)

z :: forall @tag a. GTaggedDefaultable tag a => a
z = gTaggedDefault @tag
