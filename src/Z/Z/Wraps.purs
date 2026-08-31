module Z.Z.Wraps
  ( un'
  , class Wraps
  ) where

import Data.Newtype as NT
import Z.Z.Id (Idented, idented'v)

class Wraps o i | o -> i where
  un' :: o -> i

instance Wraps (Idented id t) t where
  un' = idented'v
else instance (NT.Newtype o i) => Wraps o i where
  un' = NT.unwrap