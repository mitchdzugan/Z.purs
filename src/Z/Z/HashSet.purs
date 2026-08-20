module Z.Z.HashSet where

import Prelude

import Z.Z.Bin (Bin, bin'empty, bin'fromFoldable, bin'insert, bin'lookup)
import Z.Z.Key (class Keyed)

type HashSet a = Bin a

hs'empty :: forall a. Keyed a => HashSet a
hs'empty = bin'empty