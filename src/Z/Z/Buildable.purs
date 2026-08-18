module Z.Z.Buildable
  ( BuildableMapper(..)
  , ConstOp(..)
  , HashSetOp(..)
  , build
  , class BuildsTo
  , js_mk
  ) where

import Prelude

import Data.List (List(..))
import Heterogeneous.Mapping (class Mapping)
import Z.Z.Defaultable (class Generable, GDefault, default)

data BuildableMapper = BuildableMapper

class BuildsTo f g | g -> g where
  build :: List f -> g

instance (BuildsTo f g) => Mapping BuildableMapper (List f) (g) where
  mapping _ = build

data ConstOp t = Const'Set t

instance (Generable t GDefault t) => BuildsTo (ConstOp t) t where
  build (Cons (Const'Set finalVal) _) = finalVal
  build _ = js_mk $ default

data HashSetOp a = HashSet'add a | HashSet'rm a

foreign import js_mk :: forall a. a -> a