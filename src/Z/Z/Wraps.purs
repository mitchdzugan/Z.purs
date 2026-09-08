module Z.Z.Wraps
  ( class Unwraps
  , class Wraps
  , class Wraps'Old
  , nu'
  , nu'mk
  , un'
  , wrapped'from
  , wrapped'get
  , wrapped'mk
  , wrapped'mkFor
  , wrapped'rest
  ) where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Newtype as NT
import Data.Tuple (Tuple(..))
import Z.Z.Defaultable (class Generable, GDefault, g)
import Z.Z.Id (class IsId, Idented, idented'id, idented'mk, idented'v)

class Wraps'Old o completer i | o -> completer i where
  un' :: o -> i
  nu'mk :: completer -> i -> o

instance (NT.Newtype o i) => Wraps'Old o Unit i where
  un' = NT.unwrap
  nu'mk _ = NT.wrap

nu' :: forall o i. Wraps'Old o Unit i => i -> o
nu' = nu'mk @o unit

class Wraps o i | o -> i where
  wrapped'get :: o -> i

class Wraps o i <= Unwraps o rest i | o -> rest i where
  wrapped'mk :: i -> rest -> o
  wrapped'rest :: o -> rest

wrapped'from
  :: forall @o rest i
   . Generable rest GDefault rest
  => Unwraps o rest i
  => i
  -> o
wrapped'from i = wrapped'mk @o i $ g @rest

wrapped'mkFor :: forall @o rest i. Unwraps o rest i => rest -> i -> o
wrapped'mkFor = flip $ wrapped'mk @o

instance Wraps (Tuple l r) l where
  wrapped'get (Tuple l _) = l

instance Unwraps (Tuple l r) r l where
  wrapped'mk l r = (Tuple l r)
  wrapped'rest (Tuple _ r) = r

instance IsId id => Wraps (Idented id t) t where
  wrapped'get = idented'v

instance IsId id => Unwraps (Idented id t) id t where
  wrapped'mk = flip idented'mk
  wrapped'rest = idented'id
