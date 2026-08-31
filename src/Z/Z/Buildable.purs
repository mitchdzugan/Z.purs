module Z.Z.Buildable
  ( B'Const'Def
  , B'Const'Op(..)
  , B'ConstVia'Def
  , B'ConstVia'Op(..)
  , B'Def'Built
  , B'Def'ListOp
  , B'HMap'Tag
  , B'HashMap'Def
  , B'HashMap'Op(..)
  , B'HashSet'Def
  , B'HashSet'Op(..)
  , B'Map'Def
  , B'Map'Op(..)
  , B'Set'Def
  , B'Set'Op(..)
  , b'build
  , b'finish
  , b'hmapBuild
  , b'update
  , class B'Builds
  , class B'Builds'ForeignSt
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.List (List(..))
import Data.Map (Map)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object, runST, values)
import Foreign.Object.ST (STObject, delete, new, poke)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Z.Z.Core (Set, map'fromFoldable, set'fromFoldable)
import Z.Z.Defaultable (class Generable, GDefault, default, g)
import Z.Z.HashMap (HashMap, hm'fromFoldable)
import Z.Z.HashSet (HashSet, hs'fromFoldable)
import Z.Z.Id (class Identable, ident'key)
import Z.Z.X (Edit, edit)

data B'HMap'Tag = B'HMap'Tag

type T'b'hmapBuild'h tIn tOut = Generable tIn GDefault tIn => Edit tIn -> tOut

b'hmapBuild
  ∷ forall tIn tOut. HMap B'HMap'Tag tIn tOut => T'b'hmapBuild'h tIn tOut
b'hmapBuild = hmap B'HMap'Tag <<< edit (default @tIn)

class B'Builds f g | f -> g where
  b'build :: List f -> g

class B'Builds'ForeignSt f i g | f -> i g where
  b'update :: forall r. f -> STObject r i -> ST r (STObject r i)
  b'finish :: Object i -> g

instance (B'Builds f g) => Mapping B'HMap'Tag (List f) (g) where
  mapping _ = b'build

buildForeignStUpdate
  :: forall r f i g
   . B'Builds'ForeignSt f i g
  => List f
  -> STObject r i
  -> ST r (STObject r i)
buildForeignStUpdate Nil st = pure st
buildForeignStUpdate (Cons next rest) st = b'update next st >>=
  buildForeignStUpdate rest

buildForeignStRun :: forall @f @i @g. B'Builds'ForeignSt f i g => List f -> g
buildForeignStRun l = b'finish @f @i $ runST (new >>= buildForeignStUpdate l)

type B'Def'ListOp :: forall k. Type -> k -> Type
type B'Def'ListOp a b = List a

type B'Def'Built :: forall k1 k2. k1 -> k2 -> k2
type B'Def'Built a b = b

------------------------------------------------------------------------------

data B'Const'Op t = B'Const'reset | B'Const'is t

type B'Const'Def :: forall k1. (Type -> Type -> k1) -> Type -> k1
type B'Const'Def sel t = sel (B'Const'Op t) t

instance (Generable t GDefault t) => B'Builds (B'Const'Op t) t where
  b'build (Cons (B'Const'is finalVal) _) = finalVal
  b'build _ = default

------------------------------------------------------------------------------

data B'ConstVia'Op :: Type -> Type -> Type
data B'ConstVia'Op tag t = B'ConstVia'reset | B'ConstVia'is t

type B'ConstVia'Def :: forall k. (Type -> Type -> k) -> Type -> Type -> k
type B'ConstVia'Def sel tag t = sel (B'ConstVia'Op tag t) t

instance (Generable tag GDefault t) => B'Builds (B'ConstVia'Op tag t) t where
  b'build (Cons (B'ConstVia'is finalVal) _) = finalVal
  b'build _ = g @tag

------------------------------------------------------------------------------

data B'Set'Op a = B'Set'reset | B'Set'add a | B'Set'rm a

type B'Set'Def :: forall k1. (Type -> Type -> k1) -> Type -> k1
type B'Set'Def sel a = sel (B'Set'Op a) (Set a)

instance (Identable a, Ord a) => B'Builds'ForeignSt (B'Set'Op a) a (Set a) where
  b'update (B'Set'rm a) st = delete (ident'key a) st
  b'update (B'Set'add a) st = poke (ident'key a) a st
  b'update B'Set'reset _ = new
  b'finish o = set'fromFoldable $ values o

instance (Identable a, Ord a) => B'Builds (B'Set'Op a) (Set a) where
  b'build = buildForeignStRun @(B'Set'Op a) @a @(Set a)

------------------------------------------------------------------------------

data B'Map'Op k v = B'Map'reset | B'Map'set k v | B'Map'rm k

type B'Map'Def :: forall k1. (Type -> Type -> k1) -> Type -> Type -> k1
type B'Map'Def sel k v = sel (B'Map'Op k v) (Map k v)

instance
  ( Identable k
  , Ord k
  ) =>
  B'Builds'ForeignSt (B'Map'Op k v) (k /\ v) (Map k v) where
  b'update (B'Map'rm k) st = delete (ident'key k) st
  b'update (B'Map'set k v) st = poke (ident'key k) (k /\ v) st
  b'update (B'Map'reset) _ = new
  b'finish o = map'fromFoldable $ values o

instance (Identable k, Ord k) => B'Builds (B'Map'Op k v) (Map k v) where
  b'build = buildForeignStRun @(B'Map'Op k v) @(k /\ v) @(Map k v)

------------------------------------------------------------------------------

data B'HashMap'Op k v = B'HashMap'reset | B'HashMap'set k v | B'HashMap'rm k

type B'HashMap'Def :: forall k1. (Type -> Type -> k1) -> Type -> Type -> k1
type B'HashMap'Def sel k v = sel (B'HashMap'Op k v) (HashMap k v)

instance
  ( Identable k
  ) =>
  B'Builds'ForeignSt (B'HashMap'Op k v) (k /\ v) (HashMap k v) where
  b'update (B'HashMap'rm k) st = delete (ident'key k) st
  b'update (B'HashMap'set k v) st = poke (ident'key k) (k /\ v) st
  b'update (B'HashMap'reset) _ = new
  b'finish o = hm'fromFoldable $ values o

instance (Identable k) => B'Builds (B'HashMap'Op k v) (HashMap k v) where
  b'build = buildForeignStRun @(B'HashMap'Op k v) @(k /\ v) @(HashMap k v)

------------------------------------------------------------------------------

data B'HashSet'Op a = B'HashSet'reset | B'HashSet'add a | B'HashSet'rm a

type B'HashSet'Def :: forall k1. (Type -> Type -> k1) -> Type -> k1
type B'HashSet'Def sel a = sel (B'HashSet'Op a) (HashSet a)

instance (Identable a) => B'Builds'ForeignSt (B'HashSet'Op a) a (HashSet a) where
  b'update (B'HashSet'rm a) st = delete (ident'key a) st
  b'update (B'HashSet'add a) st = poke (ident'key a) a st
  b'update B'HashSet'reset _ = new
  b'finish o = hs'fromFoldable $ values o

instance (Identable a) => B'Builds (B'HashSet'Op a) (HashSet a) where
  b'build = buildForeignStRun @(B'HashSet'Op a) @a @(HashSet a)
