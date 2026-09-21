module Z.Z.X.Readables.RW.HashMap
  ( B'HashMap
  , R'HashMap
  , X'HashMap
  , X'HashMap'w
  , x'hashmap
  , x'hashmap'w
  , x'hashmap_
  , x'hashmap_'w
  ) where

import Z.Z.X.UtilPrelude

import Z.Z.Defaultable (whenJust)
import Z.Z.Eff.Bin
  ( Eff'Bin
  , eff'bin'add
  , eff'bin'clear
  , eff'bin'delete
  , eff'bin'freeze
  , eff'bin'insert
  , eff'bin'lookup
  , eff'bin'new
  , eff'bin'size
  , eff'bin'vals
  )
import Z.Z.HashMap (HashMap)
import Z.Z.Id (class Identable)
import Z.Z.X.Core
  ( class X'R'RespondsTo'rw
  , class X'Readable'rw
  , class X'Results'R'rw
  , RW'Tagged
  , W'Tagged
  , X'Evaluable
  , x'evaluable
  , x'evaluable_
  )
import Z.Z.X.Responds
  ( Responds
  , Responds'Const
  , responds'const'eff
  , responds'run'eff
  )

type Eff'HashMap k v = Eff'Bin { k :: k, v :: v }

newtype R'HashMap k v = R'HashMap (Eff'HashMap k v /\ HashMap k v)

instance
  Identable k =>
  X'Readable'rw (R'HashMap k v) (HashMap k v) where
  x'readable'rw'mk hm'initial = do
    st <- eff'bin'new
    eff'hm'set hm'initial st
    pure $ R'HashMap $ st /\ hm'initial

instance
  Identable k =>
  X'Results'R'rw (R'HashMap k v) (HashMap k v) where
  x'results'r'rw'impl (R'HashMap (st /\ _)) = eff'hm'freeze st

instance
  Identable k =>
  X'R'RespondsTo'rw (R'HashMap k v)
    ( VariantF
        ( extract :: Responds'Const (HashMap k v)
        , size :: Responds'Const Int
        , vals :: Responds'Const (Array v)
        , keys :: Responds'Const (Array k)
        , entries :: Responds'Const (Array $ k /\ v)
        , lookup :: Responds k (Maybe v)
        )
    )
    ( VariantF
        ( assign :: Responds (HashMap k v) Unit
        , reset :: Responds'Const Unit
        , clear :: Responds'Const Unit
        , add :: Responds (HashMap k v) Unit
        , remove :: Responds k Unit
        , insert :: Responds (k /\ v) Unit
        , replace :: Responds (k /\ v) Unit
        )
    )
    ( self :: (HashMap k v)
    , element :: v
    , key :: k
    ) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r (R'HashMap (st /\ _)) = match
    { extract: responds'const'eff $ eff'hm'freeze st
    , size: responds'const'eff $ eff'bin'size st
    , vals: responds'const'eff $ eff'bin'vals st <#> map _.v
    , keys: responds'const'eff $ eff'bin'vals st <#> map _.k
    , entries: responds'const'eff $ eff'bin'vals st <#> map \e -> e.k /\ e.v
    , lookup: responds'run'eff \k -> eff'bin'lookup k st <#> map _.v
    }
  x'r'mkResponds'w (R'HashMap (st /\ init)) = match
    { assign: responds'run'eff \hm -> eff'hm'set hm st
    , reset: responds'const'eff $ eff'hm'set init st
    , clear: responds'const'eff $ eff'bin'clear st
    , add: responds'run'eff \hs -> eff'bin'add (unwrap hs) st
    , remove: responds'run'eff \k -> eff'bin'delete k st
    , insert: responds'run'eff \(k /\ v) -> eff'bin'insert k { k, v } st
    , replace: responds'run'eff \(k /\ v) -> eff'bin'lookup k st >>= \curr ->
        whenJust curr $ const $ eff'bin'insert k { k, v } st
    }

---------------------------------------------------------------------

type X'HashMap k v = Reader (RW'Tagged (R'HashMap k v))
type X'HashMap'w k v = Reader (W'Tagged (R'HashMap k v))

x'hashmap
  :: forall @k @v. Identable k => HashMap k v -> X'Evaluable (X'HashMap k v)
x'hashmap t = x'evaluable t

x'hashmap'w
  :: forall @k @v. Identable k => HashMap k v -> X'Evaluable (X'HashMap'w k v)
x'hashmap'w t = x'evaluable t

x'hashmap_ :: forall @k @v. Identable k => X'Evaluable (X'HashMap k v)
x'hashmap_ = x'evaluable_

x'hashmap_'w :: forall @k @v. Identable k => X'Evaluable (X'HashMap'w k v)
x'hashmap_'w = x'evaluable_

---------------------------------------------------------------------

eff'hm'set :: forall k v. HashMap k v -> Eff'HashMap k v -> Effect Unit
eff'hm'set hm st = eff'bin'clear st *> do
  eff'bin'add (unwrap hm) st

eff'hm'freeze :: forall k v. Eff'HashMap k v -> Effect (HashMap k v)
eff'hm'freeze st = wrap <$> eff'bin'freeze st

type B'HashMap sel k v = sel (X'HashMap k v) (HashMap k v)