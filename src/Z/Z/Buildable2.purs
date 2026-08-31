module Z.Z.Buildable2 where

import Prelude

import Control.Monad.ST as ST
import Data.Exists (Exists, mkExists, runExists)
import Data.Functor.Variant (VariantF)
import Data.List (List(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Data.Tuple.Nested (type (/\))
import Data.Variant (class VariantMatchCases, Variant)
import Foreign.Object (Object, freezeST, runST)
import Foreign.Object.ST (STObject)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Unsafe.Coerce (unsafeCoerce)
import Z.Z.Core
  ( obj'lookup
  , objST'new
  , objST'peek
  , objST'poke
  , var'inj
  , var'match
  )
import Z.Z.Defaultable (class Generable, GDefault, g)

{-
  , B'Const'Def
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
-}

class ST'tag tag inner | tag -> inner

newtype STObjectOfF i r = STObjectOfF (ST.ST r (STObject r i))
type STObjectOf i = Exists (STObjectOfF i)

mkSTObject :: forall i r. STObject r i -> STObjectOf i
mkSTObject o = mkExists $ STObjectOfF $ pure o

handle :: forall i r. STObjectOfF i r -> Object i
handle (STObjectOfF stObject) = ST.run (unsafeCoerce (stObject >>= freezeST))

{-
newtype ST'V'F a r = ST'V'F
  { ref :: a r
  , work :: a r -> ST.ST r (a r)
  , complete :: forall x. (a r -> ST.ST r x) -> x
  }

type ST'V a = Exists (ST'V'F a)

st'v'extend ::forall a r. ST'V a -> (a r -> ST.ST r (a r)) ->
-}

-- useSTObject :: forall i. STObjectOf i -> Object i
-- useSTObject o = runExists handle o

{-

newtype STObjectOf i r = STObjectOf (STObject r i)

b'objST'finish
  :: forall r i output
   . (STObject r i -> ST r output)
  -> STObjectOf i r
  -> ST r output
b'objST'finish f (STObjectOf st) = f st

b'objST'updateMatch
  :: forall stR r rl rvI rvO i
   . RowToList r rl
  => VariantMatchCases rl rvI (ST stR (STObject stR i))
  => Union rvI () rvO
  => (STObject stR i -> Record r)
  -> Variant rvO
  -> STObjectOf i stR
  -> ST stR (STObjectOf i stR)
b'objST'updateMatch mkCase var (STObjectOf st) =
  STObjectOf <$> flip var'match var (mkCase st)

class B'Builds
  :: forall k
   . k
  -> Type
  -> Row (Type -> Type)
  -> (Region -> Type)
  -> Type
  -> Constraint
class B'Builds tag cons v'act i out | tag -> cons v'act i out where
  b'init :: forall r. cons -> ST r (i r)
  b'update :: forall r x. VariantF v'act x -> i r -> ST r (i r /\ x)
  b'finish :: forall r. i r -> ST r out

foreign import data B'Const :: Type -> Type

type B'Vars'Const t = B'Const'def B'def'param t

type B'Const'def :: forall k. (Type -> Type -> k) -> Type -> Row k
type B'Const'def b'sel t = b'sel
  ||- B'is'def t
  -|- B'reset'def

type B'is'def :: forall k1 k2. k1 -> (k1 -> Type -> k2) -> Row k2 -> Row k2
type B'is'def t b'sel r = (is :: b'sel t Unit | r)

type B'reset'def :: forall k. (Type -> Type -> k) -> Row k -> Row k
type B'reset'def b'sel r = (reset :: b'sel Unit Unit | r)

type B'def'plus
  :: forall k1 k2 k3 k4. (k1 -> k2 -> k3) -> (k1 -> k4 -> k2) -> k1 -> k4 -> k3
type B'def'plus t1 t2 b'sel r = t1 b'sel (t2 b'sel r)

type B'def'mk :: forall k1 k2 k3. k1 -> (k1 -> Row k2 -> k3) -> k3
type B'def'mk b'sel tf = tf b'sel ()

infixr 0 type B'def'plus as -|-
infixr 0 type B'def'mk as ||-

type B'def'param :: forall k1 k2. k1 -> k2 -> k1
type B'def'param param _res = param

instance
  ( Generable tag GDefault t
  ) =>
  B'Builds (B'Const tag) Unit (B'Const'def B'def'param t) (STObjectOf t) t where
  b'init _ = STObjectOf <$> objST'new
  b'finish = b'objST'finish \st -> fromMaybe (g @tag) <$> objST'peek "" st
  b'update = b'objST'updateMatch \st ->
    { is: flip (objST'poke "") st
    , reset: const objST'new
    }

b'Reset :: forall b @a. B'ActionVariant a (reset :: Unit | b) => a
b'Reset = b'actionVariant'promote $ var'inj @"reset" unit

b'Is :: forall t b @a. Newtype a (Variant (is :: t | b)) => t -> a
b'Is t = wrap $ var'inj @"is" t

class B'ActionVariant a v | a -> v where
  b'actionVariant'promote :: Variant v -> a

newtype B'def'var param res x = B'def'var (param /\ res -> x)

derive instance Newtype (B'def'var param res x) _

newtype B'X'Const t x = B'X'Const (VariantF (B'Const'def B'def'var t) x)

derive instance Newtype (B'X'Const t x) _
derive instance Functor (B'X'Const t)

-}