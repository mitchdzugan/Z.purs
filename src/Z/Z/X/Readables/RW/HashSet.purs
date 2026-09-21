module Z.Z.X.Readables.RW.HashSet
  ( B'HashSet
  , R'HashSet
  , X'HashSet
  , X'HashSet'w
  , x'hashset
  , x'hashset'w
  , x'hashset_
  , x'hashset_'w
  ) where

import Z.Z.X.UtilPrelude

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
import Z.Z.HashSet (HashSet)
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

newtype R'HashSet a = R'HashSet (Eff'Bin a /\ HashSet a)

instance
  Identable a =>
  X'Readable'rw (R'HashSet a) (HashSet a) where
  x'readable'rw'mk hs'initial = do
    st <- eff'bin'new
    eff'hs'set hs'initial st
    pure $ R'HashSet $ st /\ hs'initial

instance
  Identable a =>
  X'Results'R'rw (R'HashSet a) (HashSet a) where
  x'results'r'rw'impl (R'HashSet (st /\ _)) = eff'hs'freeze st

instance
  Identable a =>
  X'R'RespondsTo'rw (R'HashSet a)
    ( VariantF
        ( extract :: Responds'Const (HashSet a)
        , size :: Responds'Const Int
        , vals :: Responds'Const (Array a)
        , lookup :: Responds a (Maybe a)
        )
    )
    ( VariantF
        ( assign :: Responds (HashSet a) Unit
        , reset :: Responds'Const Unit
        , clear :: Responds'Const Unit
        , add :: Responds (HashSet a) Unit
        , cons :: Responds a Unit
        , remove :: Responds a Unit
        )
    )
    ( self :: (HashSet a)
    , element :: a
    , key :: a
    ) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r (R'HashSet (st /\ _)) = match
    { extract: responds'const'eff $ eff'hs'freeze st
    , size: responds'const'eff $ eff'bin'size st
    , vals: responds'const'eff $ eff'bin'vals st
    , lookup: responds'run'eff \a -> eff'bin'lookup a st
    }
  x'r'mkResponds'w (R'HashSet (st /\ init)) = match
    { assign: responds'run'eff \hs -> eff'hs'set hs st
    , reset: responds'const'eff $ eff'hs'set init st
    , clear: responds'const'eff $ eff'bin'clear st
    , add: responds'run'eff \hs -> eff'bin'add (unwrap hs) st
    , cons: responds'run'eff \a -> eff'bin'insert a a st
    , remove: responds'run'eff \a -> eff'bin'delete a st
    }

---------------------------------------------------------------------

type X'HashSet a = Reader (RW'Tagged (R'HashSet a))
type X'HashSet'w a = Reader (W'Tagged (R'HashSet a))

x'hashset :: forall @a. Identable a => HashSet a -> X'Evaluable (X'HashSet a)
x'hashset t = x'evaluable t

x'hashset'w
  :: forall @a. Identable a => HashSet a -> X'Evaluable (X'HashSet'w a)
x'hashset'w t = x'evaluable t

x'hashset_ :: forall @a. Identable a => X'Evaluable (X'HashSet a)
x'hashset_ = x'evaluable_

x'hashset_'w :: forall @a. Identable a => X'Evaluable (X'HashSet'w a)
x'hashset_'w = x'evaluable_

---------------------------------------------------------------------

eff'hs'set :: forall a. HashSet a -> Eff'Bin a -> Effect Unit
eff'hs'set hs st = eff'bin'clear st *> eff'bin'add (unwrap hs) st

eff'hs'freeze :: forall a. Eff'Bin a -> Effect (HashSet a)
eff'hs'freeze st = wrap <$> eff'bin'freeze st

type B'HashSet :: forall k. ((Type -> Type) -> Type -> k) -> Type -> k
type B'HashSet sel a = sel (X'HashSet a) (HashSet a)