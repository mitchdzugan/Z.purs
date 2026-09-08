module Z.Z.X6.Ref
  ( R'Ref
  , X'Ref
  , X'Ref'w
  , x'ref
  , x'ref'w
  ) where

import Z.Z.X6.UtilPrelude

import Z.Z.Eff.Ref (Eff'Ref, eff'ref'get, eff'ref'new, eff'ref'set)
import Z.Z.X6.Core
  ( class X'R'RespondsTo'rw
  , class X'Readable'rw
  , RW'Tagged
  , W'Tagged
  , X'Runnable
  , x'runnable
  )
import Z.Z.X6.Responds
  ( Responds
  , Responds'Const
  , responds'const'eff
  , responds'run'eff
  )

newtype R'Ref t = R'Ref (Eff'Ref t /\ t)

instance X'Readable'rw (R'Ref t) t where
  x'readable'rw'mk param = do
    st <- eff'ref'new param
    pure $ R'Ref $ st /\ param

type R'Ref'RespondsTo'R r = VariantF
  ( get :: Responds'Const r
  , result :: Responds'Const r
  )

type R'Ref'RespondsTo'W r = VariantF
  ( set :: Responds r Unit
  , reset :: Responds'Const Unit
  )

instance
  X'R'RespondsTo'rw (R'Ref t)
    (R'Ref'RespondsTo'R t)
    (R'Ref'RespondsTo'W t)
    (self :: t) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r (R'Ref (ref /\ _)) = match
    { get: responds'const'eff $ eff'ref'get ref
    , result: responds'const'eff $ eff'ref'get ref
    }
  x'r'mkResponds'w (R'Ref (ref /\ init)) = match
    { set: responds'run'eff \v -> eff'ref'set v ref
    , reset: responds'const'eff $ eff'ref'set init ref
    }

type X'Ref t = Reader (RW'Tagged (R'Ref t))
type X'Ref'w t = Reader (W'Tagged (R'Ref t))

x'ref :: forall t. t -> X'Runnable (X'Ref t)
x'ref t = x'runnable t

x'ref'w :: forall t. t -> X'Runnable (X'Ref'w t)
x'ref'w t = x'runnable t
