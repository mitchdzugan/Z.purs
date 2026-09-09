module Z.Z.X6.Readables.RW.Ref
  ( R'Ref
  , X'Ref
  , X'Ref'w
  , x'ref
  , x'ref'w
  , x'ref_
  , x'ref_'w
  ) where

import Z.Z.X6.UtilPrelude

import Z.Z.Eff.Ref (Eff'Ref, eff'ref'get, eff'ref'new, eff'ref'set)
import Z.Z.X6.Core
  ( class X'R'RespondsTo'rw
  , class X'Readable'rw
  , class X'Results'R'rw
  , RW'Tagged
  , W'Tagged
  , X'Evaluable
  , x'evaluable
  , x'evaluable_
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

instance X'Results'R'rw (R'Ref t) t where
  x'results'r'rw'impl (R'Ref (ref /\ _)) = eff'ref'get ref

instance
  X'R'RespondsTo'rw (R'Ref t)
    ( VariantF
        ( extract :: Responds'Const t
        )
    )
    ( VariantF
        ( assign :: Responds t Unit
        , reset :: Responds'Const Unit
        )
    )
    ( self :: t
    ) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r (R'Ref (ref /\ _)) = match
    { extract: responds'const'eff $ eff'ref'get ref
    }
  x'r'mkResponds'w (R'Ref (ref /\ init)) = match
    { assign: responds'run'eff \v -> eff'ref'set v ref
    , reset: responds'const'eff $ eff'ref'set init ref
    }

type X'Ref t = Reader (RW'Tagged (R'Ref t))
type X'Ref'w t = Reader (W'Tagged (R'Ref t))

x'ref :: forall t. t -> X'Evaluable (X'Ref t)
x'ref t = x'evaluable t

x'ref'w :: forall t. t -> X'Evaluable (X'Ref'w t)
x'ref'w t = x'evaluable t

x'ref_ :: forall t. Generable t GDefault t => X'Evaluable (X'Ref t)
x'ref_ = x'evaluable_

x'ref_'w :: forall t. Generable t GDefault t => X'Evaluable (X'Ref'w t)
x'ref_'w = x'evaluable_
