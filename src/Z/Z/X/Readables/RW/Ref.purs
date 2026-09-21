module Z.Z.X.Readables.RW.Ref
  ( B'Ref
  , B'Ref'nt
  , R'Ref
  , R'Ref'nt
  , X'Ref
  , X'Ref'nt
  , X'Ref'nt'w
  , X'Ref'w
  , x'ref
  , x'ref'nt
  , x'ref'nt'w
  , x'ref'w
  , x'ref_
  , x'ref_'w
  ) where

import Z.Z.X.UtilPrelude

import Z.Z.Eff.Ref (Eff'Ref, eff'ref'get, eff'ref'new, eff'ref'set)
import Z.Z.X.Core
  ( class X'R'RespondsTo'rw
  , class X'Readable'rw
  , class X'Results'R'rw
  , RW'Tagged
  , W'Tagged
  , X'Evaluable
  , x'evaluable
  )
import Z.Z.X.Responds
  ( Responds
  , Responds'Const
  , responds'const'eff
  , responds'run'eff
  )

---------------------------------------------------------------------

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
x'ref_ = x'evaluable @(X'Ref t) $ g @t

x'ref_'w :: forall t. Generable t GDefault t => X'Evaluable (X'Ref'w t)
x'ref_'w = x'evaluable @(X'Ref'w t) $ g @t

---------------------------------------------------------------------

newtype R'Ref'nt nt = R'Ref'nt (Eff'Ref nt /\ nt)

instance
  ( Generable nt GDefault nt
  , Newtype nt t
  ) =>
  X'Readable'rw (R'Ref'nt nt) Unit where
  x'readable'rw'mk _ = do
    let default = g @nt
    st <- eff'ref'new default
    pure $ R'Ref'nt $ st /\ default

instance
  ( Newtype nt t
  ) =>
  X'Results'R'rw (R'Ref'nt nt) t where
  x'results'r'rw'impl (R'Ref'nt (ref /\ _)) = eff'ref'get ref <#> unwrap

instance
  ( Newtype nt t
  ) =>
  X'R'RespondsTo'rw (R'Ref'nt nt)
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
  x'r'mkResponds'r (R'Ref'nt (ref /\ _)) = match
    { extract: responds'const'eff $ eff'ref'get ref <#> unwrap
    }
  x'r'mkResponds'w (R'Ref'nt (ref /\ init)) = match
    { assign: responds'run'eff \v -> eff'ref'set (wrap v) ref
    , reset: responds'const'eff $ eff'ref'set init ref
    }

type X'Ref'nt nt = Reader (RW'Tagged (R'Ref'nt nt))
type X'Ref'nt'w nt = Reader (W'Tagged (R'Ref'nt nt))

x'ref'nt
  :: forall @nt t
   . Newtype nt t
  => Generable nt GDefault nt
  => X'Evaluable (X'Ref'nt nt)
x'ref'nt = x'evaluable @(X'Ref'nt nt) unit

x'ref'nt'w
  :: forall @nt t
   . Newtype nt t
  => Generable nt GDefault nt
  => X'Evaluable (X'Ref'nt'w nt)
x'ref'nt'w = x'evaluable @(X'Ref'nt'w nt) unit

type B'Ref'nt sel nt t = sel (X'Ref'nt nt) t

type B'Ref sel t = sel (X'Ref t) t