module Z.Z.X.Responds
  ( Responds'Const
  , Responds
  , responds'const
  , responds'const'eff
  , responds'id
  , responds'mk
  , responds'run
  , responds'run'eff
  ) where

import Z.Z.X.UtilPrelude

newtype Responds args res v = Responds (args /\ (res -> v))

derive instance Functor (Responds a1 res)

type Responds'Const t = Responds Unit t

responds'mk :: forall args res v. args -> (res -> v) -> Responds args res v
responds'mk args f = Responds $ args /\ f

responds'id :: forall args res. args -> Responds args res res
responds'id args = responds'mk args identity

responds'run
  :: forall args res v
   . (args -> res)
  -> Responds args res v
  -> v
responds'run fm (Responds (args /\ f)) = f $ fm args

responds'const :: forall res v. res -> Responds Unit res v -> v
responds'const v (Responds (_ /\ f)) = f v

responds'run'eff
  :: forall args res v
   . (args -> Effect res)
  -> Responds args res v
  -> v
responds'run'eff fm (Responds (args /\ f)) =
  f <$> unsafePerformEffect $ fm args

responds'const'eff :: forall res v. Effect res -> Responds Unit res v -> v
responds'const'eff v (Responds (_ /\ f)) = f $ unsafePerformEffect v
