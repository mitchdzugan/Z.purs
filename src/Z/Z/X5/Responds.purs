module Z.Z.X5.Responds
  ( Responds0
  , Responds1
  , Responds2
  , responds0'id
  , responds0'mk
  , responds0'run
  , responds1'id
  , responds1'mk
  , responds1'run
  , responds2'id
  , responds2'mk
  , responds2'run
  ) where

import Z.Z.X5.UtilPrelude

newtype Responds0 res v = Responds0 (res -> v)

derive instance Functor (Responds0 res)

responds0'mk :: forall res v. (res -> v) -> Responds0 res v
responds0'mk = Responds0

responds0'id :: forall res. Responds0 res res
responds0'id = responds0'mk identity

responds0'run :: forall f res v. Functor f => f res -> Responds0 res v -> f v
responds0'run m (Responds0 f) = f <$> m

-----------------------------------------------------------------------
data Responds1 a1 res v = Responds1 a1 (res -> v)

derive instance Functor (Responds1 a1 res)

responds1'mk :: forall a1 res v. a1 -> (res -> v) -> Responds1 a1 res v
responds1'mk a1 f = Responds1 a1 f

responds1'id :: forall a1 res. a1 -> Responds1 a1 res res
responds1'id a1 = responds1'mk a1 identity

responds1'run
  :: forall f a1 res v. Functor f => (a1 -> f res) -> Responds1 a1 res v -> f v
responds1'run fm (Responds1 a1 f) = f <$> fm a1

-----------------------------------------------------------------------
data Responds2 a1 a2 res v = Responds2 a1 a2 (res -> v)

derive instance Functor (Responds2 a1 a2 res)

responds2'mk
  :: forall a1 a2 res v. a1 -> a2 -> (res -> v) -> Responds2 a1 a2 res v
responds2'mk a1 a2 f = Responds2 a1 a2 f

responds2'id :: forall a1 a2 res. a1 -> a2 -> Responds2 a1 a2 res res
responds2'id a1 a2 = responds2'mk a1 a2 identity

responds2'run
  :: forall f a1 a2 res v
   . Functor f
  => (a1 -> a2 -> f res)
  -> Responds2 a1 a2 res v
  -> f v
responds2'run fm (Responds2 a1 a2 f) = f <$> fm a1 a2