module X where

import Prelude
import Effect.Aff as Aff
import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data AffF f a = AffCmd (Aff.Aff f) (f -> a)

derive instance functorAffF :: Functor (AffF f)

type AFF f r = (aff :: (AffF f) | r)

_aff = Proxy :: Proxy "aff"

aff :: forall f r. (Aff.Aff f) -> Run ((AFF f) + r) f
aff f = lift _aff (AffCmd f identity)
