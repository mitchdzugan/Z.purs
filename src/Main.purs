module Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Effect.Console (log)
import Run
import Type.Proxy
import Type.Row

data AffF f a = AffCmd (Aff.Aff f) (f -> a)

derive instance functorAffF :: Functor (AffF f)

type AFF f r = (aff :: (AffF f) | r)

_aff = Proxy :: Proxy "aff"

aff :: forall f r. (Aff.Aff f) -> Run ((AFF f) + r) f
aff str = lift _aff (AffCmd str identity)

main :: Effect Unit
main = do
  log "🍝"
