module X where

import Prelude

import Core (JsError)
import Data.Either (Either(..), either)
import Data.Lens as Lens
import Data.Symbol as Symbol
import Effect.Aff as Aff
import Prim.Row as Row
import Record as Record
import Run (Run, lift, extract)
import Run.Except (EXCEPT, throw)
import Run.Except as RunE
import Run.Reader (READER)
import Run.Reader as RunR
import Run.State (STATE, execState)
import Run.State as RunS
import Run.Writer (WRITER)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data AffF a = AffCmd (Aff.Aff a)

derive instance functorAffF :: Functor AffF

type AFF x = (aff :: AffF | x)

_aff = Proxy :: Proxy "aff"

aff :: forall f r. (Aff.Aff f) -> Run (AFF + r) f
aff f = lift _aff (AffCmd f)

result :: forall e r x. Either e r -> Run (EXCEPT e + x) r
result res =
  case res of
    Right r -> pure r
    Left e -> throw e

tryAff
  :: forall f x. (Aff.Aff f) -> Run (AFF + EXCEPT JsError + x) f
tryAff a = do
  res <- aff $ Aff.attempt a
  result res

type A x = AFF x
type R r x = READER r x
type E e x = EXCEPT e x
type W w x = WRITER w x
type S s x = STATE s x

type X x r = Run x r

type UpdateX s = X (S s + ()) Unit

updateX :: forall a. a -> UpdateX a -> a
updateX init m = extract $ execState init m

r_view :: forall x r a. Lens.Lens' r a -> Run (READER r + x) a
r_view l = do
  o <- RunR.ask
  pure $ Lens.view l o

s_view :: forall x s a. Lens.Lens' s a -> Run (STATE s + x) a
s_view l = do
  o <- RunS.get
  pure $ Lens.view l o

s_over :: forall x s a. Lens.Lens' s a -> (a -> a) -> Run (STATE s + x) Unit
s_over l f = do
  o <- RunS.get
  RunS.put $ Lens.over l f o

s_set :: forall x s a. Lens.Lens' s a -> a -> Run (STATE s + x) Unit
s_set l a = do
  o <- RunS.get
  RunS.put $ Lens.set l a o

pass :: forall a. Applicative a => a Unit
pass = pure unit

e_map
  :: forall x e1 e2 a
   . (e1 -> e2)
  -> Run (EXCEPT e1 + EXCEPT e2 + x) a
  -> Run (EXCEPT e2 + x) a
e_map f m = do
  res <- RunE.runExcept m
  result $ either (\e1 -> Left $ f e1) (\r -> Right r) res