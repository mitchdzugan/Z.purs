module Z.Z.Defaultable.Util
  ( d
  , z
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Run (Run)
import Run.Writer (Writer, WRITER, tellAt)
import Type.Proxy (Proxy(..))
import Z.Z.Defaultable.Generable (GAt, GDescAt(..), GDescDefault(..), GTag, Generable, GenerableP, GenerableW, class GDefaultable, class GOrDefault_, class GTaggedDefaultable, class GenerableC, class GenerableWUnwrap, gDefault, gTaggedDefault, mkGenerable)

d :: forall @tag a. GTaggedDefaultable tag a => a
d = gTaggedDefault @tag

z :: forall @tag a. GTaggedDefaultable tag a => a
z = gTaggedDefault @tag

data SayT = SayT

type Say = Generable SayT

instance
  ( IsSymbol wp
  , Cons wp (Writer (m w)) x' x
  , Monad m
  , GOrDefault_ "writer" gspec wp
  ) =>
  GenerableC SayT gspec (w -> Run x Unit) where
  mkGenerable w = do
    tellAt (Proxy @wp) $ pure w
    pure unit

tt :: Run (WRITER (Array Int) ()) Unit
tt = do
  d @Say 1