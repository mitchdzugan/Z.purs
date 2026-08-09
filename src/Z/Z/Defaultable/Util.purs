module Z.Z.Defaultable.Util
  ( z
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Run (Run)
import Run.Writer (Writer, WRITER, tellAt)
import Type.Proxy (Proxy(..))
import Z.Z.Defaultable.Generable (class GOrDefault, class GTaggedDefaultable, class GenerableC, class GenerableNicknameC, type (@@), GenerableNickname, Generable, Generable, GenerableNickname, gTaggedDefault)

z :: forall @tag a. GTaggedDefaultable tag a => a
z = gTaggedDefault @tag

data SayT = SayT

type Say = Generable SayT

instance
  ( IsSymbol wp
  , Cons wp (Writer (m w)) x' x
  , Monad m
  , GOrDefault "writer" gspec wp
  ) =>
  GenerableC SayT gspec (w -> Run x Unit) where
  mkGenerable w = do
    tellAt (Proxy @wp) $ pure w
    pure unit
