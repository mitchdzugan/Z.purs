module Z.XDom.Reducer
  ( Dispatch(..)
  , R
  , Run(..)
  , T
  , Tg
  , module StExp
  ) where

import Z.Prelude hiding (Run)
import Z.Prelude as Z
import Z.XDom.Preact as XDom
import Z.XDom.State as St
import Z.XDom.State (Get(..)) as StExp

type Tg r a s = St.Tg (update :: s -> a -> s | r) s
type T a s = Tg () a s
type R a s = R' (Tg () a s)

data Dispatch = Dispatch

instance
  ( Cons rp (R' (Tg r a s)) x' x
  , IsSymbol rp
  ) =>
  RWSEFn Dispatch
    rp
    wp
    sp
    ep
    (a -> Z.Run' x) where
  rwseApply _ _ _ _ _ a = do
    r <- xAt @rp Ask
    x $ r.set $ r.update r.get a

data Run = Run

instance
  ( Cons rp (R' (T a s)) x' x
  , IsSymbol rp
  ) =>
  RWSEFn Run
    rp
    wp
    sp
    ep
    (s -> (s -> a -> s) -> XDom.RDom x -> XDom.RDom x') where
  rwseApply _ _ _ _ _ initState update m = do
    XDom.(<*#) initState \state set -> do
      let env = { set, get: state, update }
      xAt @rp XDom.DomRunR env m
