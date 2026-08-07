module Z.XDom.Reducer
  ( Dispatch(..)
  , R
  , Run(..)
  , T
  , Tg
  , X
  , module StExp
  ) where

import Z.Prelude hiding (Run)
import Z.Prelude as Z
import Z.XDom.Core as XDom
import Z.XDom.State as St
import Z.XDom.State (Get(..)) as StExp

type Tg r a s = St.Tg (update :: s -> a -> s | r) s
type T a s = Tg () a s
type R a s = R' (Tg () a s)
type X a s x = Z.R (Tg () a s) x

data Dispatch = Dispatch

instance DimensionedValTag Dispatch Dispatch

instance
  ( RP_ dspec rp
  , IsSymbol rp
  , Cons rp (R' (Tg r a s)) x' x
  ) =>
  DimensionedVal Dispatch dspec (a -> Z.Run' x) where
  mkDimensional _ _ a = do
    r <- mkDimAt @rp @Ask
    xDo $ r.set $ r.update r.get a

instance Cons0 Dispatch where
  cons0 = Dispatch

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
    r <- mkDimAt @rp @Ask
    xDo $ r.set $ r.update r.get a

data Run = Run

instance Cons0 Run where
  cons0 = Run

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
      mkDimAt @rp @XDom.DomRunR env m
