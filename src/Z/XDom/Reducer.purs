module Z.XDom.Reducer
  ( R
  , T
  , Tg
  , X
  , XDomDispatch
  , XDomDispatchT(..)
  , XDomRunReducer
  , XDomRunReducerT(..)
  ) where

import Z.Prelude hiding (Run)

import Z.Prelude as Z
import Z.XDom.Core as XDom
import Z.XDom.State as St

type Tg r a s = St.Tg (update :: s -> a -> s | r) s
type T a s = Tg () a s
type R a s = R' (Tg () a s)
type X a s x = Z.R (Tg () a s) x

data XDomDispatchT = XDomDispatchT
type XDomDispatch = Generable XDomDispatchT

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' (Tg r a s)) x' x
  ) =>
  GenerableC XDomDispatchT gspec (a -> Z.Run' x) where
  mkGenerable a = do
    r <- g1 @XAsk @rp
    xDo $ r.set $ r.update r.get a

data XDomRunReducerT = XDomRunReducerT
type XDomRunReducer = Generable XDomRunReducerT

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' (T a s)) x' x
  ) =>
  GenerableC XDomRunReducerT
    gspec
    (s -> (s -> a -> s) -> XDom.RDom x -> XDom.RDom x') where
  mkGenerable initState update m = do
    XDom.(<*#) initState \state set -> do
      let env = { set, get: state, update }
      g1 @XDom.XDomRunR @rp env m
