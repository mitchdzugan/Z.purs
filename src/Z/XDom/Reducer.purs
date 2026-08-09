module Z.XDom.Reducer
  ( R
  , T
  , Tg
  , X
  , XDomDispatch
  , XDomDispatchT
  , XDomRunReducer
  , XDomRunReducerT
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

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' (Tg r a s)) x' x
  ) =>
  GenerableC XDomDispatchT gspec (a -> Z.Run' x) where
  mkGenerable a = do
    r <- z @(XAsk @@ gspec)
    xDo $ r.set $ r.update r.get a

type XDomDispatch = Generable XDomDispatchT

data XDomRunReducerT = XDomRunReducerT

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
      z @(XDom.XDomRunR @@ gspec) env m

type XDomRunReducer = Generable XDomRunReducerT