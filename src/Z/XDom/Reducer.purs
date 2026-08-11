module Z.XDom.Reducer
  ( R
  , T
  , Tg
  , X
  , XDomDispatch
  , XDomRunReducer
  ) where

import Z.Prelude hiding (Run)

import Z.Prelude as Z
import Z.XDom.Core as XDom
import Z.XDom.State as St

type Tg r a s = St.Tg (update :: s -> a -> s | r) s
type T a s = Tg () a s
type R a s = R' (Tg () a s)
type X a s x = Z.R (Tg () a s) x

data XDomDispatch

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' (T a s)) x' x
  ) =>
  Generable XDomDispatch gspec (a -> Z.Run' x) where
  mkGenerable a = do
    r <- g1 @XAsk @rp
    xDo $ r.set $ r.update r.get a

data XDomRunReducer

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' (T a s)) x' x
  ) =>
  Generable XDomRunReducer
    gspec
    (s -> (s -> a -> s) -> XDom.RDom x -> XDom.RDom x') where
  mkGenerable initState update m = do
    XDom.(<*#) initState \state set -> do
      let env = { set, get: state, update }
      g1 @XDom.XDomRunR @rp env m
