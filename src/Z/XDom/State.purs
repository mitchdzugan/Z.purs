module Z.XDom.State
  ( R
  , T
  , Tg
  , XDomGetState
  , XDomGetStateT
  , XDomRunState
  , XDomRunStateT
  , XDomSetState
  , XDomSetStateT
  ) where

import Z.Prelude hiding (Get(..), Run, Set(..))

import Z.Prelude as Z
import Z.XDom.Core as XDom

type Tg r s = { get :: s, set :: s -> XEff Unit | r }
type T s = Tg () s
type R s = R' (Tg () s)

data XDomGetStateT = XDomGetStateT
type XDomGetState = Generable XDomGetStateT

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' (Tg r s)) x' x
  ) =>
  GenerableC XDomGetStateT gspec (Z.Run x s) where
  mkGenerable = do
    r <- mkDimAt @rp @Ask
    pure r.get

data XDomSetStateT = XDomSetStateT
type XDomSetState = Generable XDomSetStateT

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' (Tg r s)) x' x
  ) =>
  GenerableC XDomSetStateT gspec (s -> Z.Run x Unit) where
  mkGenerable s = do
    r <- z @(XAsk @@ gspec)
    xDo $ r.set s

data XDomRunStateT = XDomRunStateT

type XDomRunState = Generable XDomRunStateT

instance
  ( GOrDefault "reader" gdesc rp
  , IsSymbol rp
  , Cons rp (R' (T s)) x' x
  ) =>
  GenerableC XDomRunStateT gdesc (s -> XDom.RDom x -> XDom.RDom x') where
  mkGenerable initState m = do
    XDom.(<*#) initState \state set -> do
      let env = { set, get: state }
      z @(XDom.XDomRunR @@ gdesc) env m