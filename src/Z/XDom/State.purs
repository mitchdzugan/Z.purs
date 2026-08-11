module Z.XDom.State
  ( R
  , T
  , Tg
  , XDomGetState
  , XDomRunState
  , XDomSetState
  ) where

import Z.Prelude hiding (Run)

import Z.Prelude as Z
import Z.XDom.Core as XDom

type Tg r s = { get :: s, set :: s -> XEffTagged "xDomEff" Unit | r }
type T s = Tg () s
type R s = R' (Tg () s)

data XDomGetState

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' (Tg r s)) x' x
  ) =>
  Generable XDomGetState gspec (Z.Run x s) where
  mkGenerable = do
    r <- g1 @XAsk @rp
    pure r.get

data XDomSetState

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' (Tg r s)) x' (XDom.XDOMEFF x)
  ) =>
  Generable XDomSetState gspec (s -> Z.Run (XDom.XDOMEFF x) Unit) where
  mkGenerable s = do
    r <- g1 @XAsk @rp
    XDom.xDomDo $ r.set s

data XDomRunState

instance
  ( GOrDefault "reader" gdesc rp
  , IsSymbol rp
  , Cons rp (R' (T s)) x' x
  ) =>
  Generable XDomRunState gdesc (s -> XDom.RDom x -> XDom.RDom x') where
  mkGenerable initState m = do
    XDom.(<*#) initState \state set -> do
      let env = { set, get: state }
      g1 @XDom.XDomRunR @rp env m