module Z.XDom.State
  ( Get(..)
  , R
  , Run(..)
  , Set(..)
  , T
  , Tg
  ) where

import Z.Prelude hiding (Run, Get(..), Set(..))
import Z.Prelude as Z
import Z.XDom.Preact as XDom

type Tg r s = { get :: s, set :: s -> XPure Unit | r }
type T s = Tg () s
type R s = R' (Tg () s)

data Get = Get

instance Cons0 Get where
  cons0 = Get

instance
  ( Cons rp (R' (Tg r s)) x' x
  , IsSymbol rp
  ) =>
  RWSEFn Get
    rp
    wp
    sp
    ep
    (Z.Run x s) where
  rwseApply _ _ _ _ _ = do
    r <- x @rp @"ask"
    pure r.get

data Set = Set

instance Cons0 Set where
  cons0 = Set

instance
  ( Cons rp (R' (Tg r s)) x' x
  , IsSymbol rp
  ) =>
  RWSEFn Set
    rp
    wp
    sp
    ep
    (s -> Z.Run' x) where
  rwseApply _ _ _ _ _ s = do
    r <- x @rp @"ask"
    xtls @"$" $ r.set s

data Run = Run

instance Cons0 Run where
  cons0 = Run

instance
  ( Cons rp (R' (T s)) x' x
  , IsSymbol rp
  ) =>
  RWSEFn Run
    rp
    wp
    sp
    ep
    (s -> XDom.RDom x -> XDom.RDom x') where
  rwseApply _ _ _ _ _ initState m = do
    XDom.(<*#) initState \state set -> do
      let env = { set, get: state }
      x @rp @"$" XDom.DomRunR env m