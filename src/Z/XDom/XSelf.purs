module Z.XDom.XSelf
  ( IdS(..)
  , MComp(..)
  , XSelf
  , XSelfF(..)
  , baseXSelf
  , runDisposable
  , runEls
  , runUnit
  , unCompDisposable
  , unCompF
  , unId
  , xSelfExtend
  , xSelfExtend'
  ) where

import Z.Prelude
import Z.XDom.Preact (ReactEl)

data IdS a = IdS a

instance Functor IdS where
  map f (IdS a) = IdS (f a)

unId :: forall a. IdS a -> a
unId (IdS a) = a

newtype XSelfF x m = XSelfF
  { run :: forall a. Run x a -> Run () (m a)
  , fmap :: forall a1 a2. (a1 -> a2) -> m a1 -> m a2
  , unEls :: m (Array ReactEl) -> Array ReactEl
  , unUnit :: m Unit -> Unit
  , unDisposable :: m (Run x Unit) -> Run x Unit
  }

type XSelf x = Exists (XSelfF x)

mkMXSelfF
  :: forall @m x
   . (forall a. Run x a -> Run () (m a))
  -> (forall a1 a2. (a1 -> a2) -> m a1 -> m a2)
  -> (m (Array ReactEl) -> Array ReactEl)
  -> (m Unit -> Unit)
  -> (m (Run x Unit) -> Run x Unit)
  -> XSelfF x m
mkMXSelfF run fmap unEls unUnit unDisposable = XSelfF
  { run
  , fmap
  , unEls
  , unUnit
  , unDisposable
  }

runEls :: forall x. XSelf x -> Run x (Array ReactEl) -> Array ReactEl
runEls self m = runExists useSelf self
  where
  useSelf :: forall m. XSelfF x m -> Array ReactEl
  useSelf (XSelfF { run, unEls }) = unEls $ eval_ $ run m

runUnit :: forall x. XSelf x -> Run x Unit -> Unit
runUnit self m = runExists useSelf self
  where
  useSelf :: forall m. XSelfF x m -> Unit
  useSelf (XSelfF { run, unUnit }) = unUnit $ eval_ $ run m

runDisposable :: forall x. XSelf x -> Run x (Run x Unit) -> Unit -> Run x Unit
runDisposable self m = runExists useSelf self
  where
  useSelf :: forall m. XSelfF x m -> Unit -> Run x Unit
  useSelf (XSelfF { run, unDisposable }) =
    let d' = eval_ $ run m in \_ -> unDisposable d'

data MComp m' m x = MComp (m' (m x))

instance
  ( Functor m'
  , Functor m
  ) =>
  Functor (MComp m' m) where
  map f (MComp m) = MComp (m <#> \i -> i <#> f)

unCompF
  :: forall m' m t
   . (forall a1 a2. (a1 -> a2) -> m' a1 -> m' a2)
  -> (m' t -> t)
  -> (m t -> t)
  -> MComp m' m t
  -> t
unCompF fmap' f' f (MComp m) = f' (fmap' f m)

unCompDisposable
  :: forall x x' m' m
   . Functor m
  => (forall a1 a2. (a1 -> a2) -> m' a1 -> m' a2)
  -> (forall a. Run x' a -> Run () (m' a))
  -> (forall a. Run x a -> Run x' (m a))
  -> (m' Unit -> Unit)
  -> (m Unit -> Unit)
  -> (m' (Run x' Unit) -> (Run x' Unit))
  -> (m (Run x Unit) -> (Run x Unit))
  -> MComp m' m (Run x Unit)
  -> (Run x Unit)
unCompDisposable fmap' run adapt unUnit' unUnit f' f (MComp m) = do
  let xOff = f' $ flip fmap' m \m' -> unUnit <$> adapt (f m')
  pure $ unUnit' $ eval_ $ run xOff

xSelfFExtend
  :: forall x' x @m' m
   . Functor m
  => (forall a. Run x a -> Run x' (m a))
  -> (m (Array ReactEl) -> Array ReactEl)
  -> (m Unit -> Unit)
  -> (m (Run x Unit) -> Run x Unit)
  -> XSelfF x' m'
  -> XSelf x
xSelfFExtend adapt unEls unUnit unDisposable (XSelfF s') = mkExists $
  mkMXSelfF
    @(MComp m' m)
    (\m -> (s'.run (adapt m)) <#> MComp)
    ( ( \f (MComp m) -> MComp (s'.fmap (\i -> i <#> f) m)
      )
    )
    (unCompF s'.fmap s'.unEls unEls)
    (unCompF s'.fmap s'.unUnit unUnit)
    ( unCompDisposable s'.fmap s'.run adapt s'.unUnit unUnit s'.unDisposable
        unDisposable
    )

xSelfExtend
  :: forall x' x @m
   . Functor m
  => (forall a. Run x a -> Run x' (m a))
  -> (m (Array ReactEl) -> Array ReactEl)
  -> (m Unit -> Unit)
  -> (m (Run x Unit) -> Run x Unit)
  -> XSelf x'
  -> XSelf x
xSelfExtend adapt unEls unUnit unDisposable =
  runExists (xSelfFExtend adapt unEls unUnit unDisposable)

xSelfExtend'
  :: forall x' x. (forall a. Run x a -> Run x' a) -> XSelf x' -> XSelf x
xSelfExtend' adapt = runExists (xSelfFExtend (adapt <##> Identity) un' un' un')

baseXSelf :: XSelf (XBASE ())
baseXSelf = mkExists $ mkMXSelfF (runXBase <##> Identity) map un' un' un'
