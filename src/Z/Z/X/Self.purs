module Z.Z.X.Self where

import Z.Prelude

import Z.XDom.Preact (ReactEl)

data Self__________ x = Self (forall a. Run x a -> Run () a)

{-

run :: forall x a. Self x -> Run x a -> a
run (Self runner) m = eval_ $ runner m

extendSelf :: forall x' x. Self x' -> (forall a. Run x a -> Run x' a) -> Self x
extendSelf (Self runner) fm = Self (runner <<< fm)

baseSelf :: Self ()
baseSelf = Self id


data MComp :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
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
-}