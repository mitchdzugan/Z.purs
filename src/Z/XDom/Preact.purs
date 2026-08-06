module Z.XDom.Preact
  ( ($&)
  , ($&-)
  , ($*&)
  , ($*&-)
  , ($+&)
  , ($+&-)
  , (%%&)
  , (%%)
  , (%%-&)
  , (%%^&)
  , (%^&)
  , (%^)
  , (%^-&)
  , (%^^&)
  , (-$&)
  , (-$*&)
  , (-$+&)
  , (<!&)
  , (<&)
  , (<*#)
  , D2withNewState
  , DTextW
  , DTextW'
  , DTextW_
  , DTextW_'
  , DType
  , DomBindE(..)
  , DomRunR(..)
  , DomS
  , DomS'
  , Duse1Eff
  , Duse1Eff'
  , DuseEff
  , DuseEff'
  , DuseEveryEff
  , DuseEveryEff'
  , DwithKey
  , DwithNewState
  , GDomFn'
  , ISet
  , IdS(..)
  , PropWF(..)
  , RDom
  , RDom'
  , RDomFn
  , RDomFn'
  , ReactEl
  , SetStateFn
  , XDom
  , XDom'
  , XDomFn
  , XDomFn'
  , XDomS
  , XDomS'
  , XEl
  , XPROPS
  , XProps_
  , XSELF
  , XSELF'
  , XSelf
  , XSelf2
  , XSelf2F(..)
  , XSelf_
  , _xProps
  , d
  , da
  , del
  , dpureText
  , dpureTextW
  , dpureTextWnl
  , dpureTextWsp
  , dtext
  , dtextW
  , dtextWnl
  , dtextWsep
  , dtextWsp
  , duse1Eff
  , duse1Eff'
  , duse1Eff_
  , duseEff
  , duseEff'
  , duseEff_
  , duseEveryEff
  , duseEveryEff'
  , duseEveryEff_
  , dwithKey
  , dwithNewState
  , extXSelf
  , propWFKey
  , propWFVal
  , renderX
  , runEls
  , unId
  , xRawFragment
  ) where

import Z.Prelude

import Debug (trace)

data IdS a = IdS a

instance Functor IdS where
  map f (IdS a) = IdS (f a)

unId :: forall a. IdS a -> a
unId (IdS a) = a

newtype XSelf2F x m = XSelf2F
  { run :: forall a. Run x a -> Run () (m a)
  , fmap :: forall a1 a2. (a1 -> a2) -> m a1 -> m a2
  , unEls :: m (Array ReactEl) -> Array ReactEl
  , unUnit :: m Unit -> Unit
  , unDisposable :: m (Run x Unit) -> Run x Unit
  }

type XSelf2 x = Exists (XSelf2F x)

runEls :: forall x. XSelf2 x -> Run x (Array ReactEl) -> Array ReactEl
runEls self m = runExists useSelf self
  where
  useSelf :: forall m. XSelf2F x m -> Array ReactEl
  useSelf (XSelf2F { run, unEls }) = unEls $ eval_ $ run m

runUnit :: forall x. XSelf2 x -> Run x Unit -> Unit
runUnit self m = runExists useSelf self
  where
  useSelf :: forall m. XSelf2F x m -> Unit
  useSelf (XSelf2F { run, unUnit }) = unUnit $ eval_ $ run m

runDisposable :: forall x. XSelf2 x -> Run x (Run x Unit) -> Unit -> Run x Unit
runDisposable self m = runExists useSelf self
  where
  useSelf :: forall m. XSelf2F x m -> Unit -> Run x Unit
  useSelf (XSelf2F { run, unDisposable }) =
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

extXSelfImpl
  :: forall x' x @m' m
   . Functor m
  => (forall a. Run x a -> Run x' (m a))
  -> (m (Array ReactEl) -> Array ReactEl)
  -> (m Unit -> Unit)
  -> (m (Run x Unit) -> Run x Unit)
  -> XSelf2F x' m'
  -> XSelf2 x
extXSelfImpl adapt unEls unUnit unDisposable (XSelf2F s') = mkExists $
  mkMTXSelf2
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

extXSelf
  :: forall x' x @m
   . Functor m
  => XSelf2 x'
  -> (forall a. Run x a -> Run x' (m a))
  -> (m (Array ReactEl) -> Array ReactEl)
  -> (m Unit -> Unit)
  -> (m (Run x Unit) -> Run x Unit)
  -> XSelf2 x
extXSelf xSelf' adapt unEls unUnit unDisposable =
  runExists (extXSelfImpl adapt unEls unUnit unDisposable) xSelf'

mkMTXSelf2
  :: forall @m x
   . (forall a. Run x a -> Run () (m a))
  -> (forall a1 a2. (a1 -> a2) -> m a1 -> m a2)
  -> (m (Array ReactEl) -> Array ReactEl)
  -> (m Unit -> Unit)
  -> (m (Run x Unit) -> Run x Unit)
  -> XSelf2F x m
mkMTXSelf2 run fmap unEls unUnit unDisposable = XSelf2F
  { run
  , fmap
  , unEls
  , unUnit
  , unDisposable
  }

baseSelf :: XSelf2 (XBASE ())
baseSelf = mkExists $ mkMTXSelf2 (\m -> runXBase m <#> IdS) map unId unId unId

type XSelf x =
  { runEls :: Run x (Array ReactEl) -> Array ReactEl
  , runUnit :: Run x Unit -> Unit
  , runDisposable :: Run x (Run x Unit) -> (Unit -> Unit)
  }

type XSelf_ = "xDomSelf"
type XSELF' x' x = (xDomSelf :: (R' (XSelf2 x')) | x)
type XSELF x = XSELF' x x

type GDomFn'
  :: forall k
   . (k -> Row (Type -> Type))
  -> k
  -> (Row (Type -> Type) -> Row (Type -> Type))
  -> Type
  -> Type
type GDomFn' wx x fx a = Run (fx (XSELF' (wx x) (Wa ReactEl (wx x)))) a

type XDomFn' x fx a = GDomFn' XBASE x fx a
type XDomFn x a = GDomFn' XBASE x IdT a
type XDom x = GDomFn' XBASE x IdT Unit
type XDom' x fx = GDomFn' XBASE x fx Unit

type RDomFn' x fx a = GDomFn' IdT x fx a
type RDomFn x a = GDomFn' IdT x IdT a
type RDom x = GDomFn' IdT x IdT Unit
type RDom' x fx = GDomFn' IdT x fx Unit

type XPROPS x = (xProps :: Writer (Array PropWF) | x)

renderX :: XDom () -> ReactEl
renderX m = js_renderFragment $ evalX $ x @XSelf_ @"runR" baseSelf $ x' @"execW"
  $ m

type DwithKey = forall x. String -> RDom x -> RDom x

dwithKey :: DwithKey
dwithKey k m = do
  rn <- mkDimAt @XSelf_ @Ask
  mkDim @Say $ js_withKey k $ js_renderFragment $ runEls rn $ x' @"execW"
    $ x @XSelf_ @"runR" rn
    $ m

infixr 3 dwithKey as <!&

type SetStateFn s = s -> X () Unit

type DwithNewState =
  forall x s. s -> (s -> (s -> XPure Unit) -> RDom x) -> RDom x

dwithNewState :: DwithNewState
dwithNewState initalState fm = do
  rn <- mkDimAt @XSelf_ @Ask
  mkDim @Say $ flip (js_withState pure) initalState (renderFn rn)
  where
  renderFn rn s ss = runEls rn $ mkDim @ExecW $ x @XSelf_ @"runR" rn $ fm
    s
    (w ss)
  w ss s = XPure $ ss s

type D2withNewState =
  forall x x' s. s -> (s -> (s -> Run x' Unit) -> RDom x) -> RDom x

infixr 3 dwithNewState as <*#

xRawFragment :: forall x. Array ReactEl -> RDom x
xRawFragment = mkDim @Say <<< js_renderFragment

data DomRunR = DomRunR

instance DimensionedValTag DomRunR DomRunR
instance
  ( R_ dspec rp
  , IsSymbol rp
  , Cons rp (R' r) x' x
  ) =>
  DimensionedVal DomRunR dspec (r -> RDom x -> RDom x') where
  mkDimensional _ _ env m = do
    r <- mkDimAt @XSelf_ @Ask
    let ir = extXSelf @(IdS) r ((<$>) IdS <<< x @rp @"runR" env) unId unId unId
    xRawFragment $ runEls ir $ x' @"execW" $ x @XSelf_ @"runR" ir $ m

instance Cons0 DomRunR where
  cons0 = DomRunR

instance
  ( Cons rp (R' r) x' x
  , IsSymbol rp
  ) =>
  RWSEFn DomRunR rp wp sp ep (r -> RDom x -> RDom x') where
  rwseApply _ _ _ _ _ env m = do
    r <- mkDimAt @XSelf_ @Ask
    let ir = extXSelf @(IdS) r ((<$>) IdS <<< x @rp @"runR" env) unId unId unId
    xRawFragment $ runEls ir $ x' @"execW" $ x @XSelf_ @"runR" ir $ m

data DomBindE = DomBindE

instance Cons0 DomBindE where
  cons0 = DomBindE

instance
  ( Cons ep (E' e) x' x
  , IsSymbol ep
  ) =>
  RWSEFn DomBindE
    rp
    wp
    sp
    ep
    ((e -> RDom x') -> RDom x -> RDom x') where
  rwseApply _ _ _ _ _ em m = do
    r <- mkDimAt @XSelf_ @Ask
    let
      ir = extXSelf @(Either e) r
        (mkDimAt @ep @Try)
        ( \eOr -> case eOr of
            (Left e) -> js_throwBoundedError e
            (Right v) -> v
        )
        ( \eOr -> case eOr of
            (Left _) -> unit
            (Right v) -> v
        )
        ( \eOr -> case eOr of
            (Left _) -> pure unit
            (Right v) -> v
        )
    mkDim @Say $ js_withBoundedError (rErr r) (rMain ir)
    where
    rMain rn _ = js_renderFragment $ runEls rn $ x' @"execW"
      $ x @XSelf_ @"runR" rn
      $ m
    rErr rn e = js_renderFragment $ runEls rn $ x' @"execW"
      $ x @XSelf_ @"runR" rn
      $ em e

type DomS a s = { get :: s, act :: a -> XPure Unit }

type DomS' s = DomS s s

type XDomS a s = R' (DomS a s)

type XDomS' s = R' (DomS s s)

type ISet s = s -> XPure Unit

type XEl x = Wa ReactEl + XPROPS x
type XProps_ = "xProps"
_xProps = Proxy :: Proxy XProps_

type DuseEff = forall x a. Eq a => a -> Run x (Run' x) -> RDom x
type Duse1Eff = forall x. Run x (Run' x) -> RDom x
type DuseEveryEff = forall x. Run x (Run' x) -> RDom x
type DuseEff' = forall x a. Eq a => a -> Run' x -> RDom x
type Duse1Eff' = forall x. Run' x -> RDom x
type DuseEveryEff' = forall x. Run' x -> RDom x

duseEff :: DuseEff
duseEff v m = do
  r <- mkDimAt @XSelf_ @Ask
  mkDim @Say $ js_effComponent eq v
    (\_ -> let runD' = runDisposable r m in \_ -> runUnit r (runD' unit))
    ((#) unit)

duse1Eff :: Duse1Eff
duse1Eff = duseEff unit

duseEveryEff :: DuseEveryEff
duseEveryEff = duseEff antiUnit

duseEff' :: DuseEff'
duseEff' v m = duseEff v do
  m
  pure $ pure unit

duse1Eff' :: Duse1Eff'
duse1Eff' = duseEff' unit

duseEveryEff' :: DuseEveryEff'
duseEveryEff' = duseEff' antiUnit

duseEff_ :: DuseEff'
duseEff_ v m = duseEff v $ pure m

duse1Eff_ :: Duse1Eff'
duse1Eff_ = duseEff_ unit

duseEveryEff_ :: DuseEveryEff'
duseEveryEff_ = duseEff_ antiUnit

type DType f =
  { div :: f
  , button :: f
  , a :: f
  , iframe :: f
  , article :: f
  , pre :: f
  , span :: f
  , text :: forall t x. SText t => t -> RDom x
  , textW :: DTextW_
  , textWsp :: DTextW_
  , textWsp_ ::
      forall x. ((forall t. (SText t) => (t -> StrW)) -> StrW) -> RDom x
  , textWnl :: DTextW_
  , textWsep :: String -> DTextW_
  , useEff :: DuseEff
  , use1Eff :: Duse1Eff
  , useEveryEff :: DuseEveryEff
  , useEff' :: DuseEff'
  , use1Eff' :: Duse1Eff'
  , useEveryEff' :: DuseEveryEff'
  , useEff_ :: DuseEff'
  , use1Eff_ :: Duse1Eff'
  , useEveryEff_ :: DuseEveryEff'
  , withKey :: DwithKey
  , withNewState :: DwithNewState
  }

mkD :: forall f x. ((RDom' x XEl -> RDom x) -> f) -> DType f
mkD f =
  { div: f $ del "div"
  , button: f $ del "button"
  , a: f $ del "a"
  , iframe: f $ del "iframe"
  , article: f $ del "article"
  , pre: f $ del "pre"
  , span: f $ del "span"
  , text: dtext
  , textW: dtextW
  , textWsp: dtextWsp
  , textWsp_: dtextWsp
  , textWnl: dtextWnl
  , textWsep: dtextWsep
  , useEff: duseEff
  , use1Eff: duse1Eff
  , useEveryEff: duseEveryEff
  , useEff': duseEff'
  , use1Eff': duse1Eff'
  , useEveryEff': duseEveryEff'
  , useEff_: duseEff_
  , use1Eff_: duse1Eff_
  , useEveryEff_: duseEveryEff_
  , withKey: dwithKey
  , withNewState: dwithNewState
  }

d :: forall x. DType (RDom' x XEl -> RDom x)
d = mkD id

del
  :: forall x
   . String
  -> RDom' x XEl
  -> RDom x
del s m = do
  (propWFs /\ elBuild) <- x @"xProps" @"runW" $ x' @"execW" m
  let props = js_propsFromPropWs propWFKey propWFVal propWFs
  mkDim @Say $ js_renderEl s (encodeOpts props) elBuild

infixr 3 del as <&

dtext :: forall t x. SText t => t -> RDom x
dtext t = mkDim @Say $ js_textEl $ stext t

type DTextW_' x = ((forall t. (SText t) => (t -> StrW)) -> StrW) -> RDom x
type DTextW_ = forall x. DTextW_' x
type DTextW' x = forall t. (SText t) => ((t -> StrW) -> StrW) -> RDom x
type DTextW = forall x. DTextW' x

dtextWsep :: String -> DTextW_
dtextWsep sep fm = dtext $ joinStrW sep $ fm xSayText

dtextW :: DTextW_
dtextW = dtextWsep ""

xSayText :: forall t. (SText t) => t -> StrW
xSayText = mkDim @Say <<< stext

dtextWsp :: DTextW_
dtextWsp = dtextWsep " "

dtextWnl :: DTextW_
dtextWnl = dtextWsep "\n"

dpureText :: forall x. (RDom' x XEl -> RDom x) -> String -> RDom x
dpureText fm m = fm $ mkDim @Say $ js_textEl m

dpureTextW :: forall x. (RDom' x XEl -> RDom x) -> DTextW_' x
dpureTextW fm m = fm $ dtextW m

dpureTextWsp :: forall x. (RDom' x XEl -> RDom x) -> DTextW_' x
dpureTextWsp fm m = fm $ dtextWsp m

dpureTextWnl :: forall x. (RDom' x XEl -> RDom x) -> DTextW_' x
dpureTextWnl fm m = fm $ dtextWnl m

infixr 3 dtext as %^
infixr 3 dtextW as %^&
infixr 3 dtextWsp as %^-&
infixr 3 dtextWnl as %^^&
infixr 3 dpureText as %%
infixr 3 dpureTextW as %%&
infixr 3 dpureTextWsp as %%-&
infixr 3 dpureTextWnl as %%^&

infixr 3 duseEff as $&

infixr 3 duse1Eff as $+&

infixr 3 duseEveryEff as $*&

infixr 3 duseEff' as $&-

infixr 3 duse1Eff' as $+&-

infixr 3 duseEveryEff' as $*&-

infixr 3 duseEff_ as -$&

infixr 3 duse1Eff_ as -$+&

infixr 3 duseEveryEff_ as -$*&

foreign import data ReactEl :: Type

foreign import js_textEl :: String -> ReactEl
foreign import js_renderFragment :: Array ReactEl -> ReactEl
foreign import js_renderEl :: String -> Json -> Array ReactEl -> ReactEl
foreign import js_propsFromPropWs
  :: (PropWF -> String) -> (PropWF -> JsAny) -> Array PropWF -> Json

foreign import js_withState
  :: forall s
   . (Unit -> Run () Unit)
  -> (s -> (SetStateFn s) -> Array ReactEl)
  -> s
  -> ReactEl

foreign import js_withKey :: String -> ReactEl -> ReactEl
foreign import js_effComponent
  :: forall a
   . (a -> a -> Boolean)
  -> a
  -> (Unit -> (Unit -> Unit))
  -> ((Unit -> Unit) -> Unit)
  -> ReactEl

foreign import js_withBoundedError
  :: forall e. (e -> ReactEl) -> (Unit -> ReactEl) -> ReactEl

foreign import js_throwBoundedError :: forall e a. e -> a

data PropWF
  = Href String
  | ClassName String
  | OnClick (Int -> Unit)
  | PKey String

propWFKey :: PropWF -> String
propWFKey (ClassName _) = "className"
propWFKey (Href _) = "href"
propWFKey (OnClick _) = "onClick"
propWFKey (PKey _) = "key"

propWFVal :: PropWF -> JsAny
propWFVal (ClassName s) = jsAny s
propWFVal (Href s) = jsAny s
propWFVal (OnClick s) = jsAny s
propWFVal (PKey s) = jsAny s

da
  :: { key :: forall x. String -> RDom' x XPROPS
     , cn :: forall x. String -> RDom' x XPROPS
     , cnW :: forall x. ((String -> StrW) -> StrW) -> RDom' x XPROPS
     , href :: forall x. String -> RDom' x XPROPS
     , onClick :: forall x. (Int -> Run' x) -> RDom' x XPROPS
     }
da =
  { key: mkDimAt @XProps_ @Tell <<< pure <<< PKey
  , cn: mkDimAt @XProps_ @Tell <<< pure <<< ClassName
  , cnW: \fm -> mkDimAt @XProps_ @Tell $ pure $ ClassName $ joinStrW " " $ fm $
      mkDim @Say
  , href: mkDimAt @XProps_ @Tell <<< pure <<< Href
  , onClick: \f -> do
      r <- mkDimAt @XSelf_ @Ask
      mkDimAt @XProps_ @Tell $ pure $ OnClick $ \e -> runUnit r $ f e
  }
