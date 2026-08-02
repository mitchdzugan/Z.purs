module Z.XDOM.Preact where

import Z.Prelude

type XSelf x = (Run (x) (Array ReactEl) -> Array ReactEl)
type XSelf_ = "xDomSelf"
type XSELF' x' x = (xDomSelf :: (Reader (XSelf x')) | x)
type XSELF x = XSELF' x x

type XDomFn' x fx a = X (fx (XSELF' (XBASE x) (Wa ReactEl x))) a
type XDomFn x a = XDomFn' x IdT a
type XDom x = XDomFn' x IdT Unit
type XDom' x fx = XDomFn' x fx Unit

type RDomFn' x fx a = Run (fx (XSELF' (x) (Wa ReactEl x))) a
type RDomFn x a = RDomFn' x IdT a
type RDom x = RDomFn' x IdT Unit
type RDom' x fx = RDomFn' x fx Unit

type XPROPS x = (xProps :: Writer (Array PropWF) | x)

renderX :: XDom () -> ReactEl
renderX m = js_renderFragment $ evalX $ xAt @XSelf_ RunR baseR $ x ExecW $ m
  where
  baseR mm = evalX mm

upRunner
  :: forall @p x x' r
   . Cons p (Reader r) x' x
  => IsSymbol p
  => r
  -> XSelf x'
  -> XSelf x
upRunner env (fm) = fm <<< xAt @p RunR env

xDomKeyed
  :: forall x
   . String
  -> XDom x
  -> XDom x
xDomKeyed k m = do
  rn <- xAt @XSelf_ Ask
  x Say $ js_withKey k $ js_renderFragment $ rn $ x ExecW $ xAt @XSelf_ RunR rn
    $ m

xDomNewState
  :: forall x s
   . s
  -> (s -> (s -> X () Unit) -> RDom x)
  -> RDom x
xDomNewState initalState fm = do
  rn <- xAt @XSelf_ Ask
  x Say $ flip (js_withState pure) initalState (renderFn rn)
  where
  renderFn rn s ss = rn $ x ExecW $ xAt @XSelf_ RunR rn $ fm s ss

data DomRunR = DomRunR

instance
  ( Cons rp (Reader r) x' x
  , IsSymbol rp
  ) =>
  RWSEFn DomRunR rp wp sp ep (r -> RDom x -> RDom x') where
  rwseApply _ _ _ _ _ env m = do
    runner <- xAt @XSelf_ Ask
    let irunner = upRunner @rp env runner
    x Say $ js_renderFragment $ irunner $ x ExecW $ xAt @XSelf_ RunR irunner $ m

data DomRunRWithNewStateSetter = DomRunRWithNewStateSetter

instance
  ( Cons rp (XDomStateSetter s) x' x
  , IsSymbol rp
  ) =>
  RWSEFn DomRunRWithNewStateSetter
    rp
    wp
    sp
    ep
    (s -> RDom x -> RDom x') where
  rwseApply _ _ _ _ _ initState m = do
    xDomNewState initState \state setState -> do
      let env = { set: setState, get: state }
      xAt @rp DomRunR env m

data DomRunRWithNewStateReducer = DomRunRWithNewStateReducer

instance
  ( Cons rp (XDomStateReducer a s) x' x
  , IsSymbol rp
  ) =>
  RWSEFn DomRunRWithNewStateReducer
    rp
    wp
    sp
    ep
    (s -> (s -> a -> s) -> RDom x -> RDom x') where
  rwseApply _ _ _ _ _ initState updateState m = do
    xDomNewState initState \state setState -> do
      let act = \a -> setState $ updateState state a
      let env = { act, get: state }
      xAt @rp DomRunR env m

data DomBindError = DomBindError

instance
  ( Cons ep (Except e) x' x
  , IsSymbol ep
  ) =>
  RWSEFn DomBindError
    rp
    wp
    sp
    ep
    ((e -> RDom x') -> RDom x -> RDom x') where
  rwseApply _ _ _ _ _ em m = do
    runner <- xAt @XSelf_ Ask
    let irunner = \mm -> runner $ xAt @ep Try mm >>= eOr
    x Say $ js_withBoundedError (rErr runner) (rMain irunner)
    where
    rMain rn _ = js_renderFragment $ rn $ x ExecW $ xAt @XSelf_ RunR rn $ m
    rErr rn e = js_renderFragment $ rn $ x ExecW $ xAt @XSelf_ RunR rn $ em e
    eOr (Left e) = js_throwBoundedError e
    eOr (Right v) = pure v

type XDomStateReducer a s = Reader { get :: s, act :: a -> X () Unit }

type XDomStateSetter s = Reader { get :: s, set :: s -> X () Unit }

type IdT :: forall k. k -> k
type IdT a = a

type XEl x = Wa ReactEl + XPROPS x
type XProps_ = "xProps"
_xProps = Proxy :: Proxy XProps_

xEl
  :: forall x
   . String
  -> XDom' x XEl
  -> XDom x
xEl s m = do
  (propWFs /\ elBuild) <- xAt @"xProps" RunW $ x ExecW m
  let props = js_propsFromPropWs propWFKey propWFVal propWFs
  x Say $ js_renderEl s (encodeOpts props) elBuild

xDiv :: forall x. XDom' x XEl -> XDom x
xDiv = xEl "div"

xButton :: forall x. XDom' x XEl -> XDom x
xButton = xEl "button"

xText :: forall x. String -> XDom x
xText s = x Say $ js_textEl s

xFragment :: forall x. Array ReactEl -> XDom x
xFragment els = x Say $ js_renderFragment els

foreign import data ReactEl :: Type

foreign import js_textEl :: String -> ReactEl
foreign import js_renderFragment :: Array ReactEl -> ReactEl
foreign import js_renderEl :: String -> Json -> Array ReactEl -> ReactEl
foreign import js_propsFromPropWs
  :: (PropWF -> String) -> (PropWF -> JsAny) -> Array PropWF -> Json

foreign import js_withState
  :: forall s
   . (Unit -> X () Unit)
  -> (s -> (s -> X () Unit) -> Array ReactEl)
  -> s
  -> ReactEl

foreign import js_withKey :: String -> ReactEl -> ReactEl
foreign import js_didMountEl :: (Unit -> Unit) -> ReactEl
foreign import js_withBoundedError
  :: forall e. (e -> ReactEl) -> (Unit -> ReactEl) -> ReactEl

foreign import js_throwBoundedError :: forall e a. e -> a

data PropWF = ClassName String | OnClick (Int -> Unit) | PKey String

propWFKey :: PropWF -> String
propWFKey (ClassName _) = "className"
propWFKey (OnClick _) = "onClick"
propWFKey (PKey _) = "key"

propWFVal :: PropWF -> JsAny
propWFVal (ClassName s) = jsAny s
propWFVal (OnClick s) = jsAny s
propWFVal (PKey s) = jsAny s

xCn :: forall x. String -> XDom' x XPROPS
xCn s = xAt @XProps_ Tell $ pure (ClassName s)

xCnX
  :: forall x
   . ((String -> X (Wa String ()) Unit) -> X (Wa String ()) Unit)
  -> XDom' x XPROPS
xCnX fm = do
  let ss = evalX $ x ExecW $ fm (x Say)
  xAt @XProps_ Tell $ pure (ClassName $ strJoinWith " " ss)

xOnClick :: forall x. (Int -> X () Unit) -> XDom' x XPROPS
xOnClick f = xAt @XProps_ Tell $ pure (OnClick $ \e -> evalX $ f e)

xKey :: forall x. String -> XDom' x XPROPS
xKey s = xAt @XProps_ Tell $ pure (PKey s)

xOnMount :: forall x. X () Unit -> XDom x
xOnMount onMount = x Say $ js_didMountEl (\_ -> evalX onMount)
