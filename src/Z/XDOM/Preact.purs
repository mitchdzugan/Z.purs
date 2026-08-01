module Z.XDOM.Preact where

import Z.Prelude

import Run.Writer as RW

type XSelf' x' = (Run x' (Array ReactEl) -> Array ReactEl)
type XSelf x = (X x (Array ReactEl) -> Array ReactEl)

type XDomFn' x fx a = X (fx (RWa (XSelf x) ReactEl x)) a
type XDomFn x a = XDomFn' x IdT a
type XDom x = XDomFn' x IdT Unit
type XDom' x fx = XDomFn' x fx Unit

type XPROPS x = (xProps :: Writer (Array PropWF) | x)

xRender :: XDom () -> ReactEl
xRender m = js_renderFragment $ xEval $ x RunR (baseR) $ xListen_ $ m
  where
  baseR mm = xEval mm

type XCompX x =
  { render :: X (Wa ReactEl x) Unit -> Array ReactEl
  }

newtype XAdaptF x z = XAdaptF
  ( (X (Wa ReactEl z) Unit -> X (Wa ReactEl x) Unit)
    -> X (Wa ReactEl z) Unit
    -> Array ReactEl
  )

type XAdapt x = Exists (XAdaptF x)

upRunner
  :: forall @p x x' r
   . Cons p (Reader r) x' x
  => IsSymbol p
  => r
  -> XSelf' x'
  -> XSelf' x
upRunner env (fm) = fm <<< xAt @p RunR env

xDKeyed
  :: forall x
   . String
  -> XDom x
  -> XDom x
xDKeyed k m = do
  rn <- x AtR
  x Say $ js_withKey k $ js_renderFragment $ rn $ xListen_ $ x RunR rn $ m

xDNewState
  :: forall x s
   . s
  -> (s -> (s -> X () Unit) -> Run (RWa (XSelf' x) ReactEl x) Unit)
  -> Run (RWa (XSelf' x) ReactEl x) Unit
xDNewState initalState fm = do
  rn <- x AtR
  x Say $ flip (js_withState pure) initalState (renderFn rn)
  where
  renderFn rn s ss = rn $ xListen_ $ x RunR rn $ fm s ss

xDRespondWithAt
  :: forall @p x x' r
   . Cons p (Reader r) x' x
  => IsSymbol p
  => r
  -> Run (RWa (XSelf' x) ReactEl x) Unit
  -> Run (RWa (XSelf' x') ReactEl x') Unit
xDRespondWithAt env m = do
  runner <- x AtR
  let irunner = upRunner @p env runner
  x Say $ js_renderFragment $ irunner $ xListen_ $ x RunR irunner $ m

xDRespondWithNewStateReducerAt
  :: forall @p x' x a s
   . IsSymbol p
  => Cons p (XDomStateReducer a s) x' x
  => s
  -> (s -> a -> s)
  -> Run (RWa (XSelf' x) ReactEl x) Unit
  -> Run (RWa (XSelf' x') ReactEl x') Unit
xDRespondWithNewStateReducerAt initState updateState m = do
  xDNewState initState \state setState -> do
    let act = \a -> setState $ updateState state a
    let env = { act, get: state }
    xDRespondWithAt @p env m

xDRespondWithNewStateSetterAt
  :: forall @p x' x s
   . IsSymbol p
  => Cons p (XDomStateSetter s) x' x
  => s
  -> Run (RWa (XSelf' x) ReactEl x) Unit
  -> Run (RWa (XSelf' x') ReactEl x') Unit
xDRespondWithNewStateSetterAt initState m = do
  xDNewState initState \state setState -> do
    let env = { set: setState, get: state }
    xDRespondWithAt @p env m

xDBoundError
  :: forall x e
   . (e -> Run (RWa (XSelf' x) ReactEl x) Unit)
  -> Run (RWa (XSelf' (E e x)) ReactEl (E e x)) Unit
  -> Run (RWa (XSelf' x) ReactEl x) Unit
xDBoundError em m = do
  runner <- x AtR
  let irunner = \mm -> runner $ xTry mm >>= eOr
  x Say $ js_withBoundedError (renderErr runner) (renderMain irunner)
  where
  renderMain rn _ = js_renderFragment $ rn $ xListen_ $ x RunR rn $ m
  renderErr rn e = js_renderFragment $ rn $ xListen_ $ x RunR rn $ em e
  eOr (Left e) = js_throwBoundedError e
  eOr (Right v) = pure v

type XDomStateReducer a s = Reader { get :: s, act :: a -> X () Unit }

type XDomStateSetter s = Reader { get :: s, set :: s -> X () Unit }

type IdT :: forall k. k -> k
type IdT a = a

type XEl x = Wa ReactEl + XPROPS x
_xProps = Proxy :: Proxy "xProps"

el
  :: forall x
   . String
  -> XDom' x XEl
  -> XDom x
el s m = do
  (propWFs /\ (elBuild /\ _)) <- RW.runWriterAt _xProps $ xListen m
  let props = js_propsFromPropWs propWFKey propWFVal propWFs
  x Say $ js_renderEl s (encodeOpts props) elBuild

div :: forall x. XDom' x XEl -> XDom x
div = el "div"

button :: forall x. XDom' x XEl -> XDom x
button = el "button"

text :: forall x. String -> XDom x
text s = x Say $ js_textEl s

fragment :: forall x. Array ReactEl -> XDom x
fragment els = x Say $ js_renderFragment els

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

cn_ :: forall x. String -> XDom' x XPROPS
cn_ s = RW.tellAt _xProps $ pure (ClassName s)

cn
  :: forall x
   . ((String -> X (Wa String ()) Unit) -> X (Wa String ()) Unit)
  -> XDom' x XPROPS
cn fm = do
  let (ss /\ _) = xEval $ xListen $ fm (x Say)
  RW.tellAt _xProps $ pure (ClassName $ strJoinWith " " ss)

onClick :: forall x. (Int -> X () Unit) -> XDom' x XPROPS
onClick f = RW.tellAt _xProps $ pure (OnClick $ \e -> xEval $ f e)

pkey :: forall x. String -> XDom' x XPROPS
pkey s = RW.tellAt _xProps $ pure (PKey s)

xDOnMount :: forall x. X () Unit -> XDom x
xDOnMount onMount = x Say $ js_didMountEl (\_ -> xEval onMount)