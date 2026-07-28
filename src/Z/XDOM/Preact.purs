module Z.XDOM.Preact where

import Z.Prelude
import Run.Reader as R
import Run.Writer as RW
import Prim.RowList as RL
import Record as Rec

foreign import data ReactEl :: Type

foreign import js_textEl :: String -> ReactEl
foreign import js_renderFragment :: Array ReactEl -> ReactEl
foreign import js_renderEl :: String -> Json -> Array ReactEl -> ReactEl
foreign import js_propsFromPropWs
  :: (PropWF -> String) -> (PropWF -> JsAny) -> Array PropWF -> Json

foreign import js_strict :: ReactEl -> ReactEl

foreign import js_withState
  :: forall s
   . (Unit -> X () Unit)
  -> (s -> (s -> X () Unit) -> Array ReactEl)
  -> s
  -> ReactEl

data PropWF = ClassName String | OnClick (Int -> Unit)

propWFKey :: PropWF -> String
propWFKey (ClassName _) = "className"
propWFKey (OnClick _) = "onClick"

propWFVal :: PropWF -> JsAny
propWFVal (ClassName s) = jsAny s
propWFVal (OnClick s) = jsAny s

cn_ :: forall x. String -> XComp' x XPROPS Unit
cn_ s = RW.tellAt _xProps $ pure (ClassName s)

cn
  :: forall x
   . ((String -> X (Wa String ()) Unit) -> X (Wa String ()) Unit)
  -> XComp' x XPROPS Unit
cn fm = do
  let (ss /\ _) = xEval $ xListen $ fm xSay
  RW.tellAt _xProps $ pure (ClassName $ strJoinWith " " ss)

onClick :: forall x. (Int -> X () Unit) -> XComp' x XPROPS Unit
onClick f = RW.tellAt _xProps $ pure (OnClick $ \e -> xEval $ f e)

type XSelf x = X (Wa ReactEl x) Unit -> Array ReactEl

type XSELF x = (xSelf :: R.Reader (XSelf x) | x)

type XPROPS x = (xProps :: RW.Writer (Array PropWF) | x)

_xSelf = Proxy :: Proxy "xSelf"
_xProps = Proxy :: Proxy "xProps"

xBuild
  :: forall x
   . X (Wa ReactEl + (XSELF x)) Unit
  -> X ((XSELF x)) (Array ReactEl)
xBuild m = xListen m <#> fst

xRender :: X (Wa ReactEl + XSELF ()) Unit -> ReactEl
xRender m = js_renderFragment $ xEval $ R.runReaderAt _xSelf baseR $ xBuild m
  where
  baseR = fst <<< xEval <<< xListen

xRenderStrict :: X (Wa ReactEl + XSELF ()) Unit -> ReactEl
xRenderStrict = js_strict <<< xRender

type XComp x a = X (Wa ReactEl (XSELF x)) a
type XComp' x f a = X (f (Wa ReactEl (XSELF x))) a

type XEl x = Wa ReactEl + XPROPS x

el
  :: forall x
   . String
  -> XComp' x XEl Unit
  -> XComp x Unit
el s m = do
  (propWFs /\ (elBuild /\ _)) <- RW.runWriterAt _xProps $ xListen m
  let props = js_propsFromPropWs propWFKey propWFVal propWFs
  xSay $ js_renderEl s (jsonRmNils $ encodeJson props) elBuild

div :: forall x. XComp' x (XEl) Unit -> XComp x Unit
div = el "div"

button :: forall x. XComp' x (XEl) Unit -> XComp x Unit
button = el "button"

text :: forall x. String -> XComp x Unit
text s = xSay $ js_textEl s

fragment :: forall x. Array ReactEl -> XComp x Unit
fragment els = xSay $ js_renderFragment els

withState
  :: forall x s
   . s
  -> (s -> (s -> X () Unit) -> XComp x Unit)
  -> XComp x Unit
withState initalState fm = do
  runner <- R.askAt _xSelf
  xSay $ flip (js_withState pure) initalState $ \state setState -> runner
    $ R.runReaderAt _xSelf runner
    $ fm state setState

withEnv :: forall x r. r -> XComp (R r x) Unit -> XComp x Unit
withEnv env m = do
  runner <- R.askAt _xSelf
  let irunner = \mm -> runner $ xEvalR env mm
  fragment $ runner $ R.runReaderAt _xSelf irunner $ xEvalR env m

modEnv
  :: forall x r1 r2
   . (r1 -> r2)
  -> XComp (R r2 + R r1 x) Unit
  -> XComp (R r1 x) Unit
modEnv fenv m = do
  prevEnv <- xAsk
  runner <- R.askAt _xSelf
  let env = fenv prevEnv
  let irunner = \mm -> runner $ xEvalR env mm
  fragment $ runner $ R.runReaderAt _xSelf irunner $ xEvalR env m

type Reducer p a s r = RL.Cons p { state :: s, act :: a -> X () Unit } r
type ReducerR a s = { state :: s, act :: a -> X () Unit }

withReducer
  :: forall x r' r @p a s
   . IsSymbol p
  => Cons p (ReducerR a s) r' r
  => Lacks p r'
  => s
  -> (s -> a -> s)
  -> XComp (R { | r } + R { | r' } x) Unit
  -> XComp (R { | r' } x) Unit
withReducer initState updateState m = do
  withState initState \state setState -> do
    let act = \a -> setState $ updateState state a
    modEnv (Rec.insert (Proxy :: Proxy p) { act, state }) m