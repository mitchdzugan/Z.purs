module Z.XDom.Core
  ( (%%)
  , (*%%)
  , ATTR
  , ATTREL
  , DomA
  , DomATag
  , DomE
  , DomETag
  , DomEffPermit
  , DomPermit
  , EL
  , MDOMEFF
  , MDOMEL
  , MDom
  , MDomEff
  , MDomEl
  , MDomEl2
  , R'Self
  , SELF
  , Self
  , XDom
  , XDom'Ref
  , XDom'RefReducer
  , XDom'State
  , XDom'StateReducer
  , XDomA
  , XDomE
  , XDomEA
  , class Dom'ConsSymbol
  , dom'a
  , dom'article
  , dom'br
  , dom'button
  , dom'div
  , dom'do
  , dom'element
  , dom'iframe
  , dom'img
  , dom'input
  , dom'label
  , dom'li
  , dom'path
  , dom'pre
  , dom'span
  , dom'svg
  , dom'text
  , dom'ul
  , dom'withAdapter
  , dom'withKey
  , dom'withNewRef
  , dom'withNewState
  , domEff'async
  , domEff'do
  , domEff'every
  , domEff'every'lag
  , domEff'every'lead
  , domEff'getRunner
  , domEff'getSelf
  , domEff'onNew
  , domEff'onNew'lag
  , domEff'onNew'lead
  , domEff'once
  , domEff'once'lag
  , domEff'once'lead
  , domError'bind
  , domError'bind''
  , domError'fail
  , domError'fail''
  , domR'run
  , domR'run''
  , domRef'dispatch
  , domRef'dispatch''
  , domRef'get
  , domRef'get''
  , domRef'run
  , domRef'run''
  , domRef'runReducer
  , domRef'runReducer''
  , domRef'set
  , domRef'set''
  , domState'dispatch
  , domState'dispatch''
  , domState'get
  , domState'get''
  , domState'run
  , domState'run''
  , domState'runReducer
  , domState'runReducer''
  , domState'set
  , domState'set''
  , domx'eval
  , domx'eval_
  , el'checked
  , el'cn
  , el'cnW
  , el'd
  , el'data
  , el'fill
  , el'href
  , el'id
  , el'key
  , el'onClick
  , el'renderToString
  , el'role
  , el'roleButton
  , el'src
  , el'style
  , el'tabIndex
  , el'tabIndex0
  , el'target
  , el'targetBlank
  , el'type
  , el'viewBox
  , el'xmlns
  , exec'xdom
  , op'el'text
  , op'mdom'andText
  , x'renderToStringAsync
  ) where

import Z.Prelude

import Effect.Unsafe (unsafePerformEffect)
import Web.Event.Internal.Types as WET
import Z.XDom.Preact (js_renderToString, js_renderToStringAsync)
import Z.XDom.Preact as D

data Self dx x = Self (Runner dx) (Runner x)
type EffSelf x = Self () x

type R'Self dx x = X'R (Self dx x)

type SELF dx x = (self :: R'Self dx x | x)

eval'self :: forall dx x a. Self dx x -> Run (self :: R'Self dx x | x) a -> a
eval'self self@(Self _ rn) m = runner'eval rn $ r'run'' @"self" self $ m

evalA'self
  :: forall dx x a
   . Self dx x
  -> Run (A' (SELF dx x)) a
  -> Future a
evalA'self self@(Self _ rn) m = runner'evalA rn $ r'run'' @"self" self $ m

selfExtend :: forall dr x' x. RunMW x x' -> Self dr x' -> Self dr x
selfExtend fm (Self drn rn) = Self drn (runner'extend fm rn)

selfExtendDom :: forall dx' dx x. RunMW dx dx' -> Self dx' x -> Self dx x
selfExtendDom fm (Self drn r) = Self (runner'extend fm drn) r

selfDomless :: forall dx x. Self dx x -> Self () x
selfDomless (Self _ r) = Self runner'_ r

type DomEffPermit x = (domEff :: X'Permit | x)

type DomPermit x = (dom :: X'Permit | x)

type MDom dx x a = Run (Wa' D.ReactEl (SELF dx x)) a

type ATTR x = (attr :: X'W $ Array D.PropWF | x)
type EL x = Wa' D.ReactEl x
type ATTREL x = ATTR (EL x)
type MDOMEFF x = (self :: R'Self () (DomEffPermit x) | DomEffPermit x)
type MDOMEFF'delta x' x = (self :: R'Self () (DomEffPermit x) | DomEffPermit x')

type MDomEff x a = Run (MDOMEFF x) a
type MDomEff'delta x' x a = Run (MDOMEFF'delta x' x) a
type MDomEff'a x a = Run (A' (MDOMEFF x)) a

type MDOMEL dx x = ATTR $ Wa' D.ReactEl (self :: R'Self dx x | x)
type MDomEl dx x a = Run (MDOMEL dx x) a
type MDomEl2 dx x a = Run (Wa' D.ReactEl $ MDOMEL dx x) a

foreign import data DomETag :: Type
foreign import data DomATag :: Type

type DomE e = X'R $ Proxy $ DomETag /\ e
type DomA = X'R $ Proxy DomATag

--------------------------------------------------------------------------------

renderM
  :: forall dr x. Self dr x -> MDom dr x Unit -> Array D.ReactEl
renderM self m = eval'self self $ w'exec m

renderMEl :: forall dr x. Self dr x -> MDom dr x Unit -> D.ReactEl
renderMEl self = D.js_renderFragment <<< renderM self

domRenderM :: forall dr x. MDom dr x Unit -> MDom dr x (Array D.ReactEl)
domRenderM m = x'extract @"self" <#> flip renderM m

domRenderMEl :: forall dr x. MDom dr x Unit -> MDom dr x D.ReactEl
domRenderMEl m = domRenderM m <#> D.js_renderFragment

--------------------------------------------------------------------------------

dom'withKey :: forall dr x. String -> MDom dr x Unit -> MDom dr x Unit
dom'withKey k m = domRenderMEl m >>= w'say <<< D.js_withKey k

dom'withNewRef
  :: forall sx de s
   . s
  -> (Eff'At "dom" s -> (s -> Eff'At "dom" Unit) -> MDom sx de Unit)
  -> MDom sx de Unit
dom'withNewRef initalState fm = do
  self <- x'extract @"self"
  w'say $ D.js_withRef (renderFn self) initalState
  where
  renderFn self s ss = renderM self $ fm (eff'tag @"dom" s) (w ss)
  w ss s = eff'tag @"dom" $ ss s

dom'withNewState
  :: forall sx de s
   . s
  -> (s -> (s -> Eff'At "domEff" Unit) -> MDom sx de Unit)
  -> MDom sx de Unit
dom'withNewState initalState fm = do
  self <- x'extract @"self"
  w'say $ D.js_withState (renderFn self) initalState
  where
  renderFn self s ss = renderM self $ fm s (w ss)
  w ss s = eff'tag @"domEff" $ ss s

dom'withAdapter
  :: forall dr x' x ret
   . (forall a. Run x a -> Run x' a)
  -> MDom dr x ret
  -> MDom dr x' ret
dom'withAdapter fm m = do
  self <- x'extract @"self" <#> selfExtend fm
  let res /\ els = eval'self self $ w'run m
  w'tell els
  pure res

domEff'getSelf :: forall x. MDomEff x (Self () (DomEffPermit x))
domEff'getSelf = x'extract @"self"

domEff'getRunner :: forall x. MDomEff x (Runner (MDOMEFF x))
domEff'getRunner = domEff'getSelf <#> \s -> runner'mk (pure <<< eval'self s)

domEff'do :: forall x a. Eff'At "domEff" a -> Run (DomEffPermit x) a
domEff'do = x'do

dom'do :: forall dx x a. Eff'At "dom" a -> Run (SELF dx x) a
dom'do = x'permit @"dom" <<< x'do

dom'getEffSelf :: forall dr x. MDom dr x (Self () (DomEffPermit x))
dom'getEffSelf =
  x'extract @"self" <#> selfDomless <#> selfExtend (x'permit @"domEff")

el'getEffSelf :: forall dr x. MDomEl dr x (Self () (DomEffPermit x))
el'getEffSelf =
  x'extract @"self" <#> selfDomless <#> selfExtend (x'permit @"domEff")

--------------------------------------------------------------------------------

class
  ( ConsSymbol p m x' x
  , ConsSymbol p m (MDOMEFF'delta x' x) (MDOMEFF x)
  , ConsSymbol p m (MDOMEFF'delta (A' x') x) (A' (MDOMEFF x))
  ) <=
  Dom'ConsSymbol p m x' x

instance
  ( ConsSymbol p m x' x
  , ConsSymbol p m (MDOMEFF'delta x' x) (MDOMEFF x)
  , ConsSymbol p m (MDOMEFF'delta (A' x') x) (A' (MDOMEFF x))
  ) =>
  Dom'ConsSymbol p m x' x

--------------------------------------------------------------------------------

type T'domR'run p =
  forall r x' x dr ret
   . ConsSymbol p (X'R r) x' x
  => r
  -> MDom dr x ret
  -> MDom dr x' ret

domR'run'' :: forall @p. T'domR'run p
domR'run'' env m = dom'withAdapter (r'run'' @p env) m

domR'run :: forall p. T'use'r'AsSym p T'domR'run
domR'run = domR'run'' @p

domx'eval
  :: forall @p @m dr x' x param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => param
  -> MDom dr x a
  -> MDom dr x' a
domx'eval init m = dom'withAdapter (x'eval @p init) m

domx'eval_
  :: forall @p @m dr x' x param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => Generable param GDefault param
  => MDom dr x a
  -> MDom dr x' a
domx'eval_ m = dom'withAdapter (x'eval @p default) m

--------------------------------------------------------------------------------

type T'domError'bind e p =
  forall dx' dx x
   . ConsSymbol p (DomE e) dx' dx
  => (->) e $ MDom dx' x Unit
  -> MDom dx x Unit
  -> MDom dx' x Unit

domError'bind'' :: forall @p e. T'domError'bind e p
domError'bind'' em m = do
  oldSelf <- x'extract @"self"
  let self = selfExtendDom (r'run'' @p (Proxy @(DomETag /\ e))) oldSelf
  w'say $ D.js_withBoundedError (reflectSymbol $ p @p)
    (\e -> eff'defer \_ -> renderMEl oldSelf $ em e)
    (eff'defer \_ -> renderMEl self m)

domError'bind :: forall e p. T'use'e'AsSym p $ T'domError'bind e
domError'bind = domError'bind'' @p

type T'domError'fail p =
  forall e dx' dx x xx a
   . ConsSymbol p (DomE e) dx' dx
  => e
  -> Run (self :: R'Self dx x | xx) a

domError'fail'' :: forall @p. T'domError'fail p
domError'fail'' e = x'extract @"self" <#> \_ ->
  D.js_throwBoundedError (reflectSymbol $ Proxy @p) e

domError'fail :: forall p. T'use'e'AsSym p $ T'domError'fail
domError'fail = domError'fail'' @p

--------------------------------------------------------------------------------

type StateReducerR s a r =
  (get :: s, update :: a -> Eff'At "domEff" Unit | r)

type T'domState'runable s a tf p =
  forall dr x' x
   . ConsSymbol p (X'R $ Record $ StateReducerR s a ()) x' x
  => (tf (MDom dr x Unit -> MDom dr x' Unit))

type Tf'reducer s a res = s -> (s -> a -> s) -> res
type T'domState'runReducer p =
  forall s a. T'domState'runable s a (Tf'reducer s a) p

domState'runReducer'' :: forall @p. T'domState'runReducer p
domState'runReducer'' inits ups m = dom'withNewState inits \s sets -> do
  domR'run'' @p { get: s, update: sets <<< ups s } m

domState'runReducer :: forall @p. T'useAsSym "state" p T'domState'runReducer
domState'runReducer = domState'runReducer'' @p

type Tf'state s res = s -> res
type T'domState'run p = forall s. T'domState'runable s s (Tf'state s) p

domState'run'' :: forall @p. T'domState'run p
domState'run'' inits m = dom'withNewState inits \s sets -> do
  domR'run'' @p { get: s, update: sets } m

domState'run :: forall @p. T'use's'AsSym p T'domState'run
domState'run = domState'run'' @p

type T'domState'get p =
  forall s x' x r
   . IsSymbol p
  => Cons p (X'R $ Record (get :: s | r)) x' x
  => Run x s

domState'get'' :: forall @p. T'domState'get p
domState'get'' = x'extract @p <#> _.get

domState'get :: forall p. T'use's'AsSym p T'domState'get
domState'get = domState'get'' @p

type T'domState'setable s a p =
  forall x' x r
   . ConsSymbol p (X'R $ Record $ StateReducerR s a r) (domEff :: X'Permit | x')
       (domEff :: X'Permit | x)
  => a
  -> Run (domEff :: X'Permit | x) Unit

type T'domState'dispatch p = forall s a. T'domState'setable s a p

domState'dispatch'' :: forall @p. T'domState'dispatch p
domState'dispatch'' a = x'extract @p >>= \r -> x'do (r.update a)

domState'dispatch :: forall p. T'use's'AsSym p T'domState'dispatch
domState'dispatch = domState'dispatch'' @p

type T'domState'set p = forall s. T'domState'setable s s p

domState'set'' :: forall @p. T'domState'set p
domState'set'' = domState'dispatch'' @p

domState'set :: forall p. T'use's'AsSym p T'domState'set
domState'set = domState'set'' @p

----------------------------

domUpdateRefState
  :: forall a s
   . Eff'At "dom" s
  -> (s -> Eff'At "dom" Unit)
  -> (s -> a -> s)
  -> a
  -> Eff'At "dom" Unit
domUpdateRefState get sets update act = eff'tag @"dom" do
  s <- eff'untag @"dom" get
  eff'untag @"dom" $ sets $ update s act

type RefReducerR s a r =
  (get :: Eff'At "dom" s, update :: a -> Eff'At "dom" Unit | r)

type T'domRef'runable s a tf p =
  forall dr x' x
   . ConsSymbol p (X'R $ Record $ RefReducerR s a ()) x' x
  => (tf (MDom dr x Unit -> MDom dr x' Unit))

type T'domRef'runReducer p =
  forall s a. T'domRef'runable s a (Tf'reducer s a) p

domRef'runReducer'' :: forall @p. T'domRef'runReducer p
domRef'runReducer'' inits ups m = dom'withNewRef inits \get sets -> do
  domR'run'' @p { get, update: domUpdateRefState get sets ups } m

domRef'runReducer :: forall @p. T'useAsSym "state" p T'domRef'runReducer
domRef'runReducer = domRef'runReducer'' @p

type T'domRef'run p = forall s. T'domRef'runable s s (Tf'state s) p

domRef'run'' :: forall @p. T'domRef'run p
domRef'run'' inits m = dom'withNewRef inits \s sets -> do
  domR'run'' @p { get: s, update: sets } m

domRef'run :: forall @p. T'use's'AsSym p T'domRef'run
domRef'run = domRef'run'' @p

type T'domRef'get p =
  forall s x' x r
   . IsSymbol p
  => Cons p (X'R $ Record (get :: Eff'At "dom" s | r)) x' x
  => Run x s

domRef'get'' :: forall @p. T'domRef'get p
domRef'get'' = (x'extract @p <#> _.get) >>= x'permit @"dom" <<< x'do

domRef'get :: forall p. T'use's'AsSym p T'domRef'get
domRef'get = domRef'get'' @p

type T'domRef'setable s a p =
  forall x' x r
   . ConsSymbol p (X'R $ Record $ RefReducerR s a r) x' x
  => a
  -> Run x Unit

type T'domRef'dispatch p = forall s a. T'domRef'setable s a p

domRef'dispatch'' :: forall @p. T'domRef'dispatch p
domRef'dispatch'' a = x'extract @p >>= \r ->
  x'permit @"dom" $ x'do (r.update a)

domRef'dispatch :: forall p. T'use's'AsSym p T'domRef'dispatch
domRef'dispatch = domRef'dispatch'' @p

type T'domRef'set p = forall s. T'domRef'setable s s p

domRef'set'' :: forall @p. T'domRef'set p
domRef'set'' = domRef'dispatch'' @p

domRef'set :: forall p. T'use's'AsSym p T'domRef'set
domRef'set = domRef'set'' @p

------------------------------------------------------------------------

type MDomEff_D x = MDomEff x (MDomEff x Unit)

type MDomEffA_D x = MDomEff'a x (MDomEff'a x Unit)

-------------------------

domEff'onNew :: forall a dx x. Eq a => a -> MDomEff_D x -> MDom dx x Unit
domEff'onNew comp on = do
  self <- dom'getEffSelf
  w'say $ D.js_effComponent eq comp do
    r' <- eff'defer \_ -> eval'self self on
    pure $ eff'defer \_ -> eval'self self r'

domEff'once :: forall dx x. MDomEff_D x -> MDom dx x Unit
domEff'once = domEff'onNew unit

domEff'every :: forall dx x. MDomEff_D x -> MDom dx x Unit
domEff'every = domEff'onNew antiUnit

domEff'onNew'lead
  :: forall a dx x. Eq a => a -> MDomEff x Unit -> MDom dx x Unit
domEff'onNew'lead a m = domEff'onNew a $ m *> pure (pure unit)

domEff'once'lead :: forall dx x. MDomEff x Unit -> MDom dx x Unit
domEff'once'lead m = domEff'onNew unit $ m *> pure (pure unit)

domEff'every'lead :: forall dx x. MDomEff x Unit -> MDom dx x Unit
domEff'every'lead m = domEff'onNew antiUnit $ m *> pure (pure unit)

domEff'onNew'lag :: forall a dx x. Eq a => a -> MDomEff x Unit -> MDom dx x Unit
domEff'onNew'lag a m = domEff'onNew a $ pure m

domEff'once'lag :: forall dx x. MDomEff x Unit -> MDom dx x Unit
domEff'once'lag m = domEff'onNew unit $ pure m

domEff'every'lag :: forall dx x. MDomEff x Unit -> MDom dx x Unit
domEff'every'lag m = domEff'onNew antiUnit $ pure m

-------------------------

domEff'async :: forall x. MDomEff'a x Unit -> MDomEff x Unit
domEff'async m = do
  self <- domEff'getSelf
  pure $ unsafePerformEffect $ future'run_ $ evalA'self self m

------------------------------------------------------------------------

dom'text :: forall t dr x. SText t => t -> MDom dr x Unit
dom'text = w'say <<< D.js_textEl <<< stext

el'key :: forall dr x. String -> MDomEl dr x Unit
el'key = w'tell'' @"attr" <<< pure <<< D.PKey

el'id :: forall dr x. String -> MDomEl dr x Unit
el'id = w'tell'' @"attr" <<< pure <<< D.Id

el'type :: forall dr x. String -> MDomEl dr x Unit
el'type = w'tell'' @"attr" <<< pure <<< D.Type

el'checked :: forall dr x. Boolean -> MDomEl dr x Unit
el'checked = w'tell'' @"attr" <<< pure <<< D.Checked

el'tabIndex :: forall dr x. Int -> MDomEl dr x Unit
el'tabIndex = w'tell'' @"attr" <<< pure <<< D.TabIndex

el'tabIndex0 :: forall dr x. MDomEl dr x Unit
el'tabIndex0 = el'tabIndex 0

el'role :: forall dr x. String -> MDomEl dr x Unit
el'role = w'tell'' @"attr" <<< pure <<< D.Role

el'roleButton :: forall dr x. MDomEl dr x Unit
el'roleButton = el'role "button"

el'target :: forall dr x. String -> MDomEl dr x Unit
el'target = w'tell'' @"attr" <<< pure <<< D.Target

el'targetBlank :: forall dr x. MDomEl dr x Unit
el'targetBlank = el'target "_blank"

el'href :: forall dr x. String -> MDomEl dr x Unit
el'href = w'tell'' @"attr" <<< pure <<< D.Href

el'src :: forall dr x. String -> MDomEl dr x Unit
el'src = w'tell'' @"attr" <<< pure <<< D.Src

el'xmlns :: forall dr x. String -> MDomEl dr x Unit
el'xmlns = w'tell'' @"attr" <<< pure <<< D.XmlNS

el'viewBox :: forall dr x. String -> MDomEl dr x Unit
el'viewBox = w'tell'' @"attr" <<< pure <<< D.ViewBox

el'fill :: forall dr x. String -> MDomEl dr x Unit
el'fill = w'tell'' @"attr" <<< pure <<< D.Fill

el'd :: forall dr x. String -> MDomEl dr x Unit
el'd = w'tell'' @"attr" <<< pure <<< D.D

el'cn :: forall dr x. String -> MDomEl dr x Unit
el'cn = w'tell'' @"attr" <<< pure <<< D.ClassName

el'data :: forall dr x. String -> String -> MDomEl dr x Unit
el'data k = w'tell'' @"attr" <<< pure <<< D.Data k

el'style :: forall dx x. D.ElStyles -> MDomEl dx x Unit
el'style = w'tell'' @"attr" <<< pure <<< D.Style

el'cnW :: forall dr x. ((String -> StrW) -> StrW) -> MDomEl dr x Unit
el'cnW fm = el'cn $ w'str'sp fm

el'onClick :: forall dr x. (WET.Event -> MDomEff x Unit) -> MDomEl dr x Unit
el'onClick f = do
  self <- el'getEffSelf
  w'tell'' @"attr" $ pure $ D.OnClick $ \e -> eval'self self $ f e

type XDom_domElement_ =
  forall dr x. Run (Wa' D.ReactEl (MDOMEL dr x)) Unit -> MDom dr x Unit

dom'element :: String -> XDom_domElement_
dom'element s m = do
  (elBuild /\ propWFs) <- x'run_ @"attr" $ w'exec m
  let props = D.js_propsFromPropWs D.propWFKey D.propWFVal propWFs
  w'say $ D.js_renderEl s (encodeOpts props) elBuild

type XDom_domElement_NoCh = forall dr x. ATTR () @@> Unit -> MDom dr x Unit

dom'element_noCh :: String -> XDom_domElement_NoCh
dom'element_noCh s m = do
  let propWFs = sync'x $ x'exec_ @"attr" m
  let props = D.js_propsFromPropWs D.propWFKey D.propWFVal propWFs
  w'say $ D.js_renderEl s (encodeOpts props) []

type XDom dr x a = MDom dr (X'Base x) a
type XDomA dx x a = XDom (_'x'async :: DomA | dx) x a
type XDomE e dx x a = XDom (_'x'except :: (DomE e) | dx) x a
type XDomEA e dx x a = XDom (_'x'except :: (DomE e), _'x'async :: DomA | dx) x a

type XDom_ dr x = T'_ $ XDom dr x
type XDomA_ dr x = T'_ $ XDom dr x
type XDomE_ e dr x = T'_ $ XDomE e dr x
type XDomEA_ e dr x = T'_ $ XDomEA e dr x

baseSelf :: Self () ()
baseSelf = Self runner'_ runner'_

exec'xdom :: XDom_ () () -> D.ReactEl
exec'xdom = renderMEl $ selfDomless $ selfExtend (x'eval_ @"_'x'base") baseSelf

el'renderToString :: D.ReactEl -> String
el'renderToString = js_renderToString

x'renderToStringAsync :: forall x. D.ReactEl -> Run (EA' JsError x) String
x'renderToStringAsync = e'runEffPromise <<< js_renderToStringAsync

dom'div :: XDom_domElement_
dom'div = dom'element "div"

dom'a :: XDom_domElement_
dom'a = dom'element "a"

dom'button :: XDom_domElement_
dom'button = dom'element "button"

dom'label :: XDom_domElement_
dom'label = dom'element "label"

dom'input :: XDom_domElement_
dom'input = dom'element "input"

dom'pre :: XDom_domElement_
dom'pre = dom'element "pre"

dom'span :: XDom_domElement_
dom'span = dom'element "span"

dom'article :: XDom_domElement_
dom'article = dom'element "article"

dom'iframe :: XDom_domElement_
dom'iframe = dom'element "iframe"

dom'img :: XDom_domElement_
dom'img = dom'element "img"

dom'svg :: XDom_domElement_
dom'svg = dom'element "svg"

dom'ul :: XDom_domElement_
dom'ul = dom'element "ul"

dom'li :: XDom_domElement_
dom'li = dom'element "li"

{-
------------------------------------------------------------------
TODO all below here should be NoCh
TODO allow NoCh to have element properties
-}

dom'br :: XDom_domElement_NoCh
dom'br = dom'element_noCh "br"

dom'path :: XDom_domElement_
dom'path = dom'element "path"

------------------------------------------------------------------

op'el'text :: forall t dr x. SText t => XDom_domElement_ -> t -> MDom dr x Unit
op'el'text el t = el $ dom'text t

op'mdom'andText
  :: forall t dr x. SText t => MDomEl dr x Unit -> t -> MDomEl dr x Unit
op'mdom'andText m t = m *> dom'text t

infixr 0 op'el'text as %%
infixr 0 op'mdom'andText as *%%

type XDom'StateReducer a s =
  X'R { get :: s, update :: a -> Eff'At "domEff" Unit }

type XDom'State s = XDom'StateReducer s s

type XDom'RefReducer a s =
  X'R { get :: Eff'At "dom" s, update :: a -> Eff'At "dom" Unit }

type XDom'Ref s = XDom'RefReducer s s
