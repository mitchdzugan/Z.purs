module Z.XDom.Core3
  ( (%%)
  , (*%%)
  , ATTRW
  , ATTRW'Childless
  , DomA
  , DomATag
  , DomE
  , DomETag
  , DomEffPermit
  , DomPermit
  , MDOMEFF
  , MDom
  , MDomEff
  , MDomEl
  , MDomEl'g
  , R'Self
  , SELF
  , SELF'x
  , Self(..)
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
  , dom'element'Childless
  , dom'getRunner
  , dom'getSelf
  , dom'iframe
  , dom'img
  , dom'input
  , dom'label
  , dom'li
  , dom'path
  , dom'pre
  , dom'runReader
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
  , el'addChildren
  , el'addCn
  , el'addStyle
  , el'addStyles
  , el'checked
  , el'cn
  , el'd
  , el'data
  , el'fill
  , el'href
  , el'id
  , el'key
  , el'onClick
  , el'renderToString
  , el'resetChildren
  , el'resetCn
  , el'resetStyles
  , el'role
  , el'roleButton
  , el'setChildren
  , el'setStyles
  , el'src
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

import Data.Traversable (class Traversable)
import Effect.Unsafe (unsafePerformEffect)
import Z.XDom.Preact
  ( DomEvent
  , PropWF(..)
  , ReactEl
  , js_effComponent
  , js_renderEl
  , js_renderFragment
  , js_renderToString
  , js_renderToStringAsync
  , js_textEl
  , js_throwBoundedError
  , js_withBoundedError
  , js_withKey
  , js_withRef
  , js_withState
  )
import Z.XDom.X
  ( AttrW'
  , DomW'
  , attrW'addAttr
  , attrW'addChildren
  , attrW'addClassname
  , attrW'addStyle
  , attrW'exec
  , attrW'resetChildren
  , attrW'resetClassnames
  , attrW'resetStyles
  , domW'concat
  , domW'exec
  , domW'push
  , domW'run
  )

data Self dx x = Self (Runner dx) (Runner x)

type R'Self dx x = X'R (Self dx x)

type SELF'x dx x x' = (self :: R'Self dx x | x')

type SELF dx x = SELF'x dx x x

type EFFSELF x = SELF () x

eval'self :: forall dx x a. Self dx x -> Run (self :: R'Self dx x | x) a -> a
eval'self self@(Self _ rn) m = runner'eval rn $ r'run'' @"self" self $ m

evalA'self
  :: forall dx x a
   . Self dx x
  -> Run (A' (SELF dx x)) a
  -> Future a
evalA'self self@(Self _ rn) m = runner'evalA rn $ r'run'' @"self" self $ m

selfExtend :: forall dx x' x. RunMW x x' -> Self dx x' -> Self dx x
selfExtend fm (Self dxn xn) = Self dxn (runner'extend fm xn)

selfExtendDom :: forall dx' dx x. RunMW dx dx' -> Self dx' x -> Self dx x
selfExtendDom fm (Self drn r) = Self (runner'extend fm drn) r

selfDomless :: forall dx x. Self dx x -> Self () x
selfDomless (Self _ r) = Self runner'_ r

type DomEffPermit x = (domEff :: X'Permit | x)

type DomPermit x = (dom :: X'Permit | x)

type MDom dx x a = Run (DomW' (SELF dx x)) a

type MDomEl'g t dx x a = Run (AttrW' t (SELF dx x)) a

type MDomEl dx x a = MDomEl'g (Array ReactEl) dx x a

type MDomEl'Childless dx x a = MDomEl'g Void dx x a

type MDOMEFF x = (self :: R'Self () (DomEffPermit x) | DomEffPermit x)

type MDOMEFF'delta x' x = (self :: R'Self () (DomEffPermit x) | DomEffPermit x')

type MDomEff x a = Run (MDOMEFF x) a

type MDomEff'delta x' x a = Run (MDOMEFF'delta x' x) a

type MDomEff'a x a = Run (A' (MDOMEFF x)) a

--------------------------------------------------------------------------------

renderM
  :: forall dx x. Self dx x -> MDom dx x Unit -> Array ReactEl
renderM self m = eval'self self $ domW'exec m

renderMEl :: forall dx x. Self dx x -> MDom dx x Unit -> ReactEl
renderMEl self = js_renderFragment <<< renderM self

domRenderM :: forall dx x. MDom dx x Unit -> MDom dx x (Array ReactEl)
domRenderM m = x'extract @"self" <#> flip renderM m

domRenderMEl :: forall dx x. MDom dx x Unit -> MDom dx x ReactEl
domRenderMEl m = domRenderM m <#> js_renderFragment

--------------------------------------------------------------------------------

domEff'do :: forall x a. Eff'At "domEff" a -> Run (DomEffPermit x) a
domEff'do = x'do

dom'do :: forall x a. Eff'At "dom" a -> Run (DomPermit x) a
dom'do = x'do

dom'getSelf :: forall dx x x'. Run (SELF'x dx x x') (Self dx x)
dom'getSelf = x'extract @"self"

dom'getRunner :: forall dx x x'. Run (SELF'x dx x x') (Runner (SELF dx x))
dom'getRunner = x'extract @"self" <#> \s -> runner'mk (pure <<< eval'self s)

dom'getEffSelf
  :: forall dx x x'. Run (SELF'x dx x x') (Self () (DomEffPermit x))
dom'getEffSelf =
  dom'getSelf <#> selfDomless <#> selfExtend (x'permit @"domEff")

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

dom'withKey :: forall dx x. String -> MDom dx x Unit -> MDom dx x Unit
dom'withKey k m = domRenderMEl m >>= domW'push <<< js_withKey k

dom'withNewRef
  :: forall dx x s
   . s
  -> (Eff'At "dom" s -> (s -> Eff'At "dom" Unit) -> MDom dx x Unit)
  -> MDom dx x Unit
dom'withNewRef initalState fm = do
  self <- x'extract @"self"
  domW'push $ js_withRef (renderFn self) initalState
  where
  renderFn self s ss = renderM self $ fm (eff'tag @"dom" s) (w ss)
  w ss s = eff'tag @"dom" $ ss s

dom'withNewState
  :: forall dx x s
   . s
  -> (s -> (s -> Eff'At "domEff" Unit) -> MDom dx x Unit)
  -> MDom dx x Unit
dom'withNewState initalState fm = do
  self <- x'extract @"self"
  domW'push $ js_withState (renderFn self) initalState
  where
  renderFn self s ss = renderM self $ fm s (w ss)
  w ss s = eff'tag @"domEff" $ ss s

dom'withAdapter
  :: forall dx x' x ret
   . (forall a. Run x a -> Run x' a)
  -> MDom dx x ret
  -> MDom dx x' ret
dom'withAdapter fm m = do
  self <- x'extract @"self" <#> selfExtend fm
  let res /\ els = eval'self self $ domW'run m
  domW'concat els
  pure res

domx'eval
  :: forall @p @m dx x' x param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => param
  -> MDom dx x a
  -> MDom dx x' a
domx'eval init m = dom'withAdapter (x'eval @p init) m

domx'eval_
  :: forall @p @m dx x' x param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => Generable param GDefault param
  => MDom dx x a
  -> MDom dx x' a
domx'eval_ m = dom'withAdapter (x'eval @p default) m

type T'dom'runReader p =
  forall r x' x dx ret
   . ConsSymbol p (X'R r) x' x
  => r
  -> MDom dx x ret
  -> MDom dx x' ret

dom'runReader :: forall @p. T'dom'runReader p
dom'runReader env m = dom'withAdapter (r'run'' @p env) m

type ATTRW x = AttrW' (Array ReactEl) x

type MDom_domElement = forall dx x. MDomEl dx x Unit -> MDom dx x Unit

dom'element :: String -> MDom_domElement
dom'element s mattr = do
  self <- x'extract @"self"
  let { attrs, children } = eval'self self $ attrW'exec identity mattr
  domW'push $ js_renderEl s attrs children

type ATTRW'Childless x = AttrW' Void x

type MDom_domElement'Childless =
  forall dx x. MDomEl'Childless dx x Unit -> MDom dx x Unit

dom'element'Childless :: String -> MDom_domElement'Childless
dom'element'Childless s mattr = do
  self <- x'extract @"self"
  let { attrs, children } = eval'self self $ attrW'exec (\_ -> []) mattr
  domW'push $ js_renderEl s attrs children

--------------------------------------------------------------------------------

foreign import data DomETag :: Type
foreign import data DomATag :: Type

type DomE e = X'R $ Proxy $ DomETag /\ e
type DomA = X'R $ Proxy DomATag

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
  domW'push $ js_withBoundedError (reflectSymbol $ p @p)
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
  js_throwBoundedError (reflectSymbol $ Proxy @p) e

domError'fail :: forall p. T'use'e'AsSym p $ T'domError'fail
domError'fail = domError'fail'' @p

--------------------------------------------------------------------------------

type StateReducerR s a r =
  (get :: s, update :: a -> Eff'At "domEff" Unit | r)

type T'domState'runable s a tf p =
  forall dx x' x
   . ConsSymbol p (X'R $ Record $ StateReducerR s a ()) x' x
  => (tf (MDom dx x Unit -> MDom dx x' Unit))

type Tf'reducer s a res = s -> (s -> a -> s) -> res
type T'domState'runReducer p =
  forall s a. T'domState'runable s a (Tf'reducer s a) p

domState'runReducer'' :: forall @p. T'domState'runReducer p
domState'runReducer'' inits ups m = dom'withNewState inits \s sets -> do
  dom'runReader @p { get: s, update: sets <<< ups s } m

domState'runReducer :: forall @p. T'useAsSym "state" p T'domState'runReducer
domState'runReducer = domState'runReducer'' @p

type Tf'state s res = s -> res
type T'domState'run p = forall s. T'domState'runable s s (Tf'state s) p

domState'run'' :: forall @p. T'domState'run p
domState'run'' inits m = dom'withNewState inits \s sets -> do
  dom'runReader @p { get: s, update: sets } m

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
  forall dx x' x
   . ConsSymbol p (X'R $ Record $ RefReducerR s a ()) x' x
  => (tf (MDom dx x Unit -> MDom dx x' Unit))

type T'domRef'runReducer p =
  forall s a. T'domRef'runable s a (Tf'reducer s a) p

domRef'runReducer'' :: forall @p. T'domRef'runReducer p
domRef'runReducer'' inits ups m = dom'withNewRef inits \get sets -> do
  dom'runReader @p { get, update: domUpdateRefState get sets ups } m

domRef'runReducer :: forall @p. T'useAsSym "state" p T'domRef'runReducer
domRef'runReducer = domRef'runReducer'' @p

type T'domRef'run p = forall s. T'domRef'runable s s (Tf'state s) p

domRef'run'' :: forall @p. T'domRef'run p
domRef'run'' inits m = dom'withNewRef inits \s sets -> do
  dom'runReader @p { get: s, update: sets } m

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
  domW'push $ js_effComponent eq comp do
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
  self <- dom'getSelf
  pure $ unsafePerformEffect $ future'run_ $ evalA'self self m

-------------------------------------------------------------------

type XDom dx x a = MDom dx (X'Base $ DomPermit x) a
type XDomE e dx x a = XDom (_'x'except :: (DomE e) | dx) x a
type XDomA dx x a = XDom (_'x'async :: DomA | dx) x a
type XDomEA e dx x a = XDom (_'x'except :: (DomE e), _'x'async :: DomA | dx) x a

baseSelf :: Self () ()
baseSelf = Self runner'_ runner'_

exec'xdom :: XDom () () Unit -> ReactEl
exec'xdom = renderMEl $ selfDomless $
  selfExtend (x'permit @"dom" <<< x'eval_ @"_'x'base") baseSelf

el'renderToString :: ReactEl -> String
el'renderToString = js_renderToString

x'renderToStringAsync :: forall x. ReactEl -> Run (EA' JsError x) String
x'renderToStringAsync = e'runEffPromise <<< js_renderToStringAsync

---------------------------------------------------------------------

dom'text :: forall t dx x. SText t => t -> MDom dx x Unit
dom'text = domW'push <<< js_textEl <<< stext

el'key :: forall t dx x. String -> MDomEl'g t dx x Unit
el'key = attrW'addAttr <<< PKey

el'id :: forall t dx x. String -> MDomEl'g t dx x Unit
el'id = attrW'addAttr <<< Id

el'type :: forall t dx x. String -> MDomEl'g t dx x Unit
el'type = attrW'addAttr <<< Type

el'checked :: forall t dx x. Boolean -> MDomEl'g t dx x Unit
el'checked = attrW'addAttr <<< Checked

el'tabIndex :: forall t dx x. Int -> MDomEl'g t dx x Unit
el'tabIndex = attrW'addAttr <<< TabIndex

el'tabIndex0 :: forall t dx x. MDomEl'g t dx x Unit
el'tabIndex0 = el'tabIndex 0

el'role :: forall t dx x. String -> MDomEl'g t dx x Unit
el'role = attrW'addAttr <<< Role

el'roleButton :: forall t dx x. MDomEl'g t dx x Unit
el'roleButton = el'role "button"

el'target :: forall t dx x. String -> MDomEl'g t dx x Unit
el'target = attrW'addAttr <<< Target

el'targetBlank :: forall t dx x. MDomEl'g t dx x Unit
el'targetBlank = el'target "_blank"

el'href :: forall t dx x. String -> MDomEl'g t dx x Unit
el'href = attrW'addAttr <<< Href

el'src :: forall t dx x. String -> MDomEl'g t dx x Unit
el'src = attrW'addAttr <<< Src

el'xmlns :: forall t dx x. String -> MDomEl'g t dx x Unit
el'xmlns = attrW'addAttr <<< XmlNS

el'viewBox :: forall t dx x. String -> MDomEl'g t dx x Unit
el'viewBox = attrW'addAttr <<< ViewBox

el'fill :: forall t dx x. String -> MDomEl'g t dx x Unit
el'fill = attrW'addAttr <<< Fill

el'd :: forall t dx x. String -> MDomEl'g t dx x Unit
el'd = attrW'addAttr <<< D

el'data :: forall t dx x. String -> String -> MDomEl'g t dx x Unit
el'data k = attrW'addAttr <<< Data k

el'onClick
  :: forall t dx x. (DomEvent -> MDomEff x Unit) -> MDomEl'g t dx x Unit
el'onClick f = do
  self <- dom'getEffSelf
  attrW'addAttr $ OnClick $ \e -> eval'self self $ f e

---------------------

el'resetStyles :: forall t dx x. MDomEl'g t dx x Unit
el'resetStyles = attrW'resetStyles

el'addStyle :: forall t dx x. String -> String -> MDomEl'g t dx x Unit
el'addStyle = attrW'addStyle

el'addStyles
  :: forall t m dx x
   . Traversable m
  => m (String /\ String)
  -> MDomEl'g t dx x Unit
el'addStyles pairs = forM_ pairs \(k /\ v) -> el'addStyle k v

el'setStyles
  :: forall t m dx x
   . Traversable m
  => m (String /\ String)
  -> MDomEl'g t dx x Unit
el'setStyles pairs = el'resetStyles *> el'addStyles pairs

---------------------

el'resetCn :: forall t dx x. MDomEl'g t dx x Unit
el'resetCn = attrW'resetClassnames

el'addCn :: forall t dx x. String -> MDomEl'g t dx x Unit
el'addCn = attrW'addClassname

el'cn :: forall t dx x. String -> MDomEl'g t dx x Unit
el'cn cn = el'resetCn *> el'addCn cn

---------------------

el'resetChildren :: forall dx x. MDomEl dx x Unit
el'resetChildren = attrW'resetChildren

el'addChildren :: forall dx x. MDom dx x Unit -> MDomEl dx x Unit
el'addChildren m = do
  self <- x'extract @"self"
  let els = renderM self m
  attrW'addChildren els

el'setChildren :: forall dx x. MDom dx x Unit -> MDomEl dx x Unit
el'setChildren m = el'resetChildren *> el'addChildren m

------------------------------

dom'div :: MDom_domElement
dom'div = dom'element "div"

dom'a :: MDom_domElement
dom'a = dom'element "a"

dom'button :: MDom_domElement
dom'button = dom'element "button"

dom'label :: MDom_domElement
dom'label = dom'element "label"

dom'input :: MDom_domElement
dom'input = dom'element "input"

dom'pre :: MDom_domElement
dom'pre = dom'element "pre"

dom'span :: MDom_domElement
dom'span = dom'element "span"

dom'article :: MDom_domElement
dom'article = dom'element "article"

dom'iframe :: MDom_domElement
dom'iframe = dom'element "iframe"

dom'img :: MDom_domElement
dom'img = dom'element "img"

dom'svg :: MDom_domElement
dom'svg = dom'element "svg"

dom'ul :: MDom_domElement
dom'ul = dom'element "ul"

dom'li :: MDom_domElement
dom'li = dom'element "li"

dom'br :: MDom_domElement'Childless
dom'br = dom'element'Childless "br"

dom'path :: MDom_domElement'Childless
dom'path = dom'element'Childless "path"

------------------------------------------------------------------

op'el'text :: forall t dr x. SText t => MDom_domElement -> t -> MDom dr x Unit
op'el'text el t = el $ el'addChildren $ dom'text t

op'mdom'andText
  :: forall t dr x. SText t => MDomEl dr x Unit -> t -> MDomEl dr x Unit
op'mdom'andText m t = m *> el'addChildren (dom'text t)

infixr 0 op'el'text as %%
infixr 0 op'mdom'andText as *%%

------------------------------------------------------------------

type XDom'StateReducer a s =
  X'R { get :: s, update :: a -> Eff'At "domEff" Unit }

type XDom'State s = XDom'StateReducer s s

type XDom'RefReducer a s =
  X'R { get :: Eff'At "dom" s, update :: a -> Eff'At "dom" Unit }

type XDom'Ref s = XDom'RefReducer s s