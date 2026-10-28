module Z.XDom.Core
  ( (%%)
  , (*%%)
  , DOMRect
  , DomA
  , DomATag
  , DomE
  , DomE'
  , DomETag
  , DomEffPermit
  , DomPermit
  , MDOMEFF
  , MDom
  , MDomEff
  , MDomEl
  , R'Self
  , SELF
  , SELF'x
  , ScrollToProps
  , Self(..)
  , XDom
  , XDom'Reader
  , XDom'Ref
  , XDom'RefReducer
  , XDom'State
  , XDom'StateReducer
  , XDomA
  , XDomE
  , XDomEA
  , XDomEff
  , XDomEl
  , XDomR'
  , class Dom'ConsSymbol
  , dom'a
  , dom'a_
  , dom'array
  , dom'article
  , dom'article_
  , dom'br_
  , dom'button
  , dom'button_
  , dom'div
  , dom'div_
  , dom'do
  , dom'element
  , dom'getRunner
  , dom'getSelf
  , dom'iframe
  , dom'iframe_
  , dom'img
  , dom'img_
  , dom'input
  , dom'input_
  , dom'label
  , dom'label_
  , dom'li
  , dom'li_
  , dom'path_
  , dom'pre
  , dom'pre_
  , dom'runReader
  , dom'span
  , dom'span_
  , dom'svg
  , dom'svg_
  , dom'text
  , dom'ul
  , dom'ul_
  , dom'withAdapter
  , dom'withKey
  , dom'withNewRef
  , dom'withNewState
  , domEff'async
  , domEff'do
  , domEff'doc'pxPerRem
  , domEff'el'blur
  , domEff'el'boundingRect
  , domEff'el'classList'add
  , domEff'el'classList'rm
  , domEff'el'parentElement
  , domEff'el'querySelector
  , domEff'el'scrollTo
  , domEff'event'currentTargetValue
  , domEff'event'preventDefault
  , domEff'every
  , domEff'every'lag
  , domEff'every'lead
  , domEff'onNew
  , domEff'onNew'lag
  , domEff'onNew'lead
  , domEff'once
  , domEff'once'lag
  , domEff'once'lead
  , domEffA'animationFrame
  , domEffA'sync
  , domError'bind
  , domError'bind''
  , domError'fail
  , domError'fail''
  , domError'ok
  , domError'ok''
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
  , el'addCn
  , el'addStyle
  , el'addStyles
  , el'alt
  , el'checked
  , el'cn
  , el'd
  , el'data
  , el'fill
  , el'href
  , el'id
  , el'key
  , el'loading
  , el'onAnimationEnd
  , el'onClick
  , el'onError
  , el'onInput
  , el'onKeyDown
  , el'placeholder
  , el'ref
  , el'renderToString
  , el'role
  , el'roleButton
  , el'setElRef
  , el'setStyles
  , el'src
  , el'tabIndex
  , el'tabIndex0
  , el'target
  , el'targetBlank
  , el'title
  , el'type
  , el'value
  , el'viewBox
  , el'xmlns
  , exec'xdom
  , exec'xdom'static
  , op'el'text
  , op'mdom'andText
  , x'renderToStringAsync
  , xdom'renderToString
  ) where

import Z.Prelude

import Data.Traversable (class Traversable)
import Effect.Unsafe (unsafePerformEffect)
import Z.XDom.Preact
  ( DomEl
  , DomEvent
  , PropWF(..)
  , ReactEl
  , js_effComponent
  , js_renderArray
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
  , attrW'addClassname
  , attrW'addStyle
  , attrW'resetClassnames
  , attrW'resetStyles
  , attrW'run
  , domW'concat
  , domW'exec
  , domW'finish
  , domW'push
  , domW'run
  , domW'start
  )
import Z.Z.X.Index (a'attempt)

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

type MDomEl dx x a = Run (domW :: X'R Unit | AttrW' Void (DomW' (SELF dx x))) a

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

dom'getDomSelf :: forall dx x x'. Run (SELF'x dx x x') (Self () x)
dom'getDomSelf = dom'getSelf <#> selfDomless

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
dom'withKey k m = do
  parentId <- domW'start
  m
  children <- domW'finish parentId
  domW'push $ js_withKey k (js_renderFragment children)

dom'array :: forall dx x. MDom dx x Unit -> MDom dx x Unit
dom'array m = do
  parentId <- domW'start
  m
  children <- domW'finish parentId
  domW'push $ js_renderArray children

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

type MDom_domElement =
  forall dx x. MDomEl dx x (MDom dx x Unit) -> MDom dx x Unit

type MDom_domElement'Childless =
  forall dx x. MDomEl dx x Unit -> MDom dx x Unit

dom'element :: String -> MDom_domElement
dom'element s mattr = do
  parentId <- domW'start
  mdom /\ { attrs } <- x'eval_ @"domW" $ attrW'run (const []) mattr
  mdom
  children <- domW'finish parentId
  domW'push $ js_renderEl s attrs children

dom'element_ :: String -> MDom_domElement'Childless
dom'element_ s mattr = dom'element s $ mattr <#> pure

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

type T'domError'ok p =
  forall e dx' dx x xx a
   . ConsSymbol p (DomE e) dx' dx
  => Either e a
  -> Run (self :: R'Self dx x | xx) a

domError'ok'' :: forall @p. T'domError'ok p
domError'ok'' (Left e) = domError'fail'' @p e
domError'ok'' (Right v) = pure v

domError'ok :: forall p. T'use'e'AsSym p $ T'domError'ok
domError'ok = domError'ok'' @p

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

domEffA'sync :: forall x. MDomEff x Unit -> MDomEff'a x Unit
domEffA'sync m = dom'getSelf <#> flip eval'self m

domEff'el'classList'rm
  :: forall x. DomEl -> String -> MDomEff (XDomR' x) Unit
domEff'el'classList'rm el cn = do
  x'extract @"_'xdom'reader" >>= \r -> x'do $ r.el'classList'rm el cn

domEff'el'classList'add
  :: forall x. DomEl -> String -> MDomEff (XDomR' x) Unit
domEff'el'classList'add el cn = do
  x'extract @"_'xdom'reader" >>= \r -> x'do $ r.el'classList'add el cn

domEff'event'preventDefault :: forall x. DomEvent -> MDomEff (XDomR' x) Unit
domEff'event'preventDefault event = do
  x'extract @"_'xdom'reader" >>= \r -> x'do $ r.event'preventDefault event

domEff'event'currentTargetValue
  :: forall x. DomEvent -> MDomEff (XDomR' x) String
domEff'event'currentTargetValue event = do
  x'extract @"_'xdom'reader" >>= \r -> x'do $ r.event'currentTargetValue event

domEff'el'parentElement
  :: forall x. DomEl -> MDomEff (XDomR' x) (Maybe DomEl)
domEff'el'parentElement el = do
  x'extract @"_'xdom'reader" >>= \r -> x'do $ r.el'parentElement el

domEff'el'querySelector
  :: forall x. DomEl -> String -> MDomEff (XDomR' x) (Maybe DomEl)
domEff'el'querySelector el s = do
  x'extract @"_'xdom'reader" >>= \r -> x'do $ r.el'querySelector el s

domEff'el'scrollTo
  :: forall x. DomEl -> Edit ScrollToProps -> MDomEff (XDomR' x) Unit
domEff'el'scrollTo el edits = do
  let props = edit default edits
  x'extract @"_'xdom'reader" >>= \r -> x'do $ r.el'scrollTo el props

domEff'el'blur :: forall x. DomEl -> MDomEff (XDomR' x) Unit
domEff'el'blur el = do
  x'extract @"_'xdom'reader" >>= \r -> x'do $ r.el'blur el

domEff'el'boundingRect :: forall x. DomEl -> MDomEff (XDomR' x) DOMRect
domEff'el'boundingRect el = do
  x'extract @"_'xdom'reader" >>= \r -> x'do $ r.el'boundingRect el

domEffA'animationFrame :: forall x. MDomEff'a (XDomR' x) Unit
domEffA'animationFrame = do
  f <- x'extract @"_'xdom'reader" >>= \r -> x'do r.future'animationFrame
  a'attempt f <#> const unit

domEff'doc'pxPerRem :: forall x. MDomEff (XDomR' x) Number
domEff'doc'pxPerRem = do
  x'extract @"_'xdom'reader" >>= \r -> x'do r.doc'pxPerRem

-------------------------------------------------------------------

type ScrollToProps =
  { isSmoothOrAuto :: Maybe Boolean
  , top :: Maybe Number
  , left :: Maybe Number
  }

type DOMRect =
  { left :: Number
  , top :: Number
  , right :: Number
  , bottom :: Number
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

type XDom'Reader =
  { el'classList'rm :: DomEl -> String -> Eff'At "domEff" Unit
  , el'classList'add :: DomEl -> String -> Eff'At "domEff" Unit
  , el'parentElement :: DomEl -> Eff'At "domEff" (Maybe DomEl)
  , el'querySelector :: DomEl -> String -> Eff'At "domEff" (Maybe DomEl)
  , el'scrollTo :: DomEl -> ScrollToProps -> Eff'At "domEff" Unit
  , el'blur :: DomEl -> Eff'At "domEff" Unit
  , el'boundingRect :: DomEl -> Eff'At "domEff" DOMRect
  , event'preventDefault :: DomEvent -> Eff'At "domEff" Unit
  , event'currentTargetValue :: DomEvent -> Eff'At "domEff" String
  , future'animationFrame :: Eff'At "domEff" (Future Unit)
  , doc'pxPerRem :: Eff'At "domEff" Number
  }

type XDom'Eff'Reader =
  { el'classList'rm :: DomEl -> String -> Effect Unit
  , el'classList'add :: DomEl -> String -> Effect Unit
  , el'parentElement :: DomEl -> Effect (Maybe DomEl)
  , el'querySelector :: DomEl -> String -> Effect (Maybe DomEl)
  , el'scrollTo :: DomEl -> ScrollToProps -> Effect Unit
  , el'blur :: DomEl -> Effect Unit
  , el'boundingRect :: DomEl -> Effect DOMRect
  , event'preventDefault :: DomEvent -> Effect Unit
  , event'currentTargetValue :: DomEvent -> Effect String
  , future'animationFrame :: Effect (Future Unit)
  , doc'pxPerRem :: Effect Number
  }

type XDom dx x a = MDom dx (XDomR' $ X'Base $ DomPermit x) a
type XDomEl dx x a = MDomEl dx (XDomR' $ X'Base $ DomPermit x) a
type XDomEff x a = MDomEff (XDomR' $ X'Base $ DomPermit x) a
type XDomE e dx x a = XDom (_'x'except :: (DomE e) | dx) x a
type XDomA dx x a = XDom (_'x'async :: DomA | dx) x a
type XDomEA e dx x a = XDom (_'x'except :: (DomE e), _'x'async :: DomA | dx) x a

type DomE' e dx = (_'x'except :: DomE e | dx)
type XDomR' x = (_'xdom'reader :: X'R XDom'Reader | x)

baseSelf :: Self () ()
baseSelf = Self runner'_ runner'_

xdom'reader'run :: forall x a. XDom'Eff'Reader -> Run (XDomR' x) a -> Run x a
xdom'reader'run = r'run'' @"_'xdom'reader" <<< adapt'eff'reader
  where
  adapt'eff'reader r =
    { el'classList'rm: \el -> eff'tag @"domEff" <<< r.el'classList'rm el
    , el'classList'add: \el -> eff'tag @"domEff" <<< r.el'classList'add el
    , el'parentElement: eff'tag @"domEff" <<< r.el'parentElement
    , el'querySelector: \el -> eff'tag @"domEff" <<< r.el'querySelector el
    , el'blur: eff'tag @"domEff" <<< r.el'blur
    , el'boundingRect: eff'tag @"domEff" <<< r.el'boundingRect
    , el'scrollTo: \el -> eff'tag @"domEff" <<< r.el'scrollTo el
    , event'preventDefault: eff'tag @"domEff" <<< r.event'preventDefault
    , event'currentTargetValue: eff'tag @"domEff" <<< r.event'currentTargetValue
    , future'animationFrame: eff'tag @"domEff" r.future'animationFrame
    , doc'pxPerRem: eff'tag @"domEff" r.doc'pxPerRem
    }

exec'xdom :: XDom'Eff'Reader -> XDom () () Unit -> ReactEl
exec'xdom r = renderMEl $ selfDomless $ selfExtend
  (x'permit @"dom" <<< x'eval_ @"_'x'base" <<< xdom'reader'run r)
  baseSelf

exec'xdom'static :: XDom () () Unit -> ReactEl
exec'xdom'static = exec'xdom
  { el'classList'rm: \_ _ -> pure unit
  , el'classList'add: \_ _ -> pure unit
  , el'parentElement: \_ -> pure Nothing
  , el'querySelector: \_ _ -> pure Nothing
  , el'blur: \_ -> pure unit
  , el'boundingRect: \_ -> pure
      { width: 0.0
      , height: 0.0
      , x: 0.0
      , y: 0.0
      , top: 0.0
      , left: 0.0
      , bottom: 0.0
      , right: 0.0
      }
  , el'scrollTo: \_ _ -> pure unit
  , event'preventDefault: \_ -> pure unit
  , event'currentTargetValue: \_ -> pure ""
  , future'animationFrame: pure $ future'of unit
  , doc'pxPerRem: pure 1.0
  }

el'renderToString :: ReactEl -> String
el'renderToString = js_renderToString

xdom'renderToString :: XDom () () Unit -> String
xdom'renderToString = js_renderToString <<< exec'xdom'static

x'renderToStringAsync :: forall x. ReactEl -> Run (EA' JsError x) String
x'renderToStringAsync = e'runEffPromise <<< js_renderToStringAsync

---------------------------------------------------------------------

dom'text :: forall t dx x. SText t => t -> MDom dx x Unit
dom'text = domW'push <<< js_textEl <<< stext

el'key :: forall dx x. String -> MDomEl dx x Unit
el'key = attrW'addAttr <<< PKey

el'id :: forall dx x. String -> MDomEl dx x Unit
el'id = attrW'addAttr <<< Id

el'type :: forall dx x. String -> MDomEl dx x Unit
el'type = attrW'addAttr <<< Type

el'checked :: forall dx x. Boolean -> MDomEl dx x Unit
el'checked = attrW'addAttr <<< Checked

el'tabIndex :: forall dx x. Int -> MDomEl dx x Unit
el'tabIndex = attrW'addAttr <<< TabIndex

el'tabIndex0 :: forall dx x. MDomEl dx x Unit
el'tabIndex0 = el'tabIndex 0

el'role :: forall dx x. String -> MDomEl dx x Unit
el'role = attrW'addAttr <<< Role

el'roleButton :: forall dx x. MDomEl dx x Unit
el'roleButton = el'role "button"

el'target :: forall dx x. String -> MDomEl dx x Unit
el'target = attrW'addAttr <<< Target

el'targetBlank :: forall dx x. MDomEl dx x Unit
el'targetBlank = el'target "_blank"

el'href :: forall dx x. String -> MDomEl dx x Unit
el'href = attrW'addAttr <<< Href

el'src :: forall dx x. String -> MDomEl dx x Unit
el'src = attrW'addAttr <<< Src

el'xmlns :: forall dx x. String -> MDomEl dx x Unit
el'xmlns = attrW'addAttr <<< XmlNS

el'viewBox :: forall dx x. String -> MDomEl dx x Unit
el'viewBox = attrW'addAttr <<< ViewBox

el'alt :: forall dx x. String -> MDomEl dx x Unit
el'alt = attrW'addAttr <<< Alt

el'title :: forall dx x. String -> MDomEl dx x Unit
el'title = attrW'addAttr <<< Title

el'placeholder :: forall dx x. String -> MDomEl dx x Unit
el'placeholder = attrW'addAttr <<< Placeholder

el'value :: forall dx x a. a -> MDomEl dx x Unit
el'value = attrW'addAttr <<< Value <<< jsAny

el'loading :: forall dx x. String -> MDomEl dx x Unit
el'loading = attrW'addAttr <<< Loading

el'fill :: forall dx x. String -> MDomEl dx x Unit
el'fill = attrW'addAttr <<< Fill

el'd :: forall dx x. String -> MDomEl dx x Unit
el'd = attrW'addAttr <<< D

el'data :: forall dx x. String -> String -> MDomEl dx x Unit
el'data k = attrW'addAttr <<< Data k

type El'On dx x = (DomEvent -> MDomEff x Unit) -> MDomEl dx x Unit

el'on' :: forall dx x. ((DomEvent -> Unit) -> PropWF) -> El'On dx x
el'on' mkPropWF f = do
  self <- dom'getEffSelf
  attrW'addAttr $ mkPropWF $ \e -> eval'self self $ f e

el'onClick :: forall dx x. El'On dx x
el'onClick = el'on' OnClick

el'onError :: forall dx x. El'On dx x
el'onError = el'on' OnError

el'onAnimationEnd :: forall dx x. El'On dx x
el'onAnimationEnd = el'on' OnAnimationEnd

el'onInput :: forall dx x. El'On dx x
el'onInput = el'on' OnInput

el'onKeyDown :: forall dx x. El'On dx x
el'onKeyDown = el'on' OnKeyDown

el'ref :: forall dx x. (DomEl -> Run (SELF () x) Unit) -> MDomEl dx x Unit
el'ref f =
  dom'getDomSelf >>= \self -> attrW'addAttr $ Ref $ eval'self self <<< f

el'setElRef
  :: forall dx x
   . (Maybe DomEl -> Eff'At "dom" Unit)
  -> MDomEl dx (DomPermit x) Unit
el'setElRef f = el'ref $ x'do <<< f <<< Just

---------------------

el'addStyle :: forall dx x. String -> String -> MDomEl dx x Unit
el'addStyle = attrW'addStyle

el'addStyles
  :: forall m dx x
   . Traversable m
  => m (String /\ String)
  -> MDomEl dx x Unit
el'addStyles pairs = forM_ pairs \(k /\ v) -> el'addStyle k v

el'setStyles
  :: forall m dx x
   . Traversable m
  => m (String /\ String)
  -> MDomEl dx x Unit
el'setStyles pairs = attrW'resetStyles *> el'addStyles pairs

---------------------

el'addCn :: forall dx x. String -> MDomEl dx x Unit
el'addCn = attrW'addClassname

el'cn :: forall dx x. String -> MDomEl dx x Unit
el'cn cn = attrW'resetClassnames *> el'addCn cn

------------------------------

dom'div :: MDom_domElement
dom'div = dom'element "div"

dom'div_ :: MDom_domElement'Childless
dom'div_ = dom'element_ "div"

dom'a :: MDom_domElement
dom'a = dom'element "a"

dom'a_ :: MDom_domElement'Childless
dom'a_ = dom'element_ "a"

dom'button :: MDom_domElement
dom'button = dom'element "button"

dom'button_ :: MDom_domElement'Childless
dom'button_ = dom'element_ "button"

dom'label :: MDom_domElement
dom'label = dom'element "label"

dom'label_ :: MDom_domElement'Childless
dom'label_ = dom'element_ "label"

dom'input :: MDom_domElement
dom'input = dom'element "input"

dom'input_ :: MDom_domElement'Childless
dom'input_ = dom'element_ "input"

dom'pre :: MDom_domElement
dom'pre = dom'element "pre"

dom'pre_ :: MDom_domElement'Childless
dom'pre_ = dom'element_ "pre"

dom'span :: MDom_domElement
dom'span = dom'element "span"

dom'span_ :: MDom_domElement'Childless
dom'span_ = dom'element_ "span"

dom'article :: MDom_domElement
dom'article = dom'element "article"

dom'article_ :: MDom_domElement'Childless
dom'article_ = dom'element_ "article"

dom'iframe :: MDom_domElement
dom'iframe = dom'element "iframe"

dom'iframe_ :: MDom_domElement'Childless
dom'iframe_ = dom'element_ "iframe"

dom'img :: MDom_domElement
dom'img = dom'element "img"

dom'img_ :: MDom_domElement'Childless
dom'img_ = dom'element_ "img"

dom'svg :: MDom_domElement
dom'svg = dom'element "svg"

dom'svg_ :: MDom_domElement'Childless
dom'svg_ = dom'element_ "svg"

dom'ul :: MDom_domElement
dom'ul = dom'element "ul"

dom'ul_ :: MDom_domElement'Childless
dom'ul_ = dom'element_ "ul"

dom'li :: MDom_domElement
dom'li = dom'element "li"

dom'li_ :: MDom_domElement'Childless
dom'li_ = dom'element_ "li"

dom'br_ :: MDom_domElement'Childless
dom'br_ = dom'element_ "br"

dom'path_ :: MDom_domElement'Childless
dom'path_ = dom'element_ "path"

------------------------------------------------------------------

op'el'text :: forall t dx x. SText t => MDom_domElement -> t -> MDom dx x Unit
op'el'text el t = el $ pure $ dom'text t

op'mdom'andText
  :: forall t dx x
   . SText t
  => MDomEl dx x Unit
  -> t
  -> MDomEl dx x (MDom dx x Unit)
op'mdom'andText m t = m *> pure (dom'text t)

infixr 0 op'el'text as %%
infixr 0 op'mdom'andText as *%%

------------------------------------------------------------------

type XDom'StateReducer a s =
  X'R { get :: s, update :: a -> Eff'At "domEff" Unit }

type XDom'State s = XDom'StateReducer s s

type XDom'RefReducer a s =
  X'R { get :: Eff'At "dom" s, update :: a -> Eff'At "dom" Unit }

type XDom'Ref s = XDom'RefReducer s s