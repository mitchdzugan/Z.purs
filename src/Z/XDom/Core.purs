module Z.XDom.Core
  ( (%%)
  , ATTR
  , DomA
  , DomATag
  , DomE
  , DomETag
  , DomEffPermit
  , MDOMEFF
  , MDom
  , MDomEff
  , MDomEl
  , R'Self
  , Self
  , XDom
  , XDomA
  , XDomE
  , XDomEA
  , dom'a
  , dom'article
  , dom'br
  , dom'button
  , dom'div
  , dom'element
  , dom'iframe
  , dom'pre
  , dom'span
  , dom'text
  , dom'withAdapter
  , dom'withKey
  , dom'withNewState
  , domE'bind
  , domE'bind''
  , domE'fail
  , domE'fail''
  , domEff'do
  , domEff'getRunner
  , domEff'getSelf
  , domEff'on
  , domEff'on'1
  , domEff'on'1'post
  , domEff'on'1'pre
  , domEff'on'all
  , domEff'on'all'post
  , domEff'on'all'pre
  , domEff'on'post
  , domEff'on'pre
  , domR'run
  , domR'run''
  , domS'dispatch
  , domS'dispatch''
  , domS'get
  , domS'get''
  , domS'runReducer
  , domS'runReducer''
  , domS'runState
  , domS'runState''
  , domS'set
  , domS'set''
  , el'cn
  , el'cnW
  , el'href
  , el'key
  , el'onClick
  , exec'xdom
  , op'el'text
  ) where

import Z.Prelude

import Z.XDom.Preact as D

data Self dx x = Self (Runner dx) (Runner x)
type EffSelf x = Self () x

type R'Self dx x = X'R (Self dx x)

eval'self :: forall dx x a. Self dx x -> Run (self :: R'Self dx x | x) a -> a
eval'self self@(Self _ rn) m = runner'eval rn $ r'run'' @"self" self $ m

selfExtend :: forall dr x' x. RunMW x x' -> Self dr x' -> Self dr x
selfExtend fm (Self drn rn) = Self drn (runner'extend fm rn)

selfExtendDom :: forall dx' dx x. RunMW dx dx' -> Self dx' x -> Self dx x
selfExtendDom fm (Self drn r) = Self (runner'extend fm drn) r

selfDomless :: forall dx x. Self dx x -> Self () x
selfDomless (Self _ r) = Self runner'_ r

type DomEffPermit x = (domEff :: X'Permit | x)

type MDom dx x a = Run (Wa' D.ReactEl (self :: R'Self dx x | x)) a

type ATTR x = (attr :: X'W $ Array D.PropWF | x)
type MDOMEFF x = (self :: R'Self () (DomEffPermit x) | DomEffPermit x)
type MDomEff x a = Run (MDOMEFF x) a
type MDomEl dx x a =
  Run (ATTR $ Wa' D.ReactEl $ Wa' D.ReactEl (self :: R'Self dx x | x)) a

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

dom'getEffSelf :: forall dr x. MDom dr x (Self () (DomEffPermit x))
dom'getEffSelf =
  x'extract @"self" <#> selfDomless <#> selfExtend (x'permit @"domEff")

el'getEffSelf :: forall dr x. MDomEl dr x (Self () (DomEffPermit x))
el'getEffSelf =
  x'extract @"self" <#> selfDomless <#> selfExtend (x'permit @"domEff")

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

--------------------------------------------------------------------------------

type T'domE'bind e p =
  forall dx' dx x
   . ConsSymbol p (DomE e) dx' dx
  => (->) e $ MDom dx' x Unit
  -> MDom dx x Unit
  -> MDom dx' x Unit

domE'bind'' :: forall @p e. T'domE'bind e p
domE'bind'' em m = do
  oldSelf <- x'extract @"self"
  let self = selfExtendDom (r'run'' @p (Proxy @(DomETag /\ e))) oldSelf
  w'say $ D.js_withBoundedError (reflectSymbol $ p @p)
    (\e _ -> renderMEl oldSelf $ em e)
    (\_ -> renderMEl self m)

domE'bind :: forall e p. T'use'e'AsSym p $ T'domE'bind e
domE'bind = domE'bind'' @p

type T'domE'fail p =
  forall e dx' dx x xx a
   . ConsSymbol p (DomE e) dx' dx
  => e
  -> Run (self :: R'Self dx x | xx) a

domE'fail'' :: forall @p. T'domE'fail p
domE'fail'' e = x'extract @"self" <#> \_ ->
  D.js_throwBoundedError (reflectSymbol $ Proxy @p) e

domE'fail :: forall p. T'use'e'AsSym p $ T'domE'fail
domE'fail = domE'fail'' @p

--------------------------------------------------------------------------------

type ReducerR s a r = (get :: s, update :: a -> Eff'At "domEff" Unit | r)

type T'domS'runable s a tf p =
  forall dr x' x
   . ConsSymbol p (X'R $ Record $ ReducerR s a ()) x' x
  => (tf (MDom dr x Unit -> MDom dr x' Unit))

type Tf'reducer s a res = s -> (s -> a -> s) -> res
type T'domS'runReducer p = forall s a. T'domS'runable s a (Tf'reducer s a) p

domS'runReducer'' :: forall @p. T'domS'runReducer p
domS'runReducer'' inits ups m = dom'withNewState inits \s sets -> do
  domR'run'' @p { get: s, update: sets <<< ups s } m

domS'runReducer :: forall @p. T'useAsSym "state" p T'domS'runReducer
domS'runReducer = domS'runReducer'' @p

type Tf'state s res = s -> res
type T'domS'run p = forall s. T'domS'runable s s (Tf'state s) p

domS'runState'' :: forall @p. T'domS'run p
domS'runState'' inits m = dom'withNewState inits \s sets -> do
  domR'run'' @p { get: s, update: sets } m

domS'runState :: forall @p. T'use's'AsSym p T'domS'run
domS'runState = domS'runState'' @p

type T'domS'get p =
  forall s x' x r
   . IsSymbol p
  => Cons p (X'R $ Record (get :: s | r)) x' x
  => Run x s

domS'get'' :: forall @p. T'domS'get p
domS'get'' = x'extract @p <#> _.get

domS'get :: forall p. T'use's'AsSym p T'domS'get
domS'get = domS'get'' @p

type T'domS'setable s a p =
  forall x' x r
   . ConsSymbol p (X'R $ Record $ ReducerR s a r) (DomEffPermit x')
       (DomEffPermit x)
  => a
  -> Run (DomEffPermit x) Unit

type T'domS'dispatch p = forall s a. T'domS'setable s a p

domS'dispatch'' :: forall @p. T'domS'dispatch p
domS'dispatch'' a = x'extract @p >>= \r -> x'do (r.update a)

domS'dispatch :: forall p. T'use's'AsSym p T'domS'dispatch
domS'dispatch = domS'dispatch'' @p

type T'domS'set p = forall s. T'domS'setable s s p

domS'set'' :: forall @p. T'domS'set p
domS'set'' = domS'dispatch'' @p

domS'set :: forall p. T'use's'AsSym p T'domS'set
domS'set = domS'set'' @p

type MDomEff_D x = MDomEff x (MDomEff x Unit)

domEff'on :: forall a dx x. Eq a => a -> MDomEff_D x -> MDom dx x Unit
domEff'on comp on = do
  self <- dom'getEffSelf
  w'say $ D.js_effComponent eq comp $ \_ ->
    let r' = eval'self self on in \_ -> eval'self self r'

domEff'on'1 :: forall dx x. MDomEff_D x -> MDom dx x Unit
domEff'on'1 = domEff'on unit

domEff'on'all :: forall dx x. MDomEff_D x -> MDom dx x Unit
domEff'on'all = domEff'on antiUnit

domEff'on'pre :: forall a dx x. Eq a => a -> MDomEff x Unit -> MDom dx x Unit
domEff'on'pre a m = domEff'on a $ m *> pure (pure unit)

domEff'on'1'pre :: forall dx x. MDomEff x Unit -> MDom dx x Unit
domEff'on'1'pre m = domEff'on unit $ m *> pure (pure unit)

domEff'on'all'pre :: forall dx x. MDomEff x Unit -> MDom dx x Unit
domEff'on'all'pre m = domEff'on antiUnit $ m *> pure (pure unit)

domEff'on'post :: forall a dx x. Eq a => a -> MDomEff x Unit -> MDom dx x Unit
domEff'on'post a m = domEff'on a $ pure m

domEff'on'1'post :: forall dx x. MDomEff x Unit -> MDom dx x Unit
domEff'on'1'post m = domEff'on unit $ pure m

domEff'on'all'post :: forall dx x. MDomEff x Unit -> MDom dx x Unit
domEff'on'all'post m = domEff'on antiUnit $ pure m

dom'text :: forall t dr x. SText t => t -> MDom dr x Unit
dom'text = w'say <<< D.js_textEl <<< stext

el'key :: forall dr x. String -> MDomEl dr x Unit
el'key = w'tell'' @"attr" <<< pure <<< D.PKey

el'href :: forall dr x. String -> MDomEl dr x Unit
el'href = w'tell'' @"attr" <<< pure <<< D.Href

el'cn :: forall dr x. String -> MDomEl dr x Unit
el'cn = w'tell'' @"attr" <<< pure <<< D.ClassName

el'cnW :: forall dr x. ((String -> StrW) -> StrW) -> MDomEl dr x Unit
el'cnW fm = el'cn $ w'str'sp fm

el'onClick :: forall dr x. (D.DomEvent -> MDomEff x Unit) -> MDomEl dr x Unit
el'onClick f = do
  self <- el'getEffSelf
  w'tell'' @"attr" $ pure $ D.OnClick $ \e -> eval'self self $ f e

type XDom_domElement_ = forall dr x. MDomEl dr x Unit -> MDom dr x Unit

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
type XDomA dx x a = XDom (_'x'aff :: DomA | dx) x a
type XDomE e dx x a = XDom (_'x'except :: (DomE e) | dx) x a
type XDomEA e dx x a = XDom (_'x'except :: (DomE e), _'x'aff :: DomA | dx) x a

type XDom_ dr x = T'_ $ XDom dr x
type XDomA_ dr x = T'_ $ XDom dr x
type XDomE_ e dr x = T'_ $ XDomE e dr x
type XDomEA_ e dr x = T'_ $ XDomEA e dr x

baseSelf :: Self () ()
baseSelf = Self runner'_ runner'_

exec'xdom :: XDom_ () () -> D.ReactEl
exec'xdom = renderMEl $ selfDomless $ selfExtend (x'eval_ @"_'x'base") baseSelf

dom'div :: XDom_domElement_
dom'div = dom'element "div"

dom'a :: XDom_domElement_
dom'a = dom'element "a"

dom'button :: XDom_domElement_
dom'button = dom'element "button"

dom'pre :: XDom_domElement_
dom'pre = dom'element "pre"

dom'span :: XDom_domElement_
dom'span = dom'element "span"

dom'article :: XDom_domElement_
dom'article = dom'element "article"

dom'iframe :: XDom_domElement_
dom'iframe = dom'element "iframe"

dom'br :: XDom_domElement_NoCh
dom'br = dom'element_noCh "br"

op'el'text :: forall t dr x. SText t => XDom_domElement_ -> t -> MDom dr x Unit
op'el'text el t = el $ dom'text t

infixr 0 op'el'text as %%