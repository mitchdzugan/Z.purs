module Z.XDom.Core
  ( ATTR
  , DomA
  , DomATag
  , DomE
  , DomETag
  , DomEffPermit
  , MDom
  , MDomEff
  , MDomEl
  , R'Self
  , Self
  , UseEffTag
  , XDom
  , XDomA
  , XDomE
  , XDomEA
  , dom'a
  , dom'article
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
  , domEff'getSelf
  , domEff'use
  , domEff'use''
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
  , domUseEff
  , el'cn
  , el'cnW
  , el'href
  , el'key
  , el'onClick
  , eval'self
  , exec'xdom
  ) where

import Z.Prelude

import Z.XDom.Preact as D

data Runner x = Runner (forall a. Run x a -> Run () a)

data Self dx x = Self (Runner dx) (Runner x)
type EffSelf x = Self () x

type R'Self dx x = R' (Self dx x)

eval'self :: forall dx x a. Self dx x -> Run (self :: R'Self dx x | x) a -> a
eval'self self@(Self _ (Runner rn)) m =
  eval_ $ rn $ r'run'' @"self" self $ m

selfExtend
  :: forall dr x' x. (forall a. Run x a -> Run x' a) -> Self dr x' -> Self dr x
selfExtend fm (Self dr (Runner rn)) = Self dr (Runner (rn <<< fm))

selfExtendDom
  :: forall dx' dx x
   . (forall a. Run dx a -> Run dx' a)
  -> Self dx' x
  -> Self dx x
selfExtendDom fm (Self (Runner rn) r) = Self (Runner (rn <<< fm)) r

selfDomless :: forall dx x. Self dx x -> Self () x
selfDomless (Self _ r) = Self (Runner id) r

type DomEffPermit x = (domEff :: Eff'Permit | x)

type MDom dx x a = Run (Wa D.ReactEl (self :: R'Self dx x | x)) a

type ATTR x = (attr :: W' $ Array D.PropWF | x)
type MDomEff x a = Run (self :: R'Self () (DomEffPermit x) | DomEffPermit x) a
type MDomEl dx x a =
  Run (ATTR $ Wa D.ReactEl $ Wa D.ReactEl (self :: R'Self dx x | x)) a

foreign import data DomETag :: Type
foreign import data DomATag :: Type

type DomE e = R' $ Proxy $ DomETag /\ e
type DomA = R' $ Proxy DomATag

--------------------------------------------------------------------------------

renderM
  :: forall dr x. Self dr x -> MDom dr x Unit -> Array D.ReactEl
renderM self m = eval'self self $ w'exec m

renderMEl :: forall dr x. Self dr x -> MDom dr x Unit -> D.ReactEl
renderMEl self = D.js_renderFragment <<< renderM self

domRenderM :: forall dr x. MDom dr x Unit -> MDom dr x (Array D.ReactEl)
domRenderM m = r'ask'' @"self" <#> flip renderM m

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
  self <- r'ask'' @"self"
  w'say $ D.js_withState (renderFn self) initalState
  where
  renderFn self s ss = renderM self $ fm s (w ss)
  w ss s = tagEffX @"domEff" $ ss s

dom'withAdapter
  :: forall dr x' x ret
   . (forall a. Run x a -> Run x' a)
  -> MDom dr x ret
  -> MDom dr x' ret
dom'withAdapter fm m = do
  self <- r'ask'' @"self" <#> selfExtend fm
  let els /\ res = eval'self self $ w'run m
  w'tell els
  pure res

domEff'getSelf :: forall x. MDomEff x (Self () (DomEffPermit x))
domEff'getSelf = r'ask'' @"self"

domEff'do :: forall x a. Eff'At "domEff" a -> Run (DomEffPermit x) a
domEff'do = eff'do'' @"domEff"

dom'getEffSelf :: forall dr x. MDom dr x (Self () (DomEffPermit x))
dom'getEffSelf =
  r'ask'' @"self" <#> selfDomless <#> selfExtend (eff'permit'' @"domEff")

el'getEffSelf :: forall dr x. MDomEl dr x (Self () (DomEffPermit x))
el'getEffSelf =
  r'ask'' @"self" <#> selfDomless <#> selfExtend (eff'permit'' @"domEff")

--------------------------------------------------------------------------------

type T'domR'run p =
  forall r x' x dr ret
   . IsSymbol p
  => Cons p (R' r) x' x
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
   . IsSymbol p
  => Cons p (DomE e) dx' dx
  => (->) e $ MDom dx' x Unit
  -> MDom dx x Unit
  -> MDom dx' x Unit

domE'bind'' :: forall @p e. T'domE'bind e p
domE'bind'' em m = do
  oldSelf <- r'ask'' @"self"
  let self = selfExtendDom (r'run'' @p (Proxy @(DomETag /\ e))) oldSelf
  w'say $ D.js_withBoundedError (reflectSymbol $ p @p)
    (\e _ -> renderMEl oldSelf $ em e)
    (\_ -> renderMEl self m)

domE'bind :: forall e p. T'use'e'AsSym p $ T'domE'bind e
domE'bind = domE'bind'' @p

type T'domE'fail p =
  forall e dx' dx x xx a
   . IsSymbol p
  => Cons p (DomE e) dx' dx
  => e
  -> Run (self :: R'Self dx x | xx) a

domE'fail'' :: forall @p. T'domE'fail p
domE'fail'' e = r'ask'' @"self" <#> \_ ->
  D.js_throwBoundedError (reflectSymbol $ Proxy @p) e

domE'fail :: forall p. T'use'e'AsSym p $ T'domE'fail
domE'fail = domE'fail'' @p

--------------------------------------------------------------------------------

type T'domS'runable s a tf p =
  forall dr x' x
   . IsSymbol p
  => Cons p (R'Rec (get :: s, update :: a -> Eff'At "domEff" Unit)) x' x
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
  forall s x' x r. IsSymbol p => Cons p (R'Rec (get :: s | r)) x' x => Run x s

domS'get'' :: forall @p. T'domS'get p
domS'get'' = r'ask'' @p <#> _.get

domS'get :: forall p. T'use's'AsSym p T'domS'get
domS'get = domS'get'' @p

type T'domS'setable s a p =
  forall x' x r
   . IsSymbol p
  => Cons p (R'Rec (get :: s, update :: a -> Eff'At "domEff" Unit | r))
       (DomEffPermit x')
       (DomEffPermit x)
  => a
  -> Run (DomEffPermit x) Unit

type T'domS'dispatch p = forall s a. T'domS'setable s a p

domS'dispatch'' :: forall @p. T'domS'dispatch p
domS'dispatch'' a = r'ask'' @p >>= \r -> eff'do'' @"domEff" (r.update a)

domS'dispatch :: forall p. T'use's'AsSym p T'domS'dispatch
domS'dispatch = domS'dispatch'' @p

type T'domS'set p = forall s. T'domS'setable s s p

domS'set'' :: forall @p. T'domS'set p
domS'set'' = domS'dispatch'' @p

domS'set :: forall p. T'use's'AsSym p T'domS'set
domS'set = domS'set'' @p

type MDomEff_D x = MDomEff x (MDomEff x Unit)

domUseEff :: forall dr x a. Eq a => a -> MDomEff_D x -> MDom dr x Unit
domUseEff comp on = do
  self <- dom'getEffSelf
  w'say $ D.js_effComponent eq comp $ \_ ->
    let r' = eval'self self on in \_ -> eval'self self r'

foreign import data UseEffTag :: forall k. k -> Type

type MD_ dx x = MDom dx x Unit
type MDE_ x = MDomEff x Unit
type MDE_D x = MDomEff_D x
type UEF = UseEffTag ""
type UEF1 = UseEffTag "1"
type UEFAll = UseEffTag "*"
type UEFOn = UseEffTag "+"
type UEF1On = UseEffTag "+1"
type UEFAllOn = UseEffTag "+*"
type UEFOff = UseEffTag "-"
type UEF1Off = UseEffTag "1-"
type UEFAllOff = UseEffTag "*-"

instance Generable UEF1 GDefault (MDE_D x -> MD_ dx x) where
  mkGenerable = domUseEff unit
else instance Generable UEFAll GDefault (MDE_D x -> MD_ dx x) where
  mkGenerable = domUseEff antiUnit
else instance (Eq a) => Generable UEFOn GDefault (a -> MDE_ x -> MD_ dx x) where
  mkGenerable a m = domUseEff a $ m *> pure (pure unit)
else instance Generable UEF1On GDefault (MDE_ x -> MD_ dx x) where
  mkGenerable m = domUseEff unit $ m *> pure (pure unit)
else instance Generable UEFAllOn GDefault (MDE_ x -> MD_ dx x) where
  mkGenerable m = domUseEff antiUnit $ m *> pure (pure unit)
else instance (Eq a) => Generable UEFOff GDefault (a -> MDE_ x -> MD_ dx x) where
  mkGenerable a m = domUseEff a $ pure m
else instance Generable UEF1Off GDefault (MDE_ x -> MD_ dx x) where
  mkGenerable m = domUseEff unit $ pure m
else instance Generable UEFAllOff GDefault (MDE_ x -> MD_ dx x) where
  mkGenerable m = domUseEff antiUnit $ pure m

domEff'use :: forall v. Generable (UseEffTag "") GDefault v => v
domEff'use = g @(UseEffTag "")

domEff'use'' :: forall @at v. Generable (UseEffTag at) GDefault v => v
domEff'use'' = g @(UseEffTag at)

type XDom_domElement_ = forall dr x. MDomEl dr x Unit -> MDom dr x Unit

dom'element :: String -> XDom_domElement_
dom'element s m = do
  (propWFs /\ elBuild) <- g1 @XRunW @"attr" $ w'exec m
  let props = D.js_propsFromPropWs D.propWFKey D.propWFVal propWFs
  w'say $ D.js_renderEl s (encodeOpts props) elBuild

dom'text :: forall t dr x. SText t => t -> MDom dr x Unit
dom'text = w'say <<< D.js_textEl <<< stext

el'key :: forall dr x. String -> MDomEl dr x Unit
el'key = w'tell'' @"attr" <<< pure <<< D.PKey

el'href :: forall dr x. String -> MDomEl dr x Unit
el'href = w'tell'' @"attr" <<< pure <<< D.Href

el'cn :: forall dr x. String -> MDomEl dr x Unit
el'cn = w'tell'' @"attr" <<< pure <<< D.ClassName

el'cnW :: forall dr x. ((String -> StrW) -> StrW) -> MDomEl dr x Unit
el'cnW fm = el'cn $ joinStrW " " $ fm $ w'say

el'onClick :: forall dr x. (Int -> MDomEff x Unit) -> MDomEl dr x Unit
el'onClick f = do
  self <- el'getEffSelf
  w'tell'' @"attr" $ pure $ D.OnClick $ \e -> eval'self self $ f e

type XDom dr x a = MDom dr (XBASE x) a
type XDomA dx x a = XDom (async :: DomA | dx) x a
type XDomE e dx x a = XDom (except :: (DomE e) | dx) x a
type XDomEA e dx x a = XDom (except :: (DomE e), async :: DomA | dx) x a

type XDom_ dr x = T'_ $ XDom dr x
type XDomA_ dr x = T'_ $ XDom dr x
type XDomE_ e dr x = T'_ $ XDomE e dr x
type XDomEA_ e dr x = T'_ $ XDomEA e dr x

baseSelf :: Self () ()
baseSelf = Self (Runner id) (Runner id)

exec'xdom :: XDom_ () () -> D.ReactEl
exec'xdom = renderMEl $ selfDomless $ selfExtend runXBase baseSelf

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