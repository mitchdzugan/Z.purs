module Z.XDom2.Core
  ( DSRRdc(..)
  , DUEffT
  , DomBindE(..)
  , DomEffAct(..)
  , DomEffAsk(..)
  , DomPlusEnv(..)
  , DomRunR(..)
  , DomStateDispatch(..)
  , DomStateSet(..)
  , MDom
  , MDomEff
  , MDomEffV
  , MDomEl
  , MDomV
  , MElAttrV
  , XDom
  , XDomEff
  , XDomEl
  , dom
  , dom''bindE
  , dom''eff
  , dom''runR
  , dom'bindE
  , dom'eff
  , dom'runR
  , domEffR''act
  , domEffR''ask
  , domEffR''run
  , domEffR'act
  , domEffR'ask
  , domEffR'encapsulate
  , domEffR'run
  , domSelfExtend'
  , domState''dispatch
  , domState''get
  , domState''run
  , domState''runReducer
  , domState''set
  , domState'dispatch
  , domState'get
  , domState'run
  , domState'runReducer
  , domState'set
  , domUseEff
  , el
  , renderM
  ) where

import Z.Prelude
import Z.XDom2.Preact
import Z.XDom2.XSelf

import Record (get, insert)

type MDomV sx de = (self :: R' $ XSelf sx, effEnv :: R' de | sx)
type MDom sx de a = Run (Wa ReactEl $ MDomV sx de) a

type MElAttrV x = (attr :: W' $ Array PropWF | x)
type MDomEl sx de a = Run (MElAttrV $ Wa ReactEl $ Wa ReactEl $ MDomV sx de) a

type MDomEffV sx de = (domEff :: XRunsEffTagged, effEnv :: R' de | sx)
type MDomEff sx de a = Run (MDomEffV sx de) a

type XDom sx de a = MDom (XBASE sx) de a
type XDomEff sx de a = MDomEff (XBASE sx) de a
type XDomEl sx de a = MDomEl (XBASE sx) de a

dom
  :: { render :: XDom () {} Unit -> ReactEl
     , withKey :: XDom_withKey
     , withNewState :: XDom_withNewState
     , text :: forall t sx de. SText t => t -> MDom sx de Unit
     , div :: XDom_domElement_
     , button :: XDom_domElement_
     , a :: XDom_domElement_
     , iframe :: XDom_domElement_
     , article :: XDom_domElement_
     , pre :: XDom_domElement_
     , span :: XDom_domElement_
     , doEff ::
         forall x a
          . XEffTagged "domEff" a
         -> Run (domEff :: XRunsEffTagged | x) a
     }
dom =
  { render: renderMEl baseXSelf {}
  , withKey
  , withNewState
  , text: w''say <<< js_textEl <<< stext
  , div: domElement "div"
  , button: domElement "button"
  , a: domElement "a"
  , iframe: domElement "iframe"
  , article: domElement "article"
  , pre: domElement "pre"
  , span: domElement "span"
  , doEff: g1 @XDoTagged @"domEff"
  }

el
  :: { key :: forall sx de. String -> MDomEl sx de Unit
     , cn :: forall sx de. String -> MDomEl sx de Unit
     , cnW :: forall sx de. ((String -> StrW) -> StrW) -> MDomEl sx de Unit
     , href :: forall sx de. String -> MDomEl sx de Unit
     , onClick :: forall sx de. (Int -> MDomEff sx de Unit) -> MDomEl sx de Unit
     }
el =
  { key: w'tell @"attr" <<< pure <<< PKey
  , cn: w'tell @"attr" <<< pure <<< ClassName
  , cnW: \fm -> w'tell @"attr" $ pure $ ClassName $ joinStrW " " $ fm $ w''say
  , href: w'tell @"attr" <<< pure <<< Href
  , onClick: \f -> do
      effEnv <- r'ask @"effEnv"
      self' <- elSelfExtend' (eff'permit @"domEff" <<< r'run @"effEnv" effEnv)
      w'tell @"attr" $ pure $ OnClick $ \e -> runUnit self' $ f e
  }

--------------------------------------------------------------------------------

renderM :: forall sx de. XSelf sx -> de -> MDom sx de Unit -> Array ReactEl
renderM self effEnv m = runEls self
  $ w''exec
  $ r'run @"self" self
  $ r'run @"effEnv" effEnv
  $ m

renderMEl :: forall sx de. XSelf sx -> de -> MDom sx de Unit -> ReactEl
renderMEl self effEnv = js_renderFragment <<< renderM self effEnv

type XDom_withKey = forall sx de. String -> MDom sx de Unit -> MDom sx de Unit

withKey :: XDom_withKey
withKey k m = do
  self <- r'ask @"self"
  effEnv <- r'ask @"effEnv"
  w''say $ js_withKey k $ renderMEl self effEnv m

type XDom_withNewState =
  forall sx de s
   . s
  -> (s -> (s -> XEffTagged "domEff" Unit) -> MDom sx de Unit)
  -> MDom sx de Unit

withNewState :: XDom_withNewState
withNewState initalState fm = do
  self <- r'ask @"self"
  effEnv <- r'ask @"effEnv"
  w''say $ js_withState (renderFn self effEnv) initalState
  where
  renderFn self effEnv s ss = renderM self effEnv $ fm s (w ss)
  w ss s = tagEffX @"domEff" $ ss s

--------------------------------------------------------------------------------

domSelfExtend'
  :: forall sx' sx de
   . (forall a. Run sx a -> Run sx' a)
  -> MDom sx' de (XSelf sx)
domSelfExtend' m = r'ask @"self" <#> xSelfExtend' m

elSelfExtend'
  :: forall sx' sx de
   . (forall a. Run sx a -> Run sx' a)
  -> MDomEl sx' de (XSelf sx)
elSelfExtend' m = r'ask @"self" <#> xSelfExtend' m

--------------------------------------------------------------------------------

data DomRunR

instance
  ( GOrDefault "reader" gdesc rp
  , IsSymbol rp
  , Cons rp (R' r) sx' sx
  ) =>
  Generable DomRunR gdesc (r -> MDom sx de Unit -> MDom sx' de Unit) where
  mkGenerable env m = do
    effEnv <- r'ask @"effEnv"
    self' <- domSelfExtend' (r'run @rp env)
    w''tell $ renderM self' effEnv m

dom''runR :: forall v. Generable DomRunR GDefault v => v
dom''runR = g @DomRunR

dom'runR :: forall @at v. Generable DomRunR (G1 at) v => v
dom'runR = g1 @DomRunR @at

--------------------------------------------------------------------------------

data DomPlusEnv

instance
  ( GOrDefault "reader" gdesc rp
  , IsSymbol rp
  , Cons rp r de' de
  , Lacks rp de'
  ) =>
  Generable DomPlusEnv
    gdesc
    ( r
      -> MDom sx { | de } Unit
      -> MDom sx { | de' } Unit
    ) where
  mkGenerable env m = do
    currEnv <- r'ask @"effEnv"
    let nextEnv = insert (p @rp) env currEnv
    self <- r'ask @"self"
    w''tell $ renderM self nextEnv m

domEffR''run :: forall v. Generable DomPlusEnv GDefault v => v
domEffR''run = g @DomPlusEnv

domEffR'run :: forall @at v. Generable DomPlusEnv (G1 at) v => v
domEffR'run = g1 @DomPlusEnv @at

data DomEffAsk

instance
  ( GOrDefault "reader" gdesc rp
  , IsSymbol rp
  , Cons rp r de' de
  ) =>
  Generable DomEffAsk gdesc (Run (effEnv :: R' { | de } | x) r) where
  mkGenerable = r'ask @"effEnv" <#> get (p @rp)

domEffR''ask :: forall v. Generable DomEffAsk GDefault v => v
domEffR''ask = g @DomEffAsk

domEffR'ask :: forall @at v. Generable DomEffAsk (G1 at) v => v
domEffR'ask = g1 @DomEffAsk @at

data DomEffAct

instance
  ( GOrDefault "reader" gdesc rp
  , IsSymbol rp
  , Cons rp r de' de
  ) =>
  Generable DomEffAct
    gdesc
    ((r -> XEffTagged rp a) -> Run (effEnv :: R' { | de } | x) a) where
  mkGenerable getEff =
    r'ask @"effEnv" <#> get (p @rp) <#> getEff <#> (#) <#> useTag @rp

domEffR''act :: forall v. Generable DomEffAct GDefault v => v
domEffR''act = g @DomEffAct

domEffR'act :: forall @at v. Generable DomEffAct (G1 at) v => v
domEffR'act = g1 @DomEffAct @at

domEffR'encapsulate
  :: forall a de x. Run (effEnv :: R' { | de } | x) (XDomEff () { | de } a -> a)
domEffR'encapsulate =
  r'encapsulate @"effEnv" <#> flip (<<<) (eff'permit @"domEff")

--------------------------------------------------------------------------------

data DomBindE

instance
  ( Cons ep (E' e) sx' sx
  , IsSymbol ep
  , GOrDefault "except" gdesc ep
  ) =>
  Generable DomBindE
    gdesc
    ((e -> MDom sx' de Unit) -> MDom sx de Unit -> MDom sx' de Unit) where
  mkGenerable em m = do
    effEnv <- r'ask @"effEnv"
    self <- r'ask @"self"
    self' <- r'ask @"self" <#> xSelfExtend @(Either e)
      (g1 @XTry @ep)
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
    g @XSay $ js_withBoundedError (rErr self effEnv) (rMain self' effEnv)
    where
    rMain self effEnv = \_ -> renderMEl self effEnv m
    rErr self effEnv e = renderMEl self effEnv $ em e

dom''bindE :: forall v. Generable DomBindE GDefault v => v
dom''bindE = g @DomBindE

dom'bindE :: forall @at v. Generable DomBindE (G1 at) v => v
dom'bindE = g1 @DomBindE @at

--------------------------------------------------------------------------------

data DSRRdc

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' { get :: s, update :: a -> XEffTagged "domEff" Unit }) sx' sx
  ) =>
  Generable DSRRdc
    gdesc
    (s -> (s -> a -> s) -> MDom sx de Unit -> MDom sx' de Unit) where
  mkGenerable initState act m = do
    dom.withNewState initState \state set -> do
      let env = { update: set <<< act state, get: state }
      dom'runR @rp env m

domState''runReducer :: forall v. Generable DSRRdc GDefault v => v
domState''runReducer = g @DSRRdc

domState'runReducer
  :: forall @rp sx' sx de s a
   . IsSymbol rp
  => Cons rp (R' { get :: s, update :: a -> XEffTagged "domEff" Unit }) sx' sx
  => s
  -> (s -> a -> s)
  -> MDom sx de Unit
  -> MDom sx' de Unit
domState'runReducer initState act m = do
  dom.withNewState initState \state set -> do
    let env = { update: set <<< act state, get: state }
    dom'runR @rp env m

domState''run
  :: forall s r. Generable DSRRdc GDefault (s -> (s -> s -> s) -> r) => s -> r
domState''run initState = domState''runReducer initState (\_ newS -> newS)

domState'run
  :: forall @rp sx' sx de s
   . IsSymbol rp
  => Cons rp (R' { get :: s, update :: s -> XEffTagged "domEff" Unit }) sx' sx
  => s
  -> MDom sx de Unit
  -> MDom sx' de Unit
domState'run initState = domState'runReducer @rp initState (\_ newS -> newS)

data DomStateDispatch

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' { update :: a -> XEffTagged "domEff" Unit | rs })
      (domEff :: XRunsEffTagged | x')
      (domEff :: XRunsEffTagged | x)
  ) =>
  Generable DomStateDispatch
    gdesc
    (a -> Run (domEff :: XRunsEffTagged | x) Unit) where
  mkGenerable a = do
    r <- r'ask @rp
    dom.doEff $ r.update a

domState''dispatch :: forall v. Generable DomStateDispatch GDefault v => v
domState''dispatch = g @DomStateDispatch

domState'dispatch
  :: forall @rp a rs x' x
   . IsSymbol rp
  => Cons rp (R' { update :: a -> XEffTagged "domEff" Unit | rs })
       (domEff :: XRunsEffTagged | x')
       (domEff :: XRunsEffTagged | x)
  => a
  -> Run (domEff :: XRunsEffTagged | x) Unit
domState'dispatch a = do
  r <- r'ask @rp
  dom.doEff $ r.update a

data DomStateSet

instance
  ( GOrDefault "reader" gspec rp
  , IsSymbol rp
  , Cons rp (R' { get :: s, update :: s -> XEffTagged "domEff" Unit | rs })
      (domEff :: XRunsEffTagged | x')
      (domEff :: XRunsEffTagged | x)
  ) =>
  Generable DomStateSet
    gdesc
    (s -> Run (domEff :: XRunsEffTagged | x) Unit) where
  mkGenerable a = do
    r <- r'ask @rp
    dom.doEff $ r.update a

domState''set :: forall v. Generable DomStateSet GDefault v => v
domState''set = g @DomStateSet

domState'set :: forall @at v. Generable DomStateSet (G1 at) v => v
domState'set = g1 @DomStateSet @at

type DomState'get rp =
  forall s rs x' x
   . IsSymbol rp
  => Cons rp (R' { get :: s | rs }) x' x
  => Run x s

type WithSymAs s p t = IsSymbol p => TypeEquals p s => t p

domState''get :: forall p. WithSymAs "reader" p DomState'get
domState''get = domState'get @p

domState'get :: forall @rp. DomState'get rp
domState'get = r'ask @rp <#> _.get

--------------------------------------------------------------------------------

type DUEffFn sx de =
  Run (MDomEffV sx de) (Run (MDomEffV sx de) Unit) -> MDom sx de Unit

domUseEff
  :: forall sx de a. Eq a => a -> DUEffFn sx de
domUseEff comp on = do
  effEnv <- r'ask @"effEnv"
  self' <- domSelfExtend' (eff'permit @"domEff" <<< r'run @"effEnv" effEnv)
  w''say $ js_effComponent eq comp $ \_ ->
    let r' = runDisposable self' on in \_ -> runUnit self' (r' unit)

foreign import data DUEffT :: forall k. k -> Type

type DUEffFn_ sx de = MDomEff sx de Unit -> MDom sx de Unit

instance (Eq a) => Generable (DUEffT "") GDefault (a -> DUEffFn sx de) where
  mkGenerable = domUseEff

instance (Eq a) => Generable (DUEffT "1") GDefault (DUEffFn sx de) where
  mkGenerable = domUseEff unit

instance (Eq a) => Generable (DUEffT "*") GDefault (DUEffFn sx de) where
  mkGenerable = domUseEff antiUnit

instance (Eq a) => Generable (DUEffT "+") GDefault (a -> DUEffFn_ sx de) where
  mkGenerable a m = domUseEff a $ m *> pure (pure unit)

instance (Eq a) => Generable (DUEffT "+1") GDefault (DUEffFn_ sx de) where
  mkGenerable m = domUseEff unit $ m *> pure (pure unit)

instance (Eq a) => Generable (DUEffT "+*'") GDefault (DUEffFn_ sx de) where
  mkGenerable m = domUseEff antiUnit $ m *> pure (pure unit)

instance (Eq a) => Generable (DUEffT "-") GDefault (a -> DUEffFn_ sx de) where
  mkGenerable a m = domUseEff a $ pure m

instance (Eq a) => Generable (DUEffT "1-") GDefault (DUEffFn_ sx de) where
  mkGenerable m = domUseEff unit $ pure m

instance (Eq a) => Generable (DUEffT "*-'") GDefault (DUEffFn_ sx de) where
  mkGenerable m = domUseEff antiUnit $ pure m

dom''eff :: forall v. Generable (DUEffT "") GDefault v => v
dom''eff = g @(DUEffT "")

dom'eff :: forall @at v. Generable (DUEffT at) GDefault v => v
dom'eff = g @(DUEffT at)

type XDom_domElement_ = forall sx de. MDomEl sx de Unit -> MDom sx de Unit

domElement :: String -> XDom_domElement_
domElement s m = do
  (propWFs /\ elBuild) <- g1 @XRunW @"attr" $ g @XExecW m
  let props = js_propsFromPropWs propWFKey propWFVal propWFs
  g @XSay $ js_renderEl s (encodeOpts props) elBuild

