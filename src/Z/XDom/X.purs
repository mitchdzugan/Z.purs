module Z.XDom.X
  ( AttrW'
  , DomElProps
  , DomW'
  , R'AttrW
  , R'DomW
  , ReactEls
  , ReactEls2d
  , X'AttrW
  , X'DomW
  , attrW'addAttr
  , attrW'addChildren
  , attrW'addClassname
  , attrW'addStyle
  , attrW'exec
  , attrW'extract
  , attrW'resetChildren
  , attrW'resetClassnames
  , attrW'resetStyles
  , attrW'run
  , domW'concat
  , domW'exec
  , domW'extractAndResume
  , domW'pause
  , domW'push
  , domW'resume
  , domW'run
  , domW'runWithCompleted
  ) where

import Z.Prelude

import Data.Functor.Variant (match)
import Z.XDom.Preact (PropWF, ReactEl, propWFKey, propWFVal)
import Z.Z.X.Responds
  ( Responds
  , Responds'Const
  , responds'const'eff
  , responds'run'eff
  )

type ReactEls = Array ReactEl
type ReactEls2d = Array ReactEls

foreign import data R'DomW :: Type

------------------------------------------------------------

instance X'Readable'rw R'DomW ReactEls2d where
  x'readable'rw'mk = js_domwNew

instance X'Results'R'rw R'DomW ReactEls where
  x'results'r'rw'impl = js_domwResults

instance
  X'R'RespondsTo'rw R'DomW
    ( VariantF
        ( extract :: Responds'Const ReactEls
        )
    )
    ( VariantF
        ( resume :: Responds ReactEls2d Unit
        , push :: Responds ReactEl Unit
        , concat :: Responds ReactEls Unit
        , pause :: Responds'Const ReactEls2d
        )
    )
    ( self :: t
    ) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r domx = match
    { extract: responds'const'eff $ js_domwResults domx
    }
  x'r'mkResponds'w domx = match
    { resume: responds'run'eff \v -> js_domwResume v domx
    , push: responds'run'eff \v -> js_domwPush v domx
    , concat: responds'run'eff \v -> js_domwConcat v domx
    , pause: responds'const'eff $ js_domwPause domx
    }

type X'DomW = Reader (RW'Tagged R'DomW)

type DomW' x = (domW :: X'DomW | x)

domW'runWithCompleted
  :: forall x a. ReactEls2d -> Run (DomW' x) a -> Run x (a /\ ReactEls)
domW'runWithCompleted completed m = x'run @"domW" @X'DomW completed m

domW'run :: forall x a. Run (DomW' x) a -> Run x (a /\ ReactEls)
domW'run = domW'runWithCompleted []

domW'exec :: forall x. Run (DomW' x) Unit -> Run x ReactEls
domW'exec m = domW'runWithCompleted [] m <#> snd

domW'push :: forall x. ReactEl -> Run (DomW' x) Unit
domW'push = x'respondTo @"domW" @"push"

domW'concat :: forall x. ReactEls -> Run (DomW' x) Unit
domW'concat = x'respondTo @"domW" @"concat"

domW'pause :: forall x. Run (DomW' x) ReactEls2d
domW'pause = x'respondTo_ @"domW" @"pause"

domW'resume :: forall x. ReactEls2d -> Run (DomW' x) Unit
domW'resume = x'respondTo @"domW" @"resume"

domW'extract :: forall x. Run (DomW' x) ReactEls
domW'extract = x'respondTo_ @"domW" @"extract"

domW'extractAndResume :: forall x. ReactEls2d -> Run (DomW' x) ReactEls
domW'extractAndResume paused = do
  els <- domW'extract
  domW'resume paused
  pure els

------------------------------------------------------------

foreign import js_domwNew :: ReactEls2d -> Effect R'DomW

foreign import js_domwResults :: R'DomW -> Effect ReactEls

foreign import js_domwPause :: R'DomW -> Effect ReactEls2d

foreign import js_domwResume :: ReactEls2d -> R'DomW -> Effect Unit

foreign import js_domwPush :: ReactEl -> R'DomW -> Effect Unit

foreign import js_domwConcat :: ReactEls -> R'DomW -> Effect Unit

-------------------------------------------------------------------------

type DomElProps = { children :: ReactEls, attrs :: Json }

foreign import data Eff'AttrW :: Type

newtype R'AttrW t = R'AttrW { attrx :: Eff'AttrW, toChildren :: t -> ReactEls }

------------------------------------------------------------

instance X'Readable'rw (R'AttrW t) (t -> ReactEls) where
  x'readable'rw'mk toChildren = do
    attrx <- js_attrwNew
    pure $ R'AttrW { attrx, toChildren }

instance X'Results'R'rw (R'AttrW t) DomElProps where
  x'results'r'rw'impl (R'AttrW { attrx }) = js_attrwResults attrx

instance
  X'R'RespondsTo'rw (R'AttrW t)
    ( VariantF
        ( extract :: Responds'Const DomElProps
        )
    )
    ( VariantF
        ( resetClassnames :: Responds'Const Unit
        , resetChildren :: Responds'Const Unit
        , resetStyles :: Responds'Const Unit
        , addAttr :: Responds PropWF Unit
        , addClassname :: Responds String Unit
        , addChildren :: Responds t Unit
        , addStyle :: Responds (String /\ String) Unit
        )
    )
    ( self :: t
    ) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r (R'AttrW { attrx }) = match
    { extract: responds'const'eff $ js_attrwResults attrx
    }
  x'r'mkResponds'w (R'AttrW { attrx, toChildren }) = match
    { resetClassnames: responds'const'eff $ js_attrwResetClassnames attrx
    , resetChildren: responds'const'eff $ js_attrwResetChildren attrx
    , resetStyles: responds'const'eff $ js_attrwResetStyles attrx
    , addAttr: responds'run'eff \p ->
        js_attrwAddAttr (propWFKey p) (propWFVal p) attrx
    , addClassname: responds'run'eff \cn -> js_attrwAddClassname cn attrx
    , addChildren: responds'run'eff \els ->
        js_attrwAddChildren (toChildren els) attrx
    , addStyle: responds'run'eff \(k /\ v) -> js_attrwAddStyle k v attrx
    }

type X'AttrW t = Reader (RW'Tagged (R'AttrW t))

type AttrW' t x = (attrW :: X'AttrW t | x)

attrW'run
  :: forall @t x a
   . (t -> ReactEls)
  -> Run (AttrW' t x) a
  -> Run x (a /\ DomElProps)
attrW'run f m = x'run @"attrW" @(X'AttrW t) f m

attrW'exec
  :: forall @t x. (t -> ReactEls) -> Run (AttrW' t x) Unit -> Run x DomElProps
attrW'exec f m = attrW'run @t f m <#> snd

attrW'resetClassnames :: forall t x. Run (AttrW' t x) Unit
attrW'resetClassnames = x'respondTo_ @"attrW" @"resetClassnames"

attrW'resetChildren :: forall t x. Run (AttrW' t x) Unit
attrW'resetChildren = x'respondTo_ @"attrW" @"resetChildren"

attrW'resetStyles :: forall t x. Run (AttrW' t x) Unit
attrW'resetStyles = x'respondTo_ @"attrW" @"resetStyles"

attrW'addAttr :: forall t x. PropWF -> Run (AttrW' t x) Unit
attrW'addAttr = x'respondTo @"attrW" @"addAttr"

attrW'addClassname :: forall t x. String -> Run (AttrW' t x) Unit
attrW'addClassname = x'respondTo @"attrW" @"addClassname"

attrW'addChildren :: forall t x. t -> Run (AttrW' t x) Unit
attrW'addChildren = x'respondTo @"attrW" @"addChildren"

attrW'addStyle :: forall t x. String -> String -> Run (AttrW' t x) Unit
attrW'addStyle k v = x'respondTo @"attrW" @"addStyle" (k /\ v)

attrW'extract :: forall t x. Run (AttrW' t x) DomElProps
attrW'extract = x'respondTo_ @"attrW" @"extract"

------------------------------------------------------------

foreign import js_attrwNew :: Effect Eff'AttrW

foreign import js_attrwResults :: Eff'AttrW -> Effect DomElProps

foreign import js_attrwResetClassnames :: Eff'AttrW -> Effect Unit

foreign import js_attrwResetChildren :: Eff'AttrW -> Effect Unit

foreign import js_attrwResetStyles :: Eff'AttrW -> Effect Unit

foreign import js_attrwAddAttr :: String -> JsAny -> Eff'AttrW -> Effect Unit

foreign import js_attrwAddClassname :: String -> Eff'AttrW -> Effect Unit

foreign import js_attrwAddChildren :: ReactEls -> Eff'AttrW -> Effect Unit

foreign import js_attrwAddStyle :: String -> String -> Eff'AttrW -> Effect Unit