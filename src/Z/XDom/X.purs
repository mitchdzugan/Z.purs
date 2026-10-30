module Z.XDom.X
  ( AttrW'
  , DomW'
  , DomW'ParentId
  , Eff'AttrW
  , Eff'DomW
  , R'AttrW(..)
  , R'DomW(..)
  , ReactEls
  , ReactEls2d
  , X'AttrW
  , X'DomW
  , attrW'addAttr
  , attrW'addClassname
  , attrW'addStyle
  , attrW'exec
  , attrW'resetClassnames
  , attrW'resetStyles
  , attrW'run
  , domW'concat
  , domW'exec
  , domW'finish
  , domW'push
  , domW'run
  , domW'runWithCompleted
  , domW'start
  ) where

import Z.Prelude

import Z.XDom.Preact (ReactEl)
import Z.Z.X.Core (class X'Results'R'Eff)

type ReactEls = Array ReactEl
type ReactEls2d = Array ReactEls

foreign import data Eff'DomW :: Type

foreign import data DomW'ParentId :: Type

newtype R'DomW = R'DomW
  { push :: ReactEl -> Effect Unit
  , start :: Effect DomW'ParentId
  , finish :: DomW'ParentId -> Effect ReactEls
  , currentEls :: Effect ReactEls
  }

derive instance Newtype R'DomW _

------------------------------------------------------------

instance X'Readable R'DomW ReactEls2d where
  x'readable'mk _ = do
    st <- js2_domwNew
    pure $ R'DomW
      { push: flip js2_domwPush st
      , start: js2_domwStart st
      , finish: flip js2_domwFinish st
      , currentEls: js2_domwCurrEls st
      }

instance X'Results'R'Eff R'DomW ReactEls where
  x'results'r'eff'impl = _.currentEls <<< unwrap

------------------------------------------------------------

type X'DomW = Reader (R'DomW)

type DomW' x = (domW :: X'DomW | x)

domW'runWithCompleted
  :: forall x a. ReactEls2d -> Run (DomW' x) a -> Run x (a /\ ReactEls)
domW'runWithCompleted completed m =
  x'run @"domW" @X'DomW completed m

domW'run :: forall x a. Run (DomW' x) a -> Run x (a /\ ReactEls)
domW'run = domW'runWithCompleted []

domW'exec :: forall x. Run (DomW' x) Unit -> Run x ReactEls
domW'exec m = domW'runWithCompleted [] m <#> snd

domW'push :: forall x. ReactEl -> Run (DomW' x) Unit
domW'push = x'effCall @"domW" @"push"

domW'concat :: forall x. ReactEls -> Run (DomW' x) Unit
domW'concat els = forM_ els domW'push

domW'start :: forall x. Run (DomW' x) DomW'ParentId
domW'start = x'effGet @"domW" @"start"

domW'finish :: forall x. DomW'ParentId -> Run (DomW' x) ReactEls
domW'finish = x'effCall @"domW" @"finish"

-------------------------------------------------------------------------

foreign import js2_domwNew :: Effect Eff'DomW

foreign import js2_domwCurrEls :: Eff'DomW -> Effect ReactEls

foreign import js2_domwStart :: Eff'DomW -> Effect DomW'ParentId

foreign import js2_domwFinish :: DomW'ParentId -> Eff'DomW -> Effect ReactEls

foreign import js2_domwPush :: ReactEl -> Eff'DomW -> Effect Unit

-------------------------------------------------------------------------

foreign import data Eff'AttrW :: Type

newtype R'AttrW = R'AttrW
  { attrx :: Eff'AttrW
  , resetClassnames :: Effect Unit
  , resetStyles :: Effect Unit
  , addAttr :: (String /\ JsAny) -> Effect Unit
  , addClassname :: String -> Effect Unit
  , addStyle :: (String /\ String) -> Effect Unit
  }

derive instance Newtype R'AttrW _

instance X'Readable R'AttrW Unit where
  x'readable'mk _ = do
    attrx <- js_attrwNew
    pure $ R'AttrW
      { attrx
      , resetClassnames: js_attrwResetClassnames attrx
      , resetStyles: js_attrwResetStyles attrx
      , addAttr: \(k /\ v) -> js_attrwAddAttr k v attrx
      , addClassname: \cn -> js_attrwAddClassname cn attrx
      , addStyle: \(k /\ v) -> js_attrwAddStyle k v attrx
      }

instance X'Results'R'Eff R'AttrW Json where
  x'results'r'eff'impl (R'AttrW { attrx }) = js_attrwResults attrx

------------------------------------------------------------

type X'AttrW = Reader R'AttrW

type AttrW' x = (attrW :: X'AttrW | x)

attrW'run
  :: forall x a
   . Run (AttrW' x) a
  -> Run x (a /\ Json)
attrW'run m = x'run_ @"attrW" @(X'AttrW) m

attrW'exec :: forall x. Run (AttrW' x) Unit -> Run x Json
attrW'exec m = attrW'run m <#> snd

attrW'resetClassnames :: forall x. Run (AttrW' x) Unit
attrW'resetClassnames = x'effGet @"attrW" @"resetClassnames"

attrW'resetStyles :: forall x. Run (AttrW' x) Unit
attrW'resetStyles = x'effGet @"attrW" @"resetStyles"

attrW'addAttr :: forall x. String -> JsAny -> Run (AttrW' x) Unit
attrW'addAttr = curry $ x'effCall @"attrW" @"addAttr"

attrW'addClassname :: forall x. String -> Run (AttrW' x) Unit
attrW'addClassname = x'effCall @"attrW" @"addClassname"

attrW'addStyle :: forall x. String -> String -> Run (AttrW' x) Unit
attrW'addStyle = curry $ x'effCall @"attrW" @"addStyle"

------------------------------------------------------------

foreign import js_attrwNew :: Effect Eff'AttrW

foreign import js_attrwResults :: Eff'AttrW -> Effect Json

foreign import js_attrwResetClassnames :: Eff'AttrW -> Effect Unit

foreign import js_attrwResetStyles :: Eff'AttrW -> Effect Unit

foreign import js_attrwAddAttr :: String -> JsAny -> Eff'AttrW -> Effect Unit

foreign import js_attrwAddClassname :: String -> Eff'AttrW -> Effect Unit

foreign import js_attrwAddStyle :: String -> String -> Eff'AttrW -> Effect Unit