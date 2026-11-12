module Z.XDom.X
  ( AttrW'
  , Eff'AttrW
  , R'AttrW(..)
  , X'AttrW
  , attrW'addAttr
  , attrW'addClassname
  , attrW'addStyle
  , attrW'exec
  , attrW'resetClassnames
  , attrW'resetStyles
  , attrW'run
  ) where

import Z.Prelude

import Z.Z.X.Core (class X'Results'R'Eff)

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

type X'AttrW = R'AttrW

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