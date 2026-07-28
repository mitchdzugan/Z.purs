module Web.Z.Web.DOM
  ( XWeb
  , XWebF
  , runXWeb
  , xDocument
  , xExecAndExit
  , xGetElementById
  , xWindow
  ) where

import Z.Prelude

import Web.DOM.Internal.Types as T
import Web.DOM.NonElementParentNode as NEPN
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window as Window
import Effect.Unsafe as Unsafe

getElementById :: String -> HTMLDoc.HTMLDocument -> Effect (Maybe T.Element)
getElementById s = NEPN.getElementById s <<< HTMLDoc.toNonElementParentNode

xWindow :: forall x. XWeb x Window.Window
xWindow = lift _xWeb (WindowCmd id)

xDocument :: forall x. XWeb x HTMLDoc.HTMLDocument
xDocument = lift _xWeb (DocumentCmd id)

xGetElementById :: forall x. String -> XWeb x (Maybe T.Element)
xGetElementById s = lift _xWeb (GetElementByIdCmd s id)

type XWeb x a = X (xWeb :: XWebF | x) a

data XWebF a
  = WindowCmd (Window.Window -> a)
  | DocumentCmd (HTMLDoc.HTMLDocument -> a)
  | GetElementByIdCmd String (Maybe T.Element -> a)

handleXWeb :: forall r. XWebF ~> Run r
handleXWeb = case _ of
  WindowCmd f -> pure $ f (Unsafe.unsafePerformEffect HTML.window)
  DocumentCmd f -> pure $ f
    (Unsafe.unsafePerformEffect $ HTML.window >>= Window.document)
  GetElementByIdCmd id f -> pure $ f
    ( Unsafe.unsafePerformEffect $ HTML.window >>= Window.document >>=
        getElementById id
    )

derive instance functorXBaseF :: Functor XWebF

type XWEB x = (xWeb :: XWebF | x)

_xWeb = Proxy :: Proxy "xWeb"

runXWeb :: forall r. Run (XWEB + r) ~> Run r
runXWeb = run (on _xWeb handleXWeb send)

foreign import js_errorLog :: forall a. a -> Effect Unit

execAndExit :: forall e a. RtError e => Aff (Either e a) -> Effect Unit
execAndExit a = runAff_ onDone a
  where
  onDone (Left e) = do
    js_errorLog "process failed with UNHANDLED UNKNOWN error ⌄"
    js_errorLog e
  onDone (Right (Left e)) = do
    js_errorLog
      $ "process failed with known error [| "
      <> rtErrName e
      <> " |] ⌄"
    js_errorLog $ rtErrMessage e
  onDone _ = pure unit

type XWebEA e x = EA e (XWEB x)

xExecAndExit
  :: forall @w @e a. RtError e => XWa w (XWebEA e) a -> Effect Unit
xExecAndExit m = execAndExit $ xExecAff $ do
  w /\ res <- xListen $ expand $ runXWeb m
  when (arrSize w > 0) do
    xLogWarning "collected warnings ⌄"
    xLogWarning w
  pure res