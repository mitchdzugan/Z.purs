module Z.Puppeteer.Node.Module
  ( Browser
  , Element
  , Page
  , PageOrElement
  , asPageOrElement
  , class IsPageOrElement
  , el
  , els
  , goto
  , newPage
  , useBrowser
  , waitForSelector
  ) where

import Prelude

import Z as Z

---------- public api ------------------------------------------------

useBrowser
  :: forall x e a
   . (Z.ResourceStage -> Z.JsError -> e)
  -> Z.Edit BrowserOpts
  -> (Browser -> Z.X (Z.EA e (Z.E e x)) a)
  -> Z.X (Z.EA e x) a
useBrowser mapE optsEdit fm = do
  let baseOpts = { exe: Z.Nothing, args: [] }
  browser <- Z.xMapE (mapE Z.Acquire) $ launch $ Z.edit baseOpts optsEdit
  res <- Z.xTry $ fm browser
  Z.xMapE (mapE Z.Release) $ close browser
  Z.xOk res

newPage :: forall x. Browser -> Z.X (Z.EA Z.JsError x) Page
newPage = Z.xEffectPromise <<< js_newPage

goto
  :: forall x. String -> Z.Edit GotoOpts -> Page -> Z.X (Z.EA Z.JsError x) Unit
goto url _optsEdit page = do
  Z.xEffectPromise $ js_goto url {} page

waitForSelector
  :: forall x
   . String
  -> Z.Edit WaitForOpts
  -> Page
  -> Z.X (Z.EA Z.JsError x) Unit
waitForSelector sel _optsEdit page = do
  Z.xEffectPromise $ js_waitForSelector sel {} page

els
  :: forall x o
   . IsPageOrElement o
  => String
  -> o
  -> Z.X (Z.EA Z.JsError x) (Array Element)
els sel pOrE = Z.xEffectPromise $ js_els sel (asPageOrElement pOrE)

el
  :: forall x o
   . IsPageOrElement o
  => String
  -> o
  -> Z.X (Z.EA Z.JsError x) Element
el sel pOrE = Z.xEffectPromise $ js_el sel (asPageOrElement pOrE)

-------------- foreign data imports -----------------------------------

foreign import data Browser :: Type
foreign import data Page :: Type
foreign import data Element :: Type
foreign import data PageOrElement :: Type

-------------- foreign imports ----------------------------------------

foreign import js_launchPuppeteer :: BrowserOpts -> Z.Effect (Z.Promise Browser)

foreign import js_browserClose :: Browser -> Z.Effect (Z.Promise Unit)

foreign import js_newPage :: Browser -> Z.Effect (Z.Promise Page)

foreign import js_setViewport :: Int -> Int -> Page -> Z.Effect (Z.Promise Unit)

foreign import js_goto
  :: String -> GotoOpts -> Page -> Z.Effect (Z.Promise Unit)

foreign import js_waitForSelector
  :: String -> WaitForOpts -> Page -> Z.Effect (Z.Promise Unit)

foreign import js_els
  :: String -> PageOrElement -> Z.Effect (Z.Promise (Array Element))

foreign import js_el :: String -> PageOrElement -> Z.Effect (Z.Promise Element)

foreign import js_PageOrElement_P :: Page -> PageOrElement

foreign import js_PageOrElement_E :: Element -> PageOrElement

-------------- internal impls -----------------------------------------

launch :: forall x. BrowserOpts -> Z.X (Z.EA Z.JsError x) Browser
launch = Z.xEffectPromise <<< js_launchPuppeteer

close :: forall x. Browser -> Z.X (Z.EA Z.JsError x) Unit
close = Z.xEffectPromise <<< js_browserClose

-------------- internal types -----------------------------------------

class IsPageOrElement a where
  asPageOrElement :: a -> PageOrElement

instance pageIsPageOrElement :: IsPageOrElement Page where
  asPageOrElement = js_PageOrElement_P

instance elementIsPageOrElement :: IsPageOrElement Element where
  asPageOrElement = js_PageOrElement_E

type BrowserOpts =
  { exe :: Z.Maybe String
  , args :: Array String
  }

type GotoOpts =
  {}

type WaitForOpts =
  {}
