module Z.Puppeteer.Node.Module
  ( Browser
  , Element
  , Page
  , PageOrElement
  , WaitUntil(..)
  , asPageOrElement
  , class IsPageOrElement
  , el
  , els
  , getAttribute
  , goto
  , goto'
  , innerText
  , newPage
  , setViewport
  , useBrowser
  , useBrowser'
  , waitForSelector
  , waitForSelector'
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
  let opts = Z.encodeOpts $ Z.edit baseOpts optsEdit
  browser <- Z.xMapE (mapE Z.Acquire) $ launch opts
  res <- Z.xTry $ fm browser
  Z.xMapE (mapE Z.Release) $ close browser
  Z.xOk res

useBrowser'
  :: forall x e a
   . (Z.ResourceStage -> Z.JsError -> e)
  -> (Browser -> Z.X (Z.EA e (Z.E e x)) a)
  -> Z.X (Z.EA e x) a
useBrowser' = Z.arg2' Z.default useBrowser

newPage :: forall x. Browser -> Z.X (Z.EA Z.JsError x) Page
newPage = Z.xEffectPromise <<< js_newPage

goto
  :: forall x. Page -> String -> Z.Edit GotoOpts -> Z.X (Z.EA Z.JsError x) Unit
goto page url optsEdit = do
  let baseOpts = { waitUntil: Z.Nothing }
  let opts = Z.encodeOpts $ Z.edit baseOpts optsEdit
  Z.xEffectPromise $ js_goto url opts page

goto' :: forall x. Page -> String -> Z.X (Z.EA Z.JsError x) Unit
goto' = Z.arg3' Z.default goto

setViewport
  :: forall x
   . Page
  -> Int
  -> Int
  -> Z.X (Z.EA Z.JsError x) Unit
setViewport page width height = do
  Z.xEffectPromise $ js_setViewport width height page

waitForSelector
  :: forall x
   . Page
  -> String
  -> Z.Edit WaitForOpts
  -> Z.X (Z.EA Z.JsError x) Unit
waitForSelector page sel optsEdit = do
  let baseOpts = { timeout: Z.Nothing }
  let opts = Z.encodeOpts $ Z.edit baseOpts optsEdit
  Z.xEffectPromise $ js_waitForSelector sel opts page

waitForSelector'
  :: forall x
   . Page
  -> String
  -> Z.X (Z.EA Z.JsError x) Unit
waitForSelector' = Z.arg3' Z.default waitForSelector

els
  :: forall x o
   . IsPageOrElement o
  => o
  -> String
  -> Z.X (Z.EA Z.JsError x) (Array Element)
els pOrE sel = Z.xEffectPromise $ js_els sel (asPageOrElement pOrE)

el
  :: forall x o
   . IsPageOrElement o
  => o
  -> String
  -> Z.X (Z.EA Z.JsError x) Element
el pOrE sel = Z.xEffectPromise $ js_el sel (asPageOrElement pOrE)

innerText
  :: forall x o
   . IsPageOrElement o
  => o
  -> Z.X (Z.EA Z.JsError x) String
innerText pOrE = Z.xEffectPromise $ js_innerText (asPageOrElement pOrE)

getAttribute
  :: forall x
   . Element
  -> String
  -> Z.X (Z.EA Z.JsError x) String
getAttribute elem attr = Z.xEffectPromise $ js_getAttribute elem attr

-------------- foreign data imports -----------------------------------

foreign import data Browser :: Type
foreign import data Page :: Type
foreign import data Element :: Type
foreign import data PageOrElement :: Type

-------------- foreign imports ----------------------------------------

foreign import js_launchPuppeteer :: Z.Json -> Z.Effect (Z.Promise Browser)

foreign import js_browserClose :: Browser -> Z.Effect (Z.Promise Unit)

foreign import js_newPage :: Browser -> Z.Effect (Z.Promise Page)

foreign import js_setViewport :: Int -> Int -> Page -> Z.Effect (Z.Promise Unit)

foreign import js_goto
  :: String -> Z.Json -> Page -> Z.Effect (Z.Promise Unit)

foreign import js_waitForSelector
  :: String -> Z.Json -> Page -> Z.Effect (Z.Promise Unit)

foreign import js_els
  :: String -> PageOrElement -> Z.Effect (Z.Promise (Array Element))

foreign import js_el :: String -> PageOrElement -> Z.Effect (Z.Promise Element)

foreign import js_innerText :: PageOrElement -> Z.Effect (Z.Promise String)

foreign import js_getAttribute
  :: Element -> String -> Z.Effect (Z.Promise String)

foreign import js_PageOrElement_P :: Page -> PageOrElement

foreign import js_PageOrElement_E :: Element -> PageOrElement

-------------- internal impls -----------------------------------------

launch :: forall x. Z.Json -> Z.X (Z.EA Z.JsError x) Browser
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

data WaitUntil = DOMContentLoaded

instance encodeWaitUntil :: Z.EncodeJson WaitUntil where
  encodeJson DOMContentLoaded = Z.encodeJson "domcontentloaded"

type GotoOpts =
  { waitUntil :: Z.Maybe WaitUntil
  }

type WaitForOpts =
  { timeout :: Z.Maybe Int }
