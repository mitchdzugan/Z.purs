module Node.Z.Puppeteer.PuppeteerImpl
  ( Browser
  , Element_
  , Element(..)
  , Page
  , PageOrElement
  , WaitUntil(..)
  , asPageOrElement
  , class IsPageOrElement
  , context
  , el
  , els
  , getAttribute
  , goto
  , goto'
  , innerHtml
  , innerText
  , newPage
  , setViewport
  , useBrowser
  , useBrowser'
  , waitForSelector
  , waitForSelector'
  ) where

import Z.Prelude

---------- public api ------------------------------------------------

useBrowser
  :: forall x e a
   . (ResourceStage -> JsError -> e)
  -> Edit BrowserOpts
  -> (Browser -> EA e + E e x #> a)
  -> EA e x #> a
useBrowser mapE optsEdit fm = do
  let baseOpts = { exe: Nothing, args: [] }
  let opts = encodeOpts $ edit baseOpts optsEdit
  browser <- x MapE (mapE Acquire) $ launch opts
  res <- x Try $ fm browser
  x MapE (mapE Release) $ close browser
  x Ok res

useBrowser'
  :: forall x e a
   . (ResourceStage -> JsError -> e)
  -> (Browser -> EA e + E e x #> a)
  -> EA e x #> a
useBrowser' = arg2' default useBrowser

newPage :: forall x. Browser -> EA JsError x #> Page
newPage = x RunEffPromise <<< js_newPage

goto
  :: forall x. Page -> String -> Edit GotoOpts -> EA JsError x #> Unit
goto page url optsEdit = do
  let baseOpts = { waitUntil: Nothing }
  let opts = encodeOpts $ edit baseOpts optsEdit
  x RunEffPromise $ js_goto url opts page

goto' :: forall x. Page -> String -> EA JsError x #> Unit
goto' = arg3' default goto

setViewport
  :: forall x
   . Page
  -> Int
  -> Int
  -> EA JsError x #> Unit
setViewport page width height = do
  x RunEffPromise $ js_setViewport width height page

waitForSelector
  :: forall x
   . Page
  -> String
  -> Edit WaitForOpts
  -> EA JsError x #> Unit
waitForSelector page sel optsEdit = do
  let baseOpts = { timeout: Nothing }
  let opts = encodeOpts $ edit baseOpts optsEdit
  x RunEffPromise $ js_waitForSelector sel opts page

waitForSelector'
  :: forall x
   . Page
  -> String
  -> EA JsError x #> Unit
waitForSelector' = arg3' default waitForSelector

els
  :: forall x o
   . IsPageOrElement o
  => o
  -> String
  -> EA JsError x #> Array Element
els pOrE sel = do
  els_ <- x RunEffPromise $ js_els sel (asPageOrElement pOrE)
  pure $ els_ <#> \el_ -> Element ("(" <> context pOrE <> ")[]") el_

el
  :: forall x o
   . IsPageOrElement o
  => o
  -> String
  -> EA JsError x #> Element
el pOrE sel = do
  el_ <- x RunEffPromise $ js_el sel (asPageOrElement pOrE)
  pure $ Element (context pOrE <> " |> ") el_

innerText
  :: forall x o
   . IsPageOrElement o
  => o
  -> EA JsError x #> String
innerText pOrE = x RunEffPromise $ js_innerText (asPageOrElement pOrE)

innerHtml
  :: forall x o
   . IsPageOrElement o
  => o
  -> EA JsError x #> String
innerHtml pOrE = x RunEffPromise $ js_innerHtml (asPageOrElement pOrE)

getAttribute
  :: forall x
   . Element
  -> String
  -> EA JsError x #> String
getAttribute (Element _ e) attr = x RunEffPromise $ js_getAttribute e attr

-------------- foreign data imports -----------------------------------

foreign import data Browser :: Type
foreign import data Page :: Type
foreign import data Element_ :: Type
foreign import data PageOrElement :: Type

data Element = Element String Element_

-------------- foreign imports ----------------------------------------

foreign import js_launchPuppeteer :: Json -> Effect (Promise Browser)

foreign import js_browserClose :: Browser -> Effect (Promise Unit)

foreign import js_newPage :: Browser -> Effect (Promise Page)

foreign import js_setViewport :: Int -> Int -> Page -> Effect (Promise Unit)

foreign import js_goto
  :: String -> Json -> Page -> Effect (Promise Unit)

foreign import js_waitForSelector
  :: String -> Json -> Page -> Effect (Promise Unit)

foreign import js_els
  :: String -> PageOrElement -> Effect (Promise (Array Element_))

foreign import js_el :: String -> PageOrElement -> Effect (Promise Element_)

foreign import js_innerText :: PageOrElement -> Effect (Promise String)

foreign import js_innerHtml :: PageOrElement -> Effect (Promise String)

foreign import js_getAttribute
  :: Element_ -> String -> Effect (Promise String)

foreign import js_PageOrElement_P :: Page -> PageOrElement

foreign import js_PageOrElement_E :: Element_ -> PageOrElement

-------------- internal impls -----------------------------------------

launch :: forall x. Json -> EA JsError x #> Browser
launch = x RunEffPromise <<< js_launchPuppeteer

close :: forall x. Browser -> EA JsError x #> Unit
close = x RunEffPromise <<< js_browserClose

-------------- internal types -----------------------------------------

class IsPageOrElement a where
  asPageOrElement :: a -> PageOrElement
  context :: a -> String

instance pageIsPageOrElement :: IsPageOrElement Page where
  asPageOrElement = js_PageOrElement_P
  context _ = ""

instance elementIsPageOrElement :: IsPageOrElement Element_ where
  asPageOrElement = js_PageOrElement_E
  context _ = ""

instance element__IsPageOrElement :: IsPageOrElement Element where
  asPageOrElement (Element _ e) = js_PageOrElement_E e
  context (Element x _) = x

type BrowserOpts =
  { exe :: Maybe String
  , args :: Array String
  }

data WaitUntil = DOMContentLoaded

instance encodeWaitUntil :: EncodeJson WaitUntil where
  encodeJson DOMContentLoaded = encodeJson "domcontentloaded"

type GotoOpts =
  { waitUntil :: Maybe WaitUntil
  }

type WaitForOpts =
  { timeout :: Maybe Int
  }
