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
  , xEl
  , xEls
  , xGetAttribute
  , xGoto
  , xGoto'
  , xInnerHtml
  , xInnerText
  , xNewPage
  , xSetViewport
  , xUseBrowser
  , xUseBrowser'
  , xWaitForSelector
  , xWaitForSelector'
  ) where

import Z.Prelude

---------- public api ------------------------------------------------

xUseBrowser
  :: forall x e a
   . (ResourceStage -> JsError -> e)
  -> Edit BrowserOpts
  -> (Browser -> EA e + E e x #> a)
  -> EA e x #> a
xUseBrowser mapE optsEdit fm = do
  let baseOpts = { exe: Nothing, args: [] }
  let opts = encodeOpts $ edit baseOpts optsEdit
  browser <- mkDim @MapE (mapE Acquire) $ launch opts
  res <- mkDim @Try (fm browser)
  mkDim @MapE (mapE Release) $ close browser
  mkDim @Ok res

xUseBrowser'
  :: forall x e a
   . (ResourceStage -> JsError -> e)
  -> (Browser -> EA e + E e x #> a)
  -> EA e x #> a
xUseBrowser' = arg2' pass xUseBrowser

xNewPage :: forall x. Browser -> EA JsError x #> Page
xNewPage = x' @"runEffPromise" <<< js_newPage

xGoto
  :: forall x. Page -> String -> Edit GotoOpts -> EA JsError x #> Unit
xGoto page url optsEdit = do
  let baseOpts = { waitUntil: Nothing }
  let opts = encodeOpts $ edit baseOpts optsEdit
  x' @"runEffPromise" $ js_goto url opts page

xGoto' :: forall x. Page -> String -> EA JsError x #> Unit
xGoto' = arg3' pass xGoto

xSetViewport
  :: forall x
   . Page
  -> Int
  -> Int
  -> EA JsError x #> Unit
xSetViewport page width height = do
  x' @"runEffPromise" $ js_setViewport width height page

xWaitForSelector
  :: forall x
   . Page
  -> String
  -> Edit WaitForOpts
  -> EA JsError x #> Unit
xWaitForSelector page sel optsEdit = do
  let baseOpts = { timeout: Nothing }
  let opts = encodeOpts $ edit baseOpts optsEdit
  x' @"runEffPromise" $ js_waitForSelector sel opts page

xWaitForSelector'
  :: forall x
   . Page
  -> String
  -> EA JsError x #> Unit
xWaitForSelector' = arg3' pass xWaitForSelector

xEls
  :: forall x o
   . IsPageOrElement o
  => o
  -> String
  -> EA JsError x #> Array Element
xEls pOrE sel = do
  els_ <- x' @"runEffPromise" $ js_els sel (asPageOrElement pOrE)
  pure $ els_ <#> \el_ -> Element ("(" <> context pOrE <> ")[]") el_

xEl
  :: forall x o
   . IsPageOrElement o
  => o
  -> String
  -> EA JsError x #> Element
xEl pOrE sel = do
  el_ <- x' @"runEffPromise" $ js_el sel (asPageOrElement pOrE)
  pure $ Element (context pOrE <> " |> ") el_

xInnerText
  :: forall x o
   . IsPageOrElement o
  => o
  -> EA JsError x #> String
xInnerText pOrE = x' @"runEffPromise" $ js_innerText (asPageOrElement pOrE)

xInnerHtml
  :: forall x o
   . IsPageOrElement o
  => o
  -> EA JsError x #> String
xInnerHtml pOrE = x' @"runEffPromise" $ js_innerHtml (asPageOrElement pOrE)

xGetAttribute
  :: forall x
   . Element
  -> String
  -> EA JsError x #> String
xGetAttribute (Element _ e) attr = x' @"runEffPromise" $ js_getAttribute e
  attr

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
launch = x' @"runEffPromise" <<< js_launchPuppeteer

close :: forall x. Browser -> EA JsError x #> Unit
close = x' @"runEffPromise" <<< js_browserClose

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
