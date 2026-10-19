module Z.XDom.UrlState
  ( HrefSpec
  , RProvider
  , T
  , XProvider
  , actualHref
  , hrefFromUrl
  , mk
  , mkProvider
  , parsedHref
  , update
  , useProvider
  , xProvideStaticHistoryX
  ) where

import Z.Prelude

import Z.XDom.Core as XD

type HrefSpec = Either { actual :: String, parsed :: String } String

type T =
  { href :: HrefSpec
  , url :: URL
  , titleOr_ :: Maybe String
  }

actualHref :: T -> String
actualHref { href: Left { actual } } = actual
actualHref { href: Right actual } = actual

parsedHref :: T -> String
parsedHref { href: Left { parsed } } = parsed
parsedHref { href: Right parsed } = parsed

hrefFromUrl :: URL -> HrefSpec
hrefFromUrl url = Right $ url'toString url

mk :: (URL -> Maybe String) -> URL -> T
mk toTitleOr_ url =
  { href: hrefFromUrl url, url, titleOr_: toTitleOr_ url }

update :: (URL -> Maybe String) -> T -> String -> T
update toTitleOr_ state s = onParse $ url'fromString s
  where
  onParse (Just url) = mk toTitleOr_ url
  onParse _ =
    { href: Left { actual: s, parsed: parsedHref state }
    , url: state.url
    , titleOr_: toTitleOr_ state.url
    }

type ProvideFn dr x =
  (URL -> Maybe String) -> (T -> XD.MDom dr x Unit) -> XD.MDom dr x Unit

newtype RProvider dr x = RProvider
  { getBaseUrl :: XD.MDom dr x URL
  , provide :: ProvideFn dr x
  }

mkProvider
  :: forall dr x. XD.MDom dr x URL -> (URL -> ProvideFn dr x) -> RProvider dr x
mkProvider getBaseUrl mkProvide = RProvider
  { getBaseUrl
  , provide: \toTitleOr_ render -> getBaseUrl >>= \baseUrl ->
      mkProvide baseUrl toTitleOr_ render
  }

useProvider
  :: forall dr x
   . RProvider dr x
  -> (URL -> Maybe String)
  -> (URL -> T -> XD.MDom dr x Unit)
  -> XD.MDom dr x Unit
useProvider (RProvider { getBaseUrl, provide }) toTitleOr_ render = do
  baseUrl <- getBaseUrl
  provide toTitleOr_ (render baseUrl)

type XProvider dr x = RProvider dr (X'Base x)

xProvideStaticHistoryX
  :: forall dr x
   . URL
  -> (URL -> Maybe String)
  -> (T -> XD.MDom dr x Unit)
  -> XD.MDom dr x Unit
xProvideStaticHistoryX url toTitleOr_ = (#) (mk toTitleOr_ url)