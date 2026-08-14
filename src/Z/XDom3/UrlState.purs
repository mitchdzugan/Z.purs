module Z.XDom3.UrlState
  ( HrefSpec
  , RProvider
  , T
  , XProvider
  , actualHref
  , hrefFromUrl
  , mk
  , parsedHref
  , update
  ) where

import Z.Prelude

import Z.XDom3.Core as XD

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
hrefFromUrl url = Right $ urlToString url

mk :: (URL -> Maybe String) -> URL -> T
mk toTitleOr_ url =
  { href: hrefFromUrl url, url, titleOr_: toTitleOr_ url }

update :: (URL -> Maybe String) -> T -> String -> T
update toTitleOr_ state s = onParse $ urlFromString s
  where
  onParse (Just url) = mk toTitleOr_ url
  onParse _ =
    { href: Left { actual: s, parsed: parsedHref state }
    , url: state.url
    , titleOr_: toTitleOr_ state.url
    }

type RProvider dr x =
  (URL -> Maybe String) -> (T -> XD.MDom dr x Unit) -> XD.MDom dr x Unit

type XProvider dr x = RProvider dr (XBASE x)