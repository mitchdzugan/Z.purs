module Z.SSBM.Slp.DB.App where

import Z.XDom.Prelude

import Z.XDom.UrlState as UrlSt

data CountAction = Inc | Dec | Reset

countAct :: Int -> CountAction -> Int
countAct n Inc = n + 1
countAct n Dec = n - 1
countAct _ Reset = 3

type COUNT x = (count :: XDom'StateReducer CountAction Int | x)

app'onE :: forall dr x. String -> XDom dr (COUNT x) Unit
app'onE message = do
  dom'div do
    dom'text message
    dom'button do
      el'cn "btn btn-soft"
      el'onClick $ \_ -> domState'dispatch'' @"count" Reset
      dom'text "reset"

app'body :: forall dr x. XDomE String dr (COUNT x) Unit
app'body = do
  count <- domState'get'' @"count"
  dom'div $ dom'text "Hellllooooooo"
  dom'div do
    el'key "asdfasdfasdfasdf..."
    when (count < 0) do domError'fail "negative number invalid"
    dom'span %% w'str \t -> t "div with stuff" *> t (stext unit) *> t "!"
    dom'br $ pass
    dom'button do
      el'cnW \w -> do
        w "btn btn-soft" *> when (count > 5) do w "btn-accent"
      el'onClick $ (\_ -> domState'dispatch'' @"count" Dec)
      dom'text "dec"
    dom'div $ dom'text ("Count:" <+> count)
    dom'withKey "asdfasdf" $ dom'button do
      el'cnW \w -> do
        w "btn btn-soft" *> when (count > 5) do w "btn-accent"
      el'onClick \e -> do
        x'out e
        domState'dispatch'' @"count" Inc
      dom'text "inc"

app'mk :: forall dr x. UrlSt.XProvider dr x -> XDom dr x Unit
app'mk = router'run'' @"router" urlEdits parseUrl mkTitleOr_ do
  r <- router'routeOrE'' @"router"
  x'out $ show r
  dom'div do
    el'cn "flex flex-col gap-4"
    dom'iframe $ pure unit
    dom'div $ dom'a do
      router'setHref $ Profile "Jimmy"
      el'cn "btn link btn-primary"
      dom'text "profile link"
    domState'runReducer'' @"count" 3 countAct $ domError'bind app'onE app'body
  where
  mkTitleOr_ = Just <<< url'toString <<< _.url
  parseRouteL Nil = pure Home
  parseRouteL (Cons "profile" (Cons slug Nil)) = pure $ Profile slug
  parseRouteL _ = Left "Invalid Route"

  parseUrl :: URL -> Either String Route
  parseUrl u = parseRouteL $ list'fromFoldable $ url'pathSegments u

  urlEdits :: Route -> Edit UrlParts
  urlEdits (Profile s) = s'set'b @"path" [ "profile", s ]
  urlEdits _ = pure unit

data Route = Home | Profile String

derive instance Generic Route _
instance Show Route where
  show = genericShow
