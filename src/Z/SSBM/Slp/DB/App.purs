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
  dom'Div $ pure do
    dom'text message
    dom'Button do
      el'Cn "btn btn-soft"
      el'OnClick $ \_ -> domState'dispatch'' @"count" Reset
      pure $ dom'text "reset"

app'body :: forall dr x. XDomE String dr (COUNT x) Unit
app'body = do
  count <- domState'get'' @"count"
  dom'Div $ pure $ dom'text "Hellllooooooo"
  dom'Div do
    el'Key "asdfasdfasdfasdf..."
    pure do
      when (count < 0) do domError'fail "negative number invalid"
      dom'Span $ pure $ dom'text $
        w'str \t -> t "div with stuff" *> t (stext unit) *> t "!"
      dom'Br $ pass
      dom'Button do
        el'AddCn "btn btn-soft" *> when (count > 5) do el'AddCn "btn-accent"
        el'OnClick $ (\_ -> domState'dispatch'' @"count" Dec)
        pure $ dom'text "dec"
      dom'Div $ pure $ dom'text ("Count:" <+> count)
      dom'withKey "asdfasdf" $ dom'Button do
        el'AddCn "btn btn-soft" *> when (count > 5) do el'AddCn "btn-accent"
        el'OnClick \e -> do
          x'out e
          domState'dispatch'' @"count" Inc
        pure $ dom'text "inc"

app'mk :: forall dr x. UrlSt.XProvider dr x -> XDom dr x Unit
app'mk = router'run'' @"router" urlEdits parseUrl mkTitleOr_ do
  r <- router'routeOrE'' @"router"
  x'out $ show r
  dom'Div do
    el'Cn "flex flex-col gap-4"
    pure do
      dom'Iframe_ $ pure unit
      dom'Div $ pure $ dom'A do
        router'setHref $ Profile "Jimmy"
        el'Cn "btn link btn-primary"
        pure $ dom'text "profile link"
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
