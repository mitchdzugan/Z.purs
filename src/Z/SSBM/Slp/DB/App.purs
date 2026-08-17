module Z.SSBM.Slp.DB.App where

import Z.XDom.Prelude

import Z.XDom.UrlState as UrlSt

data CountAction = Inc | Dec | Reset

countAct :: Int -> CountAction -> Int
countAct n Inc = n + 1
countAct n Dec = n - 1
countAct _ Reset = 3

type COUNT x = (count :: Reducer CountAction Int | x)

app'onE :: forall dr x. String -> XDom dr (COUNT x) Unit
app'onE message = do
  dom'div do
    dom'text message
    dom'button do
      el'cn "btn btn-soft"
      el'onClick $ \_ -> domS'dispatch'' @"count" Reset
      dom'text "reset"

app'body :: forall dr x. XDomE String dr (COUNT x) Unit
app'body = do
  count <- domS'get'' @"count"
  dom'div $ dom'text "Hellllooooooo"
  dom'div do
    el'key "asdfasdfasdfasdf..."
    when (count < 0) do domE'fail "negative number invalid"
    dom'span %% w'str \t -> t "div with stuff" *> t (stext unit) *> t "!"
    dom'br $ pass
    dom'button do
      el'cnW \w -> do
        w "btn btn-soft" *> when (count > 5) do w "btn-accent"
      el'onClick $ (\_ -> domS'dispatch'' @"count" Dec)
      dom'text "dec"
    dom'div $ dom'text ("Count:" <-> count)
    dom'withKey "asdfasdf" $ dom'button do
      el'cnW \w -> do
        w "btn btn-soft" *> when (count > 5) do w "btn-accent"
      el'onClick \e -> do
        xOut e
        domS'dispatch'' @"count" Inc
      dom'text "inc"

app'mk :: forall dr x. UrlSt.XProvider dr x -> XDom dr x Unit
app'mk = router'run'' @"router" printRoute parseUrl mkTitleOr_ do
  r <- router'routeOrE'' @"router"
  xOut $ show r
  dom'div do
    el'cn "flex flex-col gap-4"
    dom'iframe $ pure unit
    dom'div $ dom'a do
      router'href $ Profile "Jimmy"
      el'cn "btn link btn-primary"
      dom'text "profile link"
    domS'runReducer'' @"count" 3 countAct $ domE'bind app'onE app'body
  where
  mkTitleOr_ = Just <<< urlToString <<< _.url
  parseRouteL Nil = pure Home
  parseRouteL (Cons "profile" (Cons slug Nil)) = pure $ Profile slug
  parseRouteL _ = Left "Invalid Route"

  parseUrl :: URL -> Either String Route
  parseUrl u = parseRouteL $ listFromFoldable $ urlPathSegments u

  printRoute :: Route -> String
  printRoute Home = "/"
  printRoute (Profile s) = "/profile/" <> s

data Route = Home | Profile String

derive instance Generic Route _
instance Show Route where
  show = genericShow
