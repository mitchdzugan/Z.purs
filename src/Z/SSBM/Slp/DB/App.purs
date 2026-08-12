module Z.SSBM.Slp.DB.App where

import Z.XDom.Prelude

data CountAction = Inc | Dec | Reset

countAct :: Int -> CountAction -> Int
countAct n Inc = n + 1
countAct n Dec = n - 1
countAct _ Reset = 3

xOnE
  :: forall sx de
   . String
  -> XDom (count :: Reducer CountAction Int | sx) de Unit
xOnE message = do
  dom.div do
    dom.text message
    dom.button do
      el.cn "btn btn-soft"
      el.onClick $ \_ -> domState'dispatch @"count" Reset
      dom.text "reset"

xApp :: forall sx de. XurlStProviderX sx de -> XDom sx de Unit
xApp = router''run printRoute parseUrl (Just <<< urlToString <<< _.url) do
  r <- router''routeOrE
  xOut $ show r
  dom.div do
    el.cn "flex flex-col gap-4"
    dom.iframe $ pure unit
    dom.div $ dom.a do
      el.cn "btn link btn-primary"
      router''href $ Profile "Jimmy"
      dom.text "profile link"
    dom.div $ domState'runReducer @"count" 3 countAct $ dom''bindE xOnE do
      count <- domState'get @"count"
      dom.div $ dom.text "Hellllooooooo"
      dom.div do
        el.key "asdfasdfasdfasdf..."
        when (count < 0) do g @XFail "negative number invalid"
        -- dom.span %%-& \t -> t "div with stuff" *> t unit *> t "!"
        dom.button do
          el.cnW \w -> do
            w "btn btn-soft" *> when (count > 5) do w "btn-accent"
          el.onClick $ (\_ -> domState'dispatch @"count" Dec)
          dom.text "dec"
        dom.div $ dom.text ("Count:" <-> count)
        dom.withKey "asdfasdf" $ dom.button do
          el.cnW \w -> do
            w "btn btn-soft" *> when (count > 5) do w "btn-accent"
          el.onClick $ const $ domState'dispatch @"count" Inc
          dom.text "inc"
  where
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