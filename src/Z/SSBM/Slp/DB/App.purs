module Z.SSBM.Slp.DB.App where

import Z.XDom.Prelude

data CountAction = Inc | Dec | Reset

countAct :: Int -> CountAction -> Int
countAct n Inc = n + 1
countAct n Dec = n - 1
countAct _ Reset = 3

xOnE :: forall x. String -> XDom (count :: Reducer CountAction Int | x)
xOnE message = do
  d.div do
    d.text message
    d.button do
      da.cn "btn btn-soft"
      da.onClick $ \_ -> g1 @XDomDispatch @"count" Reset
      d.text "reset"

xApp :: forall x. XurlStProviderX x -> XDom x
xApp = g @XDomRunRouter printRoute parseUrl (Just <<< urlToString <<< _.url) do
  r <- g @XDomRouteOrE
  xOut $ show r
  d.div do
    da.cn "flex flex-col gap-4"
    d.iframe $ pure unit
    d.div $ d.a do
      da.cn "btn link btn-primary"
      g @XDomRouteHref $ Profile "Jimmy"
      d.text "profile link"
    d.div $ g1 @XDomRunReducer @"count" 3 countAct $ g @XDomBindE xOnE do
      count <- g1 @XDomGetState @"count"
      d.div %% "Hellllooooooo"
      d.div do
        da.key "asdfasdfasdfasdf..."
        when (count < 0) do g @XFail "negative number invalid"
        d.span %%-& \t -> t "div with stuff" *> t unit *> t "!"
        d.button do
          da.cnW \w -> do
            w "btn btn-soft" *> when (count > 5) do w "btn-accent"
          da.onClick $ (\_ -> g1 @XDomDispatch @"count" Dec)
          d.text "dec"
        d.div %% "Count:" <-> count
        "asdfasdf" <!& d.button do
          da.cnW \w -> do
            w "btn btn-soft" *> when (count > 5) do w "btn-accent"
          da.onClick $ const $ g1 @XDomDispatch @"count" Inc
          d.text "inc"
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