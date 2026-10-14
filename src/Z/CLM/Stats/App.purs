module Z.CLM.Stats.App where

import Z.XDom.Prelude

import Z.XDom.UrlState as UrlSt

defaultSeasonId :: Int
defaultSeasonId = 15

app'mk :: forall dr x. UrlSt.XProvider dr x -> XDom dr x Unit
app'mk = router'run'' @"router" printRoute parseUrl mkTitleOr_ do
  r <- router'routeOrE'' @"router"
  x'out $ show r
  dom'div do
    el'cn "flex flex-col gap-4"
    dom'div $
      dom'text "profile link"
  where
  mkTitleOr_ = Just <<< urlToString <<< _.url
  parseRouteL Nil = pure $ mkRoute defaultSeasonId Stats
  parseRouteL _ = Left "Invalid Route"

  parseUrl :: URL -> Either String Route
  parseUrl u = parseRouteL $ list'fromFoldable $ urlPathSegments u

  printRoute :: Route -> String
  printRoute _ = "/"

data RoutePage = Stats | Player | Compare | Grid

newtype Route = Route (Int /\ RoutePage)

mkRoute :: Int -> RoutePage -> Route
mkRoute seasonId = Route <<< (/\) seasonId

derive instance Generic Route _
instance Show Route where
  show = genericShow

derive instance Generic RoutePage _
instance Show RoutePage where
  show = genericShow
