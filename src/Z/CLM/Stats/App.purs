module Z.CLM.Stats.App where

import Z.XDom.Prelude

import Z.CLM.Stats.Types (ClmBuildMeta)
import Z.XDom.UrlState as UrlSt

defaultSeasonId :: Int
defaultSeasonId = 15

app'mk :: forall dr x. ClmBuildMeta -> UrlSt.XProvider dr x -> XDom dr x Unit
app'mk buildMeta = router'run'' @"router" printRoute parseUrl mkTitleOr_ do
  r <- router'routeOrE'' @"router"
  x'out $ show r
  dom'div do
    el'cn "flex flex-col gap-4"
    dom'div $
      dom'text "profile link"
  where
  mkTitleOr_ = Just <<< urlToString <<< _.url
  processClmIds (Just hashStr) = do
    clmIds <- forM (str'split (Pattern "~") hashStr) \idStr -> do
      case (tryParseInt idStr) of
        Just clmId -> pure clmId
        _ -> e'fail $ "clmIds must be integers separated by `~`: " <> hashStr
    s'set'b @"!.clmIds" clmIds
  processClmIds _ = pure unit
  processRouteList Nil = pure unit
  processRouteList _ = e'fail "Invalid Route"
  parseUrl u = sync'x $ e'try $ s'exec (mkRoute defaultSeasonId Stats []) do
    processClmIds $ urlHash u
    processRouteList $ list'fromFoldable $ urlPathSegments u
  printRoute (Route r) = "/" <> printRoutePage r.page <> printRouteClmIds
    r.clmIds
  printRoutePage Stats = ""
  printRoutePage Player = "-/"
  printRoutePage Compare = "compare/"
  printRoutePage H2hGrid = "h2h/"
  printRouteClmIds [] = ""
  printRouteClmIds clmIds = (<>) "#" $ str'joinWith "~" $ clmIds <#> show

data RoutePage = Stats | Player | Compare | H2hGrid

newtype Route = Route
  { page :: RoutePage, seasonId :: Int, clmIds :: Array Int }

derive instance Newtype Route _

mkRoute :: Int -> RoutePage -> Array Int -> Route
mkRoute seasonId page clmIds = Route { page, seasonId, clmIds }

derive instance Generic Route _
instance Show Route where
  show = genericShow

derive instance Generic RoutePage _
instance Show RoutePage where
  show = genericShow
