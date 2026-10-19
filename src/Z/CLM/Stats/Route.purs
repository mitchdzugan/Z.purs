module Z.CLM.Stats.Route
  ( Route(..)
  , RoutePage(..)
  , route'urlEdits
  , route'urlParse
  ) where

import Z.Prelude

import Z.CLM.Stats.Season (seasonId'slug)

data RoutePage = Stats | Player | Compare | H2hGrid

derive instance Generic RoutePage _

instance Show RoutePage where
  show = genericShow

newtype Route = Route
  { page :: RoutePage, seasonId :: Int, clmIds :: Array Int }

derive instance Newtype Route _

derive instance Generic Route _

instance Show Route where
  show = genericShow

route'mk :: Int -> RoutePage -> Array Int -> Route
route'mk seasonId page clmIds = Route { page, seasonId, clmIds }

route'urlEdits :: Int -> Route -> Edit UrlParts
route'urlEdits defaultSeasonId (Route { page, seasonId, clmIds }) = do
  s'set'b @"hash" (getHash clmIds)
  s'set'b @"path" path
  where
  path = sync'x $ _'exec_ @(X'Vector String) do
    when (seasonId /= defaultSeasonId) do
      _'cons $ seasonId'slug seasonId
    case page of
      Player -> _'cons "-"
      Compare -> _'cons "compare"
      H2hGrid -> _'cons "h2h"
      _ -> pure unit
  getHash [] = Nothing
  getHash ids = Just $ str'joinWith "~" $ ids <#> show

route'urlParse :: Int -> URL -> Either String Route
route'urlParse defaultSeasonId u =
  sync'x $ e'try $ s'exec (route'mk defaultSeasonId Stats []) do
    processClmIds $ url'hash u
    processRouteList $ list'fromFoldable $ url'pathSegments u
  where
  processClmIds (Just hashStr) = do
    clmIds <- forM (str'split (Pattern "~") hashStr) \idStr -> do
      case (tryParseInt idStr) of
        Just clmId -> pure clmId
        _ -> e'fail $ "clmIds must be integers separated by `~`: " <> hashStr
    s'set'b @"!.clmIds" clmIds
  processClmIds _ = pure unit
  processRouteList Nil = pure unit
  processRouteList _ = e'fail "Invalid Route"