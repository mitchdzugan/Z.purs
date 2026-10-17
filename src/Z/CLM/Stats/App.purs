module Z.CLM.Stats.App where

import Z.XDom.Prelude

import Z.CLM.Stats.Types (ClmBuildOutputs, ClmSeason'Json)
import Z.XDom.UrlState as UrlSt
import Z.Z.HashMap (hm'insert)

data FetchCache'ValState v
  = FetchCache'ValState'Loading (Maybe v)
  | FetchCache'ValState'Error JsError (Maybe v)
  | FetchCache'ValState'Val v

derive instance Eq v => Eq (FetchCache'ValState v)

type FetchCache'State id v = HashMap id (FetchCache'ValState v)

data FetchCache'Action id v
  = FetchCache'Action'MarkRequesting id
  | FetchCache'Action'SetVal id v
  | FetchCache'Action'SetError id JsError
  | FetchCache'Action'ImportVals (HashMap id v)

fetchCache'lookupVal
  :: forall id v. Identable id => id -> FetchCache'State id v -> Maybe v
fetchCache'lookupVal id hm = case hm'lookup id hm of
  Just (FetchCache'ValState'Loading res) -> res
  Just (FetchCache'ValState'Error _ res) -> res
  Just (FetchCache'ValState'Val v) -> Just v
  _ -> Nothing

fetchCache'valState'shouldFetch :: forall v. FetchCache'ValState v -> Boolean
fetchCache'valState'shouldFetch (FetchCache'ValState'Error _ _) = true
fetchCache'valState'shouldFetch _ = false

fetchCache'shouldFetch
  :: forall id v. Identable id => id -> FetchCache'State id v -> Boolean
fetchCache'shouldFetch id hm =
  may'or true $ hm'lookup id hm <#> fetchCache'valState'shouldFetch

fetchCache'xUpdate
  :: forall id v
   . Identable id
  => Eq v
  => FetchCache'Action id v
  -> Edit (FetchCache'State id v)
fetchCache'xUpdate (FetchCache'Action'MarkRequesting id) = do
  currVal <- s'get <#> fetchCache'lookupVal id
  s'update $ hm'insert id $ FetchCache'ValState'Loading currVal
fetchCache'xUpdate (FetchCache'Action'SetVal id v) = do
  s'update $ hm'insert id $ FetchCache'ValState'Val v
fetchCache'xUpdate (FetchCache'Action'SetError id e) = do
  currVal <- s'get <#> fetchCache'lookupVal id
  s'update $ hm'insert id $ FetchCache'ValState'Error e currVal
fetchCache'xUpdate (FetchCache'Action'ImportVals hm'new) = do
  s'update \hm'curr -> hm'merge [ hm'curr, map FetchCache'ValState'Val hm'new ]

fetchCache'update
  :: forall id v
   . Identable id
  => Eq v
  => FetchCache'State id v
  -> FetchCache'Action id v
  -> FetchCache'State id v
fetchCache'update s a = edit s $ fetchCache'xUpdate a

type X'FetchCache id v =
  X'Reducer (FetchCache'Action id v) (FetchCache'State id v)

type T'domFetchCache'run'helper'fn dr x' x id =
  (id -> String) -> MDom dr x' (Array id) -> MDom dr x Unit -> MDom dr x' Unit

type T'domFetchCache'run'helper p id v t'useResult =
  forall dr x' x
   . Identable id
  => Eq v
  => Dom'ConsSymbol p (X'FetchCache id v) x' x
  => t'useResult (T'domFetchCache'run'helper'fn dr x' x id)

domFetchCache'run
  :: forall @p @id @v. T'domFetchCache'run'helper p id v $ (->) $ HashMap id v
domFetchCache'run cachePreload mkFetchUrl getId m = do
  currIds <- getId
  let cacheInitial = cachePreload <#> FetchCache'ValState'Val
  domS'runReducer'' @p cacheInitial fetchCache'update do
    domEffA'on (map ident'key currIds) do
      forM_ currIds \currId -> do
        shouldFetch <- domS'get'' @p <#> fetchCache'shouldFetch currId
        when shouldFetch do
          domS'dispatch'' @p $ FetchCache'Action'MarkRequesting currId
          fetchRes <- e'try $ x'fetch (mkFetchUrl currId) {}
          pure unit
      pure $ pure unit
    m

-- domFetchCache'run_ :: forall @p @id @v. T'domFetchCache'run'helper p id v T'id
-- domFetchCache'run_ = domFetchCache'run @p hm'empty

domFetchCache'lookup
  :: forall @p id v x' x
   . ConsSymbol p (X'FetchCache id v) x' x
  => Identable id
  => id
  -> Run x (Maybe v)
domFetchCache'lookup id = domS'get'' @p <#> fetchCache'lookupVal id

type ClmDom'X x = Router' String Route
  ( buildOutputs :: X'R ClmBuildOutputs
  , clmSeasons :: X'FetchCache Int ClmSeason'Json
  | x
  )

type ClmDom dr x a = XDom dr (ClmDom'X x) a

app'body :: forall dr x. ClmDom dr x Unit
app'body = do
  r <- router'routeOrE
  x'out $ show r
  dom'div do
    el'cn "flex flex-col gap-4"
    dom'div $
      dom'text "profile link"

app'mk :: forall dr x. ClmBuildOutputs -> UrlSt.XProvider dr x -> XDom dr x Unit
app'mk buildOutputs = router'run printRoute parseUrl mkTitleOr_
  $ domR'run'' @"buildOutputs" buildOutputs
  $ domFetchCache'run @"clmSeasons" hm'empty mkClmSeasonFetchUrl getClmSeasonIds
  $ app'body
  where
  mkClmSeasonFetchUrl id = "/xd/" <:> id <:> ".json"
  getClmSeasonIds =
    router'routeOrE <#> hush <#> may'toArray <#> map (g_ @"!.seasonId")
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
  defaultSeasonId = buildOutputs.timeline.defaultSeasonId
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

derive instance Generic RoutePage _

instance Show RoutePage where
  show = genericShow

newtype Route = Route
  { page :: RoutePage, seasonId :: Int, clmIds :: Array Int }

derive instance Newtype Route _

derive instance Generic Route _

instance Show Route where
  show = genericShow

mkRoute :: Int -> RoutePage -> Array Int -> Route
mkRoute seasonId page clmIds = Route { page, seasonId, clmIds }