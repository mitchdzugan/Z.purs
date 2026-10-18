module Z.CLM.Stats.App where

import Z.XDom.Prelude

import Data.Functor.Variant (VariantF)
import Z.CLM.Stats.Types
  ( ClmBuildOutputs
  , ClmPlayer
  , ClmSeason'Event
  , ClmSeason'Json
  )
import Z.XDom.UrlState as UrlSt
import Z.Z.X.Responds (Responds'Const, responds'run)

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
  => DecodeJson v
  => Dom'ConsSymbol p (X'FetchCache id v) x' x
  => t'useResult (T'domFetchCache'run'helper'fn dr x' x id)

domFetchCache'run
  :: forall @p @id @v. T'domFetchCache'run'helper p id v $ (->) $ HashMap id v
domFetchCache'run cachePreload mkFetchUrl getId m = do
  currIds <- getId
  let cacheInitial = cachePreload <#> FetchCache'ValState'Val
  domS'runReducer'' @p cacheInitial fetchCache'update do
    domEff'onNew'lead (map ident'key currIds) (onNew currIds) *> m
  where
  onNew currIds = domEff'async do
    forM_ currIds \currId -> do
      shouldFetch <- domS'get'' @p <#> fetchCache'shouldFetch currId
      when shouldFetch do
        domS'dispatch'' @p $ FetchCache'Action'MarkRequesting currId
        vOrE <- e'try $ do
          fetchRes <- x'fetch (mkFetchUrl currId) pass
          decoded <- x'decodeFetched @v fetchRes
          e'ok $ constL (jsError "decode error" "") decoded
        case vOrE of
          Left e -> do
            domS'dispatch'' @p $ FetchCache'Action'SetError currId e
          Right v -> do
            domS'dispatch'' @p $ FetchCache'Action'SetVal currId v

domFetchCache'run_ :: forall @p @id @v. T'domFetchCache'run'helper p id v T'id
domFetchCache'run_ = domFetchCache'run @p hm'empty

domFetchCache'lookup
  :: forall @p id v x' x
   . ConsSymbol p (X'FetchCache id v) x' x
  => Identable id
  => id
  -> Run x (Maybe v)
domFetchCache'lookup id = domS'get'' @p <#> fetchCache'lookupVal id

-----------------------------------------------------------------------

foreign import data ClmSeason'Tag :: Type

type ClmSeason'Responds = VariantF
  ( events :: Responds'Const (HashMap SorN ClmSeason'Event)
  , players :: Responds'Const (HashMap Int ClmPlayer)
  )

instance Boxable ClmSeason'Tag ClmSeason'Json ClmSeason'Responds where
  boxable'mkResponds b = varf'match
    { events: responds'run \_ -> keyBy _.eventId $ (boxed'v b).events
    , players: responds'run \_ -> keyBy _.clmId $ (boxed'v b).players
    }

type ClmSeason'Boxed = Boxed ClmSeason'Tag

-----------------------------------------------------------------------

type ClmBuildOutputs'X x = (buildOutputs :: X'R ClmBuildOutputs | x)

type ClmDom'X x = Router' String Route $ ClmBuildOutputs'X
  ( clmSeasons :: X'FetchCache Int ClmSeason'Boxed
  | x
  )

type ClmDom dr x a = XDom dr (ClmDom'X x) a

router'seasonId
  :: forall dx x. MDom dx (Router' String Route $ ClmBuildOutputs'X x) Int
router'seasonId = do
  defaultSeasonId <- x'extract @"buildOutputs" <#> _.timeline.defaultSeasonId
  router'routeOrE <#> gmOr_ @">!.seasonId" defaultSeasonId

app'body :: forall dr x. ClmDom dr x Unit
app'body = do
  seasonId <- router'seasonId
  seasonOr_ <- domFetchCache'lookup @"clmSeasons" seasonId
  whenJust seasonOr_ \season -> do
    let seasonEvents = boxed'call_ @"events" season
    x'out { season }
    x'out { seasonEvents }
  dom'div do
    el'cn "flex flex-col gap-4"
    dom'div $
      dom'text "profile link"

tester :: forall dx x. XDom dx x Unit
tester = do
  dom'div $ dom'text "Hello"

app'mk :: forall dr x. ClmBuildOutputs -> UrlSt.XProvider dr x -> XDom dr x Unit
app'mk buildOutputs = router'run printRoute parseUrl mkTitleOr_
  $ domR'run'' @"buildOutputs" buildOutputs
  $ domFetchCache'run_ @"clmSeasons" mkClmSeasonFetchUrl getClmSeasonIds
  $ app'body
  where
  mkClmSeasonFetchUrl id = "/db/" <:> dbHash <:> "/seasons/" <:> id <:> ".json"
  getClmSeasonIds = router'seasonId <#> pure
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
  dbHash = buildOutputs.dbHash
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