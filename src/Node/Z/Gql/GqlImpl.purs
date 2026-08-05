module Node.Z.Gql.GqlImpl
  ( Client
  , NetworkControl(..)
  , Operation
  , defOperation
  , mkClient
  , xOperate
  , xOperateUnknown
  ) where

import Node.Z.Prelude
import Z.Gql.Error as GqlE
import Z.Gql.Module as Gql
import Z.Gql.Warning as GqlW

type Client =
  { url :: String
  , authToken :: Maybe String
  , cachePath :: Maybe String
  }

mkClient :: String -> Edit Client -> Client
mkClient url clientMod = edit baseClient clientMod
  where
  baseClient = { url: url, authToken: Nothing, cachePath: Nothing }

data NetworkControl
  = CacheOnly
  | CacheFirst
  | ForceFetch

derive instance eqNetworkControl :: Eq NetworkControl

instance defaultNC :: Defaultable NetworkControl where
  default = CacheFirst

foreign import js_requestGql
  :: String -> Json -> String -> Json -> Effect (Promise Json)

requestGql
  :: forall x
   . String
  -> Json
  -> String
  -> Json
  -> EA Gql.Error x #> Json
requestGql apiUrl authToken query vars = x' @"mapE" GqlE.NetworkError
  $ x' @"runEffPromise"
  $ js_requestGql apiUrl authToken query vars

xOperateUnknown
  :: forall x
   . String
  -> Json
  -> Client
  -> NetworkControl
  -> WEA (Array GqlW.T) GqlE.T x #> Json
xOperateUnknown opString vars client networkControl = x' @"withReturn"
  \xReturn ->
    do
      (collisionCount /\ cached) <- getCached cachePath networkControl
      whenJust cached xReturn
      when (networkControl == CacheOnly) $ x' @"fail" GqlE.CacheOnlyEmpty
      xInfo { gql: "submitting operation", op: opHeader, vars }
      xTimeout 6000
      res <- requestGql url authTokenJson opString vars
      let toCache = [ res, fromString opKeyStr ]
      writeToCache cachePath collisionCount toCache
      pure res
  where
  { cachePath, authToken, url } = client
  authTokenJson = encodeJson authToken
  sortedPairs = arrSort $ jsonSortedPairs vars
  -- reverse in specific case to match my old startgg cache
  strVals = map jsonStr $ map snd $ case map fst sortedPairs of
    [ "page", "phaseGroupId" ] -> arrReverse sortedPairs
    _ -> sortedPairs
  opHeader = slice 0 1 $ strSplit (Pattern "\n") opString
  opKeyStr = strJoinWith "|" [ opString, strJoinWith "|" strVals ]
  opKey = show $ simpleHash opKeyStr
  filenameParts 0 = [ opKey, "json" ]
  filenameParts collisionCount = [ opKey, show collisionCount, "json" ]
  cacheFilename cachePath =
    (/./) cachePath <<< strJoinWith "." <<< filenameParts
  getCachedRec cachePath collisionCount = do
    let filename = cacheFilename cachePath collisionCount
    parsed <- x' @"tellMappedMHush" mapMDecodeErr $ xDecodeTextFile filename
    handleParsed parsed
    where
    mapMDecodeErr e@(DecodeError _) = [ GqlW.CacheDecode e ]
    mapMDecodeErr _ = []
    checkIsSelf parseData = jOrF do
      cachedOpKeyStr <- (nth parseData 1)
      pure $ caseJsonString false (eq opKeyStr) cachedOpKeyStr
    handleParsed Nothing = pure $ collisionCount /\ Nothing
    handleParsed (Just parseData) = do
      let isSelf = checkIsSelf parseData
      if isSelf then pure $ collisionCount /\ nth parseData 0
      else getCachedRec cachePath $ collisionCount + 1
  getCached _ ForceFetch = pure $ 0 /\ Nothing
  getCached Nothing _ = pure $ 0 /\ Nothing
  getCached (Just p) _ = getCachedRec p 0
  writeToCache Nothing _ _ = default
  writeToCache (Just cachePath) collisionCount toCache = do
    let filename = cacheFilename cachePath collisionCount
    x' @"tellMappedHush" GqlW.CacheWrite $ xEncodeTextFileP filename toCache

data Operation v r = Operation String (JsonEncodeFn v) (JsonDecodeFn r)

defOperation
  :: forall vars res
   . EncodeJson vars
  => DecodeJson res
  => String
  -> Proxy vars
  -> Proxy res
  -> Operation vars res
defOperation opString _ _ = Operation opString encodeJson decodeJson

xOperate
  :: forall vars res x
   . Operation vars res
  -> vars
  -> Client
  -> NetworkControl
  -> WEA (Array GqlW.T) GqlE.T x #> res
xOperate (Operation opString encode decode) vars client networkControl = do
  json <- xOperateUnknown opString (encode vars) client networkControl
  x' @"mapE" GqlE.ResponseTypeError $ x' @"ok" $ decode json
