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

import Unsafe.Coerce (unsafeCoerce)
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

derive instance Eq NetworkControl

instance Generable NetworkControl gdesc NetworkControl where
  mkGenerable = CacheFirst

foreign import js_requestGql
  :: ( String
       -> Json
       -> Json
       -> Json
       -> Json
       -> Int
       -> Object String
       -> Either GqlE.GqlResponseError Json
     )
  -> (Json -> Either GqlE.GqlResponseError Json)
  -> String
  -> Json
  -> String
  -> Json
  -> Effect (Promise $ Either GqlE.GqlResponseError Json)

requestGql
  :: forall x
   . String
  -> Json
  -> String
  -> Json
  -> EA Gql.Error x #> Either GqlE.GqlResponseError Json
requestGql apiUrl authToken query vars = doRequest
  where
  mkResponseError query' variables data' errors extensions status headers =
    Left
      { request: { query: query', variables }
      , response: { data: data', errors, extensions, status, headers }
      }
  doRequest = e'map GqlE.NetworkError $ g @XRunEffPromise $
    js_requestGql mkResponseError Right apiUrl authToken query vars

newtype CacheVal = CacheVal
  { res :: Either GqlE.GqlResponseError Json
  , queryId :: String
  }

cacheVal :: Either GqlE.GqlResponseError Json -> String -> CacheVal
cacheVal res queryId = CacheVal { res, queryId }

cacheValRes :: Json -> String -> CacheVal
cacheValRes d = cacheVal $ Right d

cacheValErr :: GqlE.GqlResponseError -> String -> CacheVal
cacheValErr err = cacheVal $ Left err

derive instance Newtype CacheVal _
derive instance Generic CacheVal _
instance EncodeJson CacheVal where
  encodeJson = genericEncodeJson

instance DecodeJson CacheVal where
  decodeJson = genericDecodeJson

xOperateUnknown
  :: forall x
   . String
  -> Json
  -> Client
  -> NetworkControl
  -> WEA (Array GqlW.T) GqlE.T x #> Json
xOperateUnknown opString vars client nc = x'withReturn \xReturn -> do
  (collisionCount /\ cached) <- getCached cachePath nc
  whenJust cached xReturn
  when (nc == CacheOnly) $ g @XFail GqlE.CacheOnlyEmpty
  xInfo { gql: "submitting operation", op: opHeader, vars }
  xTimeout 6000
  res <- requestGql url authTokenJson opString vars
  writeToCache cachePath collisionCount $ cacheVal res opKeyStr
  e'ok $ mapL GqlE.ResponseError res
  where
  { cachePath, authToken, url } = client
  authTokenJson = encodeJson authToken
  sortedPairs = arr'sort $ jsonSortedPairs vars
  strVals = map jsonStr $ map snd sortedPairs
  opHeader = slice 0 1 $ str'split (Pattern "\n") opString
  opKeyStr = str'joinWith "|" [ opString, str'joinWith "|" strVals ]
  opKey = show $ simpleHash opKeyStr
  filenameParts 0 = [ opKey, "json" ]
  filenameParts collisionCount = [ opKey, show collisionCount, "json" ]
  cacheFilename cachePath =
    (/./) cachePath <<< str'joinWith "." <<< filenameParts
  getCachedRec cachePath collisionCount = do
    let filename = cacheFilename cachePath collisionCount
    parsed <- g @XTellMappedMHush mapMDecodeErr $ xDecodeTextFile filename
    handleParsed parsed
    where
    mapMDecodeErr e@(DecodeError _) = [ GqlW.CacheDecode e ]
    mapMDecodeErr _ = []
    handleParsed Nothing = pure $ collisionCount /\ Nothing
    handleParsed (Just (CacheVal { res, queryId })) = do
      let isSelf = eq opKeyStr queryId
      if not isSelf then getCachedRec cachePath $ collisionCount + 1
      else e'ok (mapL GqlE.ResponseError res) <#> Just <#> (/\) collisionCount
  getCached _ ForceFetch = pure $ 0 /\ Nothing
  getCached Nothing _ = pure $ 0 /\ Nothing
  getCached (Just p) _ = getCachedRec p 0
  writeToCache Nothing _ _ = pass
  writeToCache (Just cachePath) collisionCount toCache = do
    let filename = cacheFilename cachePath collisionCount
    g @XTellMappedHush GqlW.CacheWrite $ xEncodeTextFileP filename toCache

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
  e'map GqlE.ResponseTypeError $ e'ok $ decode json
