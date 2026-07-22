module Z.Gql.Node.Impl
  ( Client
  , Operation
  , defOperation
  , fullOpts
  , mkClient
  , operate
  , operateUnknown
  ) where

import Prelude

import Z.Gql.Module as Gql
import Z.Sys.Node.Module as Sys
import Z as Z

type Client = Gql.OpenOpts (url :: String, authToken :: Z.Maybe String)

mkClient :: String -> Z.Edit Client -> Client
mkClient url clientMod = Z.edit baseClient clientMod
  where
  baseClient = Z.merge { url: url, authToken: Z.Nothing } Gql.baseOpts

fullOpts
  :: Client
  -> Z.Edit Gql.Opts
  -> Gql.Opts
fullOpts c optsMod = Z.edit baseOpts optsMod
  where
  baseOpts = { cachePath: c.cachePath, networkControl: c.networkControl }

foreign import js_requestGql
  :: String -> Z.Json -> String -> Z.Json -> Z.Effect (Z.Promise Z.Json)

requestGql
  :: forall x
   . String
  -> Z.Json
  -> String
  -> Z.Json
  -> Z.X (Z.EA Gql.Error x) Z.Json
requestGql apiUrl authToken query vars = do
  Z.xMapE Gql.NetworkError
    $ Z.xEffectPromise
    $ js_requestGql apiUrl authToken query vars

operateUnknown
  :: forall x
   . String
  -> Z.Json
  -> Client
  -> Z.Edit Gql.Opts
  -> Z.X (Z.WEA (Array Gql.Warning) Gql.Error x) Z.Json
operateUnknown opString vars client optsMod = Z.xWithRet $ do
  (collisionCount Z./\ cached) <- getCached cachePath networkControl
  Z.whenJust cached Z.xReturn
  when (networkControl == Gql.CacheOnly) $ Z.xRetFail Gql.CacheOnlyEmpty
  Z.xInfo { gql: "submitting operation", op: opHeader, vars }
  Z.xTimeout 6000
  res <- Z.xRetLift $ requestGql client.url authToken opString vars
  let toCache = [ res, Z.fromString opKeyStr ]
  Z.xRetLift $ writeToCache cachePath collisionCount toCache
  pure res
  where
  { cachePath, networkControl } = fullOpts client optsMod
  authToken = Z.encodeJson client.authToken
  sortedPairs = Z.arrSort $ Z.jsonSortedPairs vars
  -- reverse in specific case to match my old startgg cache
  strVals = map Z.jsonStr $ map Z.snd $ case map Z.fst sortedPairs of
    [ "page", "phaseGroupId" ] -> Z.arrReverse sortedPairs
    _ -> sortedPairs
  opHeader = Z.slice 0 1 $ Z.split (Z.Pattern "\n") opString
  opKeyStr = Z.joinWith "|" [ opString, Z.joinWith "|" strVals ]
  opKey = show $ Z.simpleHash opKeyStr
  filenameParts 0 = [ opKey, "json" ]
  filenameParts collisionCount = [ opKey, show collisionCount, "json" ]
  cacheFilename cachePath =
    Sys.join cachePath <<< Z.joinWith "." <<< filenameParts
  getCachedRec cachePath collisionCount = do
    let filename = cacheFilename cachePath collisionCount
    parsed <- Z.xTellMappedMHush mapMDecodeErr $ Sys.decodeTextFile filename
    handleParsed parsed
    where
    mapMDecodeErr e@(Sys.DecodeError _) = [ Gql.CacheDecode e ]
    mapMDecodeErr _ = []
    checkIsSelf parseData = Z.fromMaybe false do
      cachedOpKeyStr <- (Z.nth parseData 1)
      pure $ Z.caseJsonString false (eq opKeyStr) cachedOpKeyStr
    handleParsed Z.Nothing = pure $ collisionCount Z./\ Z.Nothing
    handleParsed (Z.Just parseData) = do
      let isSelf = checkIsSelf parseData
      if isSelf then pure $ collisionCount Z./\ Z.nth parseData 0
      else getCachedRec cachePath $ collisionCount + 1
  getCached _ Gql.ForceFetch = pure $ 0 Z./\ Z.Nothing
  getCached Z.Nothing _ = pure $ 0 Z./\ Z.Nothing
  getCached (Z.Just p) _ = getCachedRec p 0
  writeToCache Z.Nothing _ _ = Z.default
  writeToCache (Z.Just cachePath) collisionCount toCache = do
    let filename = cacheFilename cachePath collisionCount
    Z.xTellMappedHush Gql.CacheWrite $ Sys.encodeTextFileP filename toCache

data Operation vars res = Operation String (Z.JsonEncodeFn vars)
  (Z.JsonDecodeFn res)

defOperation
  :: forall vars res
   . Z.EncodeJson vars
  => Z.DecodeJson res
  => String
  -> Z.Proxy vars
  -> Z.Proxy res
  -> Operation vars res
defOperation opString _ _ = Operation opString Z.encodeJson Z.decodeJson

operate
  :: forall vars res x
   . Operation vars res
  -> vars
  -> Client
  -> Z.Edit Gql.Opts
  -> Z.X (Z.WEA (Array Gql.Warning) Gql.Error x) res
operate (Operation opString encode decode) vars c optsMod = do
  j <- operateUnknown opString (encode vars) c optsMod
  Z.xMapE Gql.ResponseTypeError $ Z.xOk $ decode j
