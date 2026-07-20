module Z.Node.Gql.Index
  ( Client
  , Operation
  , _authToken
  , _url
  , defOperation
  , fullOpts
  , mkClient
  , operate
  , operateUnknown
  ) where

import Prelude

import Z.Gql as Gql
import Z.Node.Sys as Sys
import Z.Z as Z

type Client = Gql.OpenOpts (url :: String, authToken :: Z.Maybe String)

_authToken
  :: forall r. Z.Lens' { authToken :: Z.Maybe String | r } (Z.Maybe String)
_authToken = Z.prop (Z.Proxy :: Z.Proxy "authToken")

_url :: forall r. Z.Lens' { url :: String | r } String
_url = Z.prop (Z.Proxy :: Z.Proxy "url")

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
    $ Z.effectPromiseX
    $ js_requestGql apiUrl authToken query vars

operateUnknown
  :: forall x
   . Client
  -> String
  -> Z.Json
  -> Z.Edit Gql.Opts
  -> Z.X (Z.EA Gql.Error x) Z.Json
operateUnknown client opString vars optsMod = Z.xWithRet operateUnknownImpl
  where
  operateUnknownImpl = do
    Z.xInfo { opKey, isCacheOnly, nc: opts.networkControl }
    (Z.Tuple collisionCount cached) <- getCached
    Z.xInfo { collisionCount }
    Z.xReturn <$> cached # Z.unwrap
    when isCacheOnly $ Z.xRetLift $ Z.xFail Gql.CacheOnlyEmpty
    Z.xInfo "Making GQL Call"
    Z.xTimeout 6000
    res <- Z.xRetLift $ requestGql client.url authToken opString vars
    let toCache = [ res, Z.fromString opKeyStr ]
    Z.xInfo toCache
    Z.xRetLift $ writeToCache opts.cachePath collisionCount toCache
    pure res
  opts = fullOpts client optsMod
  isCacheOnly = opts.networkControl == Gql.CacheOnly
  authToken = Z.encodeJson client.authToken
  sortedPairs = Z.arrSort $ Z.jsonSortedPairs vars
  -- reverse in specific case to match my old startgg cache
  strVals = map Z.jsonStr $ map Z.snd $ case map Z.fst sortedPairs of
    [ "page", "phaseGroupId" ] -> Z.arrReverse sortedPairs
    _ -> sortedPairs
  opKeyStr = Z.joinWith "|" [ opString, Z.joinWith "|" strVals ]
  opKey = show $ Z.simpleHash opKeyStr
  filenameParts 0 = [ opKey, "json" ]
  filenameParts collisionCount = [ opKey, show collisionCount, "json" ]
  cacheFilename cachePath =
    Sys.join cachePath <<< Z.joinWith "." <<< filenameParts
  getCachedRec cachePath collisionCount = do
    let filename = cacheFilename cachePath collisionCount
    Z.xInfo { filename }
    parsed :: Z.Maybe (Array Z.Json) <- Z.xHush do
      Sys.decodeTextFile filename
    handleParsed parsed
    where
    checkIsSelf parseData = Z.fromMaybe false do
      cachedOpKeyStr <- (Z.nth parseData 1)
      pure $ Z.caseJsonString false (eq opKeyStr) cachedOpKeyStr
    handleParsed Z.Nothing = pure $ Z.Tuple collisionCount Z.Nothing
    handleParsed (Z.Just parseData) = do
      let isSelf = checkIsSelf parseData
      if isSelf then pure $ Z.Tuple collisionCount $ Z.nth parseData 0
      else getCachedRec cachePath $ collisionCount + 1
  getCachedArgs _ Gql.ForceFetch = pure $ Z.Tuple 0 Z.Nothing
  getCachedArgs Z.Nothing _ = pure $ Z.Tuple 0 Z.Nothing
  getCachedArgs (Z.Just p) _ = getCachedRec p 0
  getCached = getCachedArgs opts.cachePath opts.networkControl
  writeToCache Z.Nothing _ _ = Z.pass
  writeToCache (Z.Just cachePath) collisionCount toCache = do
    let filename = cacheFilename cachePath collisionCount
    Z.xMapE Gql.CacheWriter $ Sys.encodeTextFile filename toCache

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
   . Client
  -> Operation vars res
  -> vars
  -> Z.Edit Gql.Opts
  -> Z.X (Z.EA Gql.Error x) res
operate c (Operation opString enc dec) vars optsMod = do
  j <- operateUnknown c opString (enc vars) optsMod
  Z.xMapE Gql.ResponseTypeError $ Z.xOk $ dec j
