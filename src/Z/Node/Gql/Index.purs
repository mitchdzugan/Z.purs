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

foreign import js_requestGql
  :: String -> Z.Json -> String -> Z.Json -> Z.Effect (Z.Promise Z.Json)

requestGql
  :: forall x
   . String
  -> Z.Json
  -> String
  -> Z.Json
  -> x Z.# Z.EA Gql.Error Z.@> Z.Json
requestGql apiUrl authToken query vars = do
  Z.xMapE Gql.NetworkError
    $ Z.effectPromiseX
    $ js_requestGql apiUrl authToken query vars

type Client = Gql.OpenOpts (url :: String, authToken :: Z.Maybe String)

_authToken
  :: forall r. Z.Lens' { authToken :: Z.Maybe String | r } (Z.Maybe String)
_authToken = Z.prop (Z.Proxy :: Z.Proxy "authToken")

_url :: forall r. Z.Lens' { url :: String | r } String
_url = Z.prop (Z.Proxy :: Z.Proxy "url")

mkClient :: String -> Z.ModX Client -> Client
mkClient url clientMod = Z.xMod baseClient clientMod
  where
  baseClient = Z.merge { url: url, authToken: Z.Nothing } Gql.baseOpts

fullOpts
  :: Client
  -> Z.ModX Gql.Opts
  -> Gql.Opts
fullOpts c optsMod = Z.xMod baseOpts optsMod
  where
  baseOpts = { cachePath: c.cachePath, networkControl: c.networkControl }

operateUnknown
  :: forall x
   . Client
  -> String
  -> Z.Json
  -> Z.ModX Gql.Opts
  -> Z.X x (Z.EA Gql.Error) Z.Json
operateUnknown client opString vars optsMod = Z.xWithReturn operateUnknownImpl
  where
  operateUnknownImpl xReturn = do
    Z.logInfo { opKey }
    cached <- getCached
    xReturn <$> Z.snd cached # Z.unwrap
    when isCacheOnly $ Z.xLiftE $ Z.xFail Gql.CacheOnlyEmpty
    Z.logInfo "Making GQL Call"
    Z.xLiftE $ requestGql client.url authToken opString vars
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
    Z.logInfo { filename }
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
  -> Z.ModX Gql.Opts
  -> x Z.# Z.EA Gql.Error Z.@> res
operate c (Operation opString enc dec) vars optsMod = do
  j <- operateUnknown c opString (enc vars) optsMod
  Z.xMapE Gql.ResponseTypeError $ Z.result $ dec j
