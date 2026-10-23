module Z.XDom.FetchCache
  ( FetchCache'Action(..)
  , FetchCache'R
  , FetchCache'State
  , FetchCache'ValState(..)
  , X'FetchCache
  , domFetchCache'ensureRequested
  , domFetchCache'forceRequest
  , domFetchCache'lookup
  , domFetchCache'run
  , domFetchCache'run_
  ) where

import Z.Prelude

import Z.XDom.Core
  ( class Dom'ConsSymbol
  , MDom
  , MDomEff
  , XDom'StateReducer
  , domEff'async
  , domEff'onNew'lead
  , domState'dispatch''
  , domState'get''
  , domState'runReducer''
  )

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
  => FetchCache'R id v
  -> FetchCache'Action id v
  -> FetchCache'R id v
fetchCache'update s a =
  let
    nextState = edit s.state $ fetchCache'xUpdate a
  in
    if (nextState =@= s.state) then s else rec'set @"state" nextState s

type FetchCache'R id v =
  { mkFetchUrl :: id -> String
  , state :: FetchCache'State id v
  }

type X'FetchCache id v =
  XDom'StateReducer (FetchCache'Action id v) (FetchCache'R id v)

domFetchCache'ensureRequested
  :: forall @p id v x' x
   . Dom'ConsSymbol p (X'FetchCache id v) x' x
  => Identable id
  => Eq v
  => DecodeJson v
  => id
  -> MDomEff x Unit
domFetchCache'ensureRequested id = do
  { state } <- domState'get'' @p
  when (fetchCache'shouldFetch id state) $ domFetchCache'forceRequest @p id

domFetchCache'forceRequest
  :: forall @p id v x' x
   . Dom'ConsSymbol p (X'FetchCache id v) x' x
  => Identable id
  => Eq v
  => DecodeJson v
  => id
  -> MDomEff x Unit
domFetchCache'forceRequest id = do
  { mkFetchUrl } <- domState'get'' @p
  domEff'async do
    domState'dispatch'' @p $ FetchCache'Action'MarkRequesting id
    vOrE <- e'try $ do
      fetchRes <- x'fetch (mkFetchUrl id) pass
      decoded <- x'decodeFetched @v fetchRes
      e'ok $ constL (jsError "decode error" "") decoded
    case vOrE of
      Left e -> do
        domState'dispatch'' @p $ FetchCache'Action'SetError id e
      Right v -> do
        domState'dispatch'' @p $ FetchCache'Action'SetVal id v

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
  let init = { mkFetchUrl, state: cachePreload <#> FetchCache'ValState'Val }
  domState'runReducer'' @p init fetchCache'update do
    domEff'onNew'lead (map ident'key currIds) (onNew currIds) *> m
  where
  onNew currIds = do
    forM_ currIds \currId -> do
      domFetchCache'ensureRequested @p currId

domFetchCache'run_ :: forall @p @id @v. T'domFetchCache'run'helper p id v T'id
domFetchCache'run_ = domFetchCache'run @p hm'empty

domFetchCache'lookup
  :: forall @p id v x' x
   . ConsSymbol p (X'FetchCache id v) x' x
  => Identable id
  => id
  -> Run x (FetchCache'ValState v)
domFetchCache'lookup id =
  domState'get'' @p <#> _.state <#> hm'lookup id <#> case _ of
    Nothing -> FetchCache'ValState'Loading Nothing
    Just res -> res