module Z.XDom.FetchCache
  ( FetchCache'Action'row
  , FetchCache'Action(..)
  , FetchCache'R
  , FetchCache'State
  , FetchCache'ValState(..)
  , X'FetchCache
  , domFetchCache'ensureRequested
  , domFetchCache'forceRequest
  , domFetchCache'lookup
  , domFetchCache'lookupVal
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

type FetchCache'ValState v =
  { isLoading :: Boolean, value :: Maybe v, error :: Maybe JsError }

type FetchCache'State id v = HashMap id (FetchCache'ValState v)

type FetchCache'Action'row id v =
  ( markRequesting :: id
  , setVal :: id /\ v
  , setError :: id /\ JsError
  , importVals :: (HashMap id v)
  )

newtype FetchCache'Action id v =
  FetchCache'Action (Variant (FetchCache'Action'row id v))

derive instance Newtype (FetchCache'Action id v) _

fetchCache'lookupVal
  :: forall id v. Identable id => id -> FetchCache'State id v -> Maybe v
fetchCache'lookupVal id hm = hm'lookup id hm >>= _.value

fetchCache'valState'shouldFetch :: forall v. FetchCache'ValState v -> Boolean
fetchCache'valState'shouldFetch { isLoading: false, error } = isJust error
fetchCache'valState'shouldFetch _ = false

fetchCache'shouldFetch
  :: forall id v. Identable id => id -> FetchCache'State id v -> Boolean
fetchCache'shouldFetch id hm =
  may'or true $ hm'lookup id hm <#> fetchCache'valState'shouldFetch

valState'default :: forall v. FetchCache'ValState v
valState'default = { isLoading: true, value: Nothing, error: Nothing }

valState'fromVal :: forall v. v -> FetchCache'ValState v
valState'fromVal v = { isLoading: false, error: Nothing, value: Just v }

fetchCache'alter
  :: forall id v
   . Identable id
  => Eq v
  => id
  -> Edit (FetchCache'ValState v)
  -> FetchCache'State id v
  -> FetchCache'State id v
fetchCache'alter id edits stateHm = hm'insert id (edit curr edits) stateHm
  where
  currOr_ = hm'lookup id stateHm
  curr = may'or valState'default currOr_

fetchCache'xUpdate
  :: forall id v
   . Identable id
  => Eq v
  => FetchCache'Action id v
  -> Edit (FetchCache'State id v)
fetchCache'xUpdate = var'case_
  # var'on @"markRequesting"
      (\id -> s'update $ fetchCache'alter id $ s'set'b @"isLoading" true)
  # var'on @"setVal"
      (\(id /\ v) -> s'update $ hm'insert id $ valState'fromVal v)
  # var'on @"setError"
      ( \(id /\ e) -> s'update $ fetchCache'alter id do
          s'set'b @"isLoading" false *> s'setPure'b @"error" e
      )
  # var'on @"importVals"
      \hm'new -> s'update \hm'curr -> hm'merge
        [ hm'curr, map valState'fromVal hm'new ]

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
  => XDecodeJson v
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
  => XDecodeJson v
  => id
  -> MDomEff x Unit
domFetchCache'forceRequest id = do
  { mkFetchUrl } <- domState'get'' @p
  domEff'async do
    domState'dispatch'' @p $ var'inj @"markRequesting" id
    vOrE <- e'try $ do
      fetchRes <- x'fetch (mkFetchUrl id) pass
      decoded <- x'decodeFetched @v fetchRes
      e'ok $ constL (jsError "decode error" "") decoded
    vOrE # eOr'_
      (domState'dispatch'' @p <<< \v -> var'inj @"setError" $ id /\ v)
      (domState'dispatch'' @p <<< \v -> var'inj @"setVal" $ id /\ v)

type T'domFetchCache'run'helper'fn dr x' x id =
  (id -> String) -> MDom dr x' (Array id) -> MDom dr x Unit -> MDom dr x' Unit

type T'domFetchCache'run'helper p id v t'useResult =
  forall dr x' x
   . Identable id
  => Eq v
  => XDecodeJson v
  => Dom'ConsSymbol p (X'FetchCache id v) x' x
  => t'useResult (T'domFetchCache'run'helper'fn dr x' x id)

domFetchCache'run
  :: forall @p @id @v. T'domFetchCache'run'helper p id v $ (->) $ HashMap id v
domFetchCache'run cachePreload mkFetchUrl getId m = do
  currIds <- getId
  let init = { mkFetchUrl, state: cachePreload <#> valState'fromVal }
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
  domState'get'' @p <#> _.state <#> hm'lookup id <#> may'or valState'default

domFetchCache'lookupVal
  :: forall @p id v x' x
   . ConsSymbol p (X'FetchCache id v) x' x
  => Identable id
  => id
  -> Run x (Maybe v)
domFetchCache'lookupVal id =
  domState'get'' @p <#> _.state <#> fetchCache'lookupVal id