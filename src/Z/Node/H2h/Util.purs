module Z.Node.H2h.Util
  ( BuildX
  , GetDataFn
  , adaptBuilder
  , ggQueryAll
  , runGGQueryAll
  ) where

import Prelude

import Z.H2h.Index as H2h
import Z.Node.Gql as Gql
import Z.Node.H2h.Builder.Startgg.Queries as GGQ
import Z.Z as Z

type BuildX x = Z.RWaEA
  { client :: Gql.Client, slug :: String, optsEdit :: Z.Edit Gql.Opts }
  H2h.Warning
  H2h.Error
  x

type GetDataFn x =
  H2h.EventSource
  -> Gql.Client
  -> Z.Edit Gql.Opts
  -> Z.X (Z.A x) (Z.Result H2h.Warning H2h.Error H2h.Event)

adaptBuilder
  :: forall x
   . Z.X (BuildX x) H2h.Event
  -> GetDataFn x
adaptBuilder b source client optsEdit = Z.xResult $ Z.xEvalR env b
  where
  env = { slug: source.slug, client, optsEdit }

type QAllR v r =
  { client :: Gql.Client
  , optsEdit :: Z.Edit Gql.Opts
  , op :: Gql.Operation { | v } { | r }
  }

type QAllS' v r x = { vars :: { | v }, res :: { | r } | x }
type QAllS v r = QAllS' v r ()

propl
  :: forall r r' l a
   . Z.IsSymbol l
  => Z.Cons l a r' r
  => Z.Proxy l
  -> Z.Proxy a
  -> Z.Lens' { | r } a
propl p _ = Z.prop p

ggQueryAll
  :: forall x v r pnr
   . Z.Lens' { | v } Int
  -> Z.Lens' { | r } (GGQ.PageInfo pnr)
  -> Z.X (Z.RWaSEA (QAllR v r) Gql.Warning (QAllS v r) Gql.Error x) Unit
ggQueryAll pageL pageInfoL = do
  { client, optsEdit, op } <- Z.xAsk
  currState <- Z.xGet
  let localState = Z.merge currState { seenIds: Z.setEmpty :: Z.Set Int }
  Z.xEvalS localState $ Z.xWithRet $ looper op client optsEdit
  where
  updateState = do
    nodes <- Z.xView _nodes
    let seenIds = Z.setFromFoldable $ map (\el -> el.id) nodes
    Z.xSet _seenIds seenIds
    Z.xOver _page Z.inc
  looper op client optsEdit = do
    updateState
    seenIds <- Z.xView _seenIds
    total <- Z.xView _total
    when (Z.setSize seenIds >= total) $ Z.xReturn Z.default
    vars <- Z.xView _vars
    res <- Z.xRetLift $ Gql.operate op vars client optsEdit
    let nodes = Z.view _nodes { res }
    Z.xOver _nodes
      (\curr -> curr <> Z.arrFilter (\x -> not $ Z.setHas x.id seenIds) nodes)
    looper op client optsEdit

  _seenIds :: forall q. Z.Lens' { seenIds :: Z.Set Int | q } (Z.Set Int)
  _seenIds = Z.prop (Z.Proxy @"seenIds")

  _vars :: forall q. Z.Lens' { vars :: { | v } | q } { | v }
  _vars = Z.prop (Z.Proxy @"vars")

  _page :: forall q. Z.Lens' { vars :: { | v } | q } Int
  _page = _vars <<< pageL

  _res :: forall q. Z.Lens' { res :: { | r } | q } { | r }
  _res = Z.prop $ Z.Proxy @"res"

  _pageInfo :: forall q. Z.Lens' { res :: { | r } | q } (GGQ.PageInfo pnr)
  _pageInfo = _res <<< pageInfoL

  _nodes :: forall q. Z.Lens' { res :: { | r } | q } (GGQ.PageNodes pnr)
  _nodes = _res <<< pageInfoL <<< Z.prop (Z.Proxy @"nodes")

  _total :: forall q. Z.Lens' { res :: { | r } | q } Int
  _total =
    _pageInfo <<< Z.prop (Z.Proxy @"pageInfo") <<< propl (Z.Proxy @"total")
      (Z.Proxy @Int)

runGGQueryAll
  :: forall x v r
   . Gql.Operation { | v } { | r }
  -> Z.X (Z.RWaSEA (QAllR v r) Gql.Warning (QAllS v r) Gql.Error x) Unit
  -> { | v }
  -> Gql.Client
  -> Z.Edit Gql.Opts
  -> Z.X (Z.WaEA Gql.Warning Gql.Error x) { | r }
runGGQueryAll op m initVars client optsEdit = do
  let r = { client, optsEdit, op }
  initRes <- Gql.operate op initVars client optsEdit
  let initS = { vars: initVars, res: initRes }
  { res } <- Z.xEvalR r $ Z.xRunS initS m
  pure res
