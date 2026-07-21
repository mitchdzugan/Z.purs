module Z.H2h.Node.Builder.Startgg.All
  ( GGPageSpecF
  , ggQueryAll
  , ggPageSpec
  ) where

import Prelude

import Z.Gql.Node.Module as Gql
import Z.H2h.Node.Builder.Startgg.Queries as GGQ
import Z as Z

data GGPageSpecF v r pnr = GGPageSpecF
  (Z.Lens' { | v } Int)
  (Z.Lens' { | r } (GGQ.PageInfo pnr))

type GGPageSpec v r = Z.Exists (GGPageSpecF v r)

ggPageSpec
  :: forall v r pnr
   . (Z.Lens' { | v } Int)
  -> (Z.Lens' { | r } (GGQ.PageInfo pnr))
  -> GGPageSpec v r
ggPageSpec pageL pageInfoL = Z.mkExists $ GGPageSpecF pageL pageInfoL

ggQueryAll
  :: forall x v r
   . Gql.Operation { | v } { | r }
  -> { | v }
  -> Array (GGPageSpec v r)
  -> Gql.Client
  -> Z.Edit Gql.Opts
  -> Z.X (Z.WaEA Gql.Warning Gql.Error x) { | r }
ggQueryAll op initVars pageSpecs client optsEdit = do
  let r = { client, optsEdit, op }
  initRes <- Gql.operate op initVars client optsEdit
  let initS = { vars: initVars, res: initRes }
  { res } <- Z.xEvalR r $ Z.xRunS initS $ Z.forM_ pageSpecs ggPageSpecHandle
  pure res

type QAllR v r =
  { client :: Gql.Client
  , optsEdit :: Z.Edit Gql.Opts
  , op :: Gql.Operation { | v } { | r }
  }

type QAllS v r = { vars :: { | v }, res :: { | r } }

type XPageSpecHandle x v r =
  Z.X (Z.RWaSEA (QAllR v r) Gql.Warning (QAllS v r) Gql.Error x) Unit

ggPageSpecHandle :: forall x v r. GGPageSpec v r -> XPageSpecHandle x v r
ggPageSpecHandle = Z.runExists ggPageSpecHandleImpl

ggPageSpecHandleImpl
  :: forall x v r pnr
   . GGPageSpecF v r pnr
  -> XPageSpecHandle x v r
ggPageSpecHandleImpl (GGPageSpecF pageL pageInfoL) = do
  { client, optsEdit, op } <- Z.xAsk
  currState <- Z.xGet
  let localState = Z.merge currState { seenIds: Z.setEmpty @Int }
  Z.xEvalS localState $ Z.xWithRet $ looper op client optsEdit
  where
  updateState = do
    nodes <- Z.xView (Z.px @"res" <<< pageInfoL <<< Z.px @"nodes")
    let seenIds = Z.setFromFoldable $ map (\el -> el.id) nodes
    Z.xSet (Z.px @"seenIds") seenIds
    Z.xOver (Z.px @"vars" <<< pageL) Z.inc
  looper op client optsEdit = do
    updateState
    seenIds <- Z.xView (Z.px @"seenIds")
    total <- Z.xView
      (Z.px @"res" <<< pageInfoL <<< Z.ppx @"pageInfo" @"total")
    when (Z.setSize seenIds >= total) $ Z.xReturn Z.default
    vars <- Z.xView (Z.px @"vars")
    res <- Z.xRetLift $ Gql.operate op vars client optsEdit
    let nodes = Z.view (pageInfoL <<< Z.px @"nodes") res
    Z.xOver (Z.px @"res" <<< pageInfoL <<< Z.px @"nodes")
      (flip (<>) $ Z.arrFilter (\node -> not $ Z.setHas node.id seenIds) nodes)
    looper op client optsEdit

