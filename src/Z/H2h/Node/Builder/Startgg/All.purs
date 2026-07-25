module Z.H2h.Node.Builder.Startgg.All
  ( GGPageSpecF
  , ggQueryAll
  , ggPageSpec
  ) where

import Prelude

import Z.Z.Shorthand (_o, _o_, o_, type (#>))
import Z as Z
import Z.Gql.Node.Module as Gql
import Z.H2h.Node.Builder.Startgg.Queries as GGQ

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
  -> Gql.NetworkControl
  -> Z.WaEA Gql.Warning Gql.Error x #> { | r }
ggQueryAll op initVars pageSpecs client networkControl = do
  let r = { client, networkControl, op }
  initRes <- Gql.operate op initVars client networkControl
  let initS = { vars: initVars, res: initRes }
  { res } <- Z.xEvalR r $ Z.xRunS initS $ Z.forM_ pageSpecs ggPageSpecHandle
  pure res

type QAllR v r =
  { client :: Gql.Client
  , networkControl :: Gql.NetworkControl
  , op :: Gql.Operation { | v } { | r }
  }

type QAllS v r = { vars :: { | v }, res :: { | r } }

type XPageSpecHandle x v r =
  Z.RWaSEA (QAllR v r) Gql.Warning (QAllS v r) Gql.Error x #> Unit

ggPageSpecHandle :: forall x v r. GGPageSpec v r -> XPageSpecHandle x v r
ggPageSpecHandle = Z.runExists ggPageSpecHandleImpl

ggPageSpecHandleImpl
  :: forall x v r pnr
   . GGPageSpecF v r pnr
  -> XPageSpecHandle x v r
ggPageSpecHandleImpl (GGPageSpecF pageL dataL) = do
  { client, networkControl, op } <- Z.xAsk
  Z.xPlusS @"seenIds" Z.setEmpty $ loop op client networkControl
  where
  loop op client networkControl = do
    Z.xToArrayOf (_o_ @"res" @"nodes+.id" dataL) >>= \ids -> do
      (Z.xSet_ @"seenIds" $ Z.setFromFoldable ids)
    seenIds <- Z.xView_ @"seenIds"
    total <- Z.xView (_o_ @"res" @"pageInfo.total" dataL)
    when (Z.setSize seenIds < total) do
      Z.xOver (_o @"vars" pageL) Z.inc
      vars <- Z.xView_ @"vars"
      res <- Gql.operate op vars client networkControl
      let nodes = Z.view (dataL # o_ @"nodes") res
      Z.xOver (_o_ @"res" @"nodes" dataL)
        (flip (<>) $ Z.arrFilter (\{ id } -> not $ Z.setHas id seenIds) nodes)
      loop op client networkControl
