module Node.Z.H2h.Startgg.All
  ( GGPageSpecF
  , ggQueryAll
  , ggPageSpec
  ) where

import Z.Prelude

import Node.Z.Gql as Gql
import Node.Z.H2h.Startgg.Queries as GGQ

data GGPageSpecF v r pnr = GGPageSpecF
  (Lens' { | v } Int)
  (Lens' { | r } (GGQ.PageInfo pnr))

type GGPageSpec v r = Exists (GGPageSpecF v r)

ggPageSpec
  :: forall v r pnr
   . (Lens' { | v } Int)
  -> (Lens' { | r } (GGQ.PageInfo pnr))
  -> GGPageSpec v r
ggPageSpec pageL pageInfoL = mkExists $ GGPageSpecF pageL pageInfoL

ggQueryAll
  :: forall x v r
   . Gql.Operation { | v } { | r }
  -> { | v }
  -> Array (GGPageSpec v r)
  -> Gql.Client
  -> Gql.NetworkControl
  -> WaEA Gql.Warning Gql.Error x #> { | r }
ggQueryAll op initVars pageSpecs client networkControl = do
  let r = { client, networkControl, op }
  initRes <- Gql.xOperate op initVars client networkControl
  let initS = { vars: initVars, res: initRes }
  { res } <- g @XRunR r $ g @XExecS initS $ forM_ pageSpecs
    ggPageSpecHandle
  pure res

type QAllR v r =
  { client :: Gql.Client
  , networkControl :: Gql.NetworkControl
  , op :: Gql.Operation { | v } { | r }
  }

type QAllS v r = { vars :: { | v }, res :: { | r } }

type XPageSpecHandle x v r =
  RWaSEA (QAllR v r) Gql.Warning (QAllS v r) Gql.Error x #> Unit

ggPageSpecHandle :: forall x v r. GGPageSpec v r -> XPageSpecHandle x v r
ggPageSpecHandle = runExists ggPageSpecHandleImpl

ggPageSpecHandleImpl
  :: forall x v r pnr
   . GGPageSpecF v r pnr
  -> XPageSpecHandle x v r
ggPageSpecHandleImpl (GGPageSpecF pageL dataL) = do
  { client, networkControl, op } <- g @XAsk
  s'plus @"seenIds" set'empty $ loop op client networkControl
  where
  loop op client networkControl = do
    g @XToArrayOfS (_o_ @"res" @"nodes+.id" dataL) >>= \ids -> do
      (s'sets @"seenIds" $ set'fromFoldable ids)
    seenIds <- s'views @"seenIds"
    total <- g @XViewS (_o_ @"res" @"pageInfo.total" dataL)
    when (set'size seenIds < total) do
      g @XOver (_o @"vars" pageL) inc
      vars <- s'views @"vars"
      res <- Gql.xOperate op vars client networkControl
      let nodes = view (dataL # o_ @"nodes") res
      g @XOver (_o_ @"res" @"nodes" dataL)
        (flip (<>) $ arrFilter (\{ id } -> not $ set'has id seenIds) nodes)
      loop op client networkControl
