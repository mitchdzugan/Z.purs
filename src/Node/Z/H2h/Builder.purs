module Node.Z.H2h.Builder
  ( BuildX
  , GetDataFn
  , adaptBuilder
  ) where

import Z.Prelude

import Node.Z.Gql as Gql
import Z.H2h.Module as H2h

adaptBuilder
  :: forall x
   . BuildX x #> H2h.Event
  -> GetDataFn x
adaptBuilder b source client networkControl = mkDim @RunResult $ g @XRunR
  env
  b
  where
  env = { slug: source.slug, client, networkControl }

type BuildX x = RWaEA
  { client :: Gql.Client, slug :: String, networkControl :: Gql.NetworkControl }
  H2h.Warning
  H2h.Error
  x

type GetDataFn x =
  H2h.EventSource
  -> Gql.Client
  -> Gql.NetworkControl
  -> A x #> Result H2h.Warning H2h.Error H2h.Event
