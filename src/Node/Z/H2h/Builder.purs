module Node.Z.H2h.Builder
  ( RunBuild
  , BuildXR
  , GetDataFn
  , adaptBuilder
  ) where

import Z.Prelude

import Node.Z.Gql as Gql
import Z.H2h.Module as H2h

adaptBuilder
  :: forall x
   . RunBuild x H2h.Event
  -> GetDataFn x
adaptBuilder b source client networkControl = we'runResult $ r'run env b
  where
  env = { slug: source.slug, client, networkControl }

type BuildXR =
  { client :: Gql.Client, slug :: String, networkControl :: Gql.NetworkControl }

type RunBuild x a = RWaE' BuildXR H2h.Warning H2h.Error x @@^-> a

type GetDataFn x =
  H2h.EventSource
  -> Gql.Client
  -> Gql.NetworkControl
  -> x @@^-> Result H2h.Warning H2h.Error H2h.Event
