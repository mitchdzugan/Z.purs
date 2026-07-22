module Z.H2h.Node.Builder.API
  ( BuildX
  , GetDataFn
  , adaptBuilder
  ) where

import Prelude

import Z.H2h.Module as H2h
import Z.Gql.Node.Module as Gql
import Z as Z

adaptBuilder
  :: forall x
   . Z.X (BuildX x) H2h.Event
  -> GetDataFn x
adaptBuilder b source client networkControl = Z.xResult $ Z.xEvalR env b
  where
  env = { slug: source.slug, client, networkControl }

type BuildX x = Z.RWaEA
  { client :: Gql.Client, slug :: String, networkControl :: Gql.NetworkControl }
  H2h.Warning
  H2h.Error
  x

type GetDataFn x =
  H2h.EventSource
  -> Gql.Client
  -> Gql.NetworkControl
  -> Z.X (Z.A x) (Z.Result H2h.Warning H2h.Error H2h.Event)
