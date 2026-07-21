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
adaptBuilder b source client optsEdit = Z.xResult $ Z.xEvalR env b
  where
  env = { slug: source.slug, client, optsEdit }

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
