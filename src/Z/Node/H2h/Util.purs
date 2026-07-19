module Z.Node.H2h.Util
  ( BuildX
  , GetDataFn
  , adaptBuilder
  ) where

import Prelude

import Z.H2h.Index as H2h
import Z.Node.Gql as Gql
import Z.Z as Z

type BuildX x = Z.RWEA
  { client :: Gql.Client, slug :: String, modOpts :: Z.ModX Gql.Opts }
  (Array H2h.Warning)
  H2h.Error
  x

type GetDataFn x =
  H2h.EventSource
  -> Gql.Client
  -> Z.ModX Gql.Opts
  -> Z.Result H2h.Warning H2h.Error H2h.Event Z.<@ Z.A Z.$ x

adaptBuilder
  :: forall x
   . H2h.Event Z.<@ BuildX Z.$ x
  -> GetDataFn x
adaptBuilder b source client modOpts = Z.xResult $ Z.xReading env b
  where
  env = { slug: source.slug, client, modOpts }
