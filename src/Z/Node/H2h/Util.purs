module Z.Node.H2h.Util
  ( BuildX
  , adaptBuilder
  ) where

import Prelude

import Z.H2h.Index as H2h
import Z.Node.Gql.Index as Gql
import Z.Z as Z

type BuildX x = Z.RWEA
  { client :: Gql.Client, slug :: String, opts :: Gql.Opts }
  (Array H2h.Warning)
  H2h.Error
  x

adaptBuilder
  :: forall x
   . H2h.Event Z.<@ BuildX Z.$ x
  -> String
  -> Gql.Client
  -> Z.ModX Gql.Opts
  -> Z.Result H2h.Warning H2h.Error H2h.Event Z.<@ Z.A Z.$ x
adaptBuilder b slug client modOpts = Z.xResult $ Z.xReading env b
  where
  env = { slug, client, opts: Gql.fullOpts client modOpts }