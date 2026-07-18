module Z.Node.H2h.Index
  ( mkClient
  , module H2h
  ) where

import Z.H2h.Index (BracketingSite(..), Error, Event, EventSource) as H2h
import Z.Node.Gql.Index as Gql
import Z.Z as Z

mkClient :: Z.ModX Gql.Client -> Gql.Client
mkClient = Gql.mkClient "https://api.start.gg/gql/alpha"
