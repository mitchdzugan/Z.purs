module Node.H2h.Index
  ( mkClient
  , mkClient'
  , module H2h
  ) where

import H2h.Index (BracketingSite(..), Error, Event, EventSource) as H2h
import Node.Gql.Index as Gql
import Z as Z

mkClient :: Z.ModX Gql.Client -> Gql.Client
mkClient = Gql.mkClient "https://api.start.gg/gql/alpha"

mkClient' :: Gql.Client
mkClient' = mkClient Z.pass