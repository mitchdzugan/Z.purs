module H2h.Node
  ( mkClient
  , mkClient'
  , module H2h
  ) where

import H2h.Core (BracketingSite(..), Error, Event, EventSource) as H2h
import Gql.Node as Gql
import Z as Z

mkClient :: Z.ModX Gql.Client -> Gql.Client
mkClient = Gql.mkClient "https://api.start.gg/gql/alpha"

mkClient' :: Gql.Client
mkClient' = mkClient Z.pass