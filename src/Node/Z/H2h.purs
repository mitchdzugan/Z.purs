module Node.Z.H2h
  ( getEventData
  , mkClient
  , module H2h
  ) where

import Z.H2h.Module
  ( BracketingSite(..)
  , Error
  , Event
  , EventSource
  , Warning
  , challongeSource
  , startggSource
  ) as H2h
import Node.Z.Gql as Gql
import Node.Z.H2h.Challonge as Challonge
import Node.Z.H2h.Startgg as Startgg
import Node.Z.H2h.Builder as B
import Z.Prelude

mkClient :: Edit Gql.Client -> Gql.Client
mkClient = Gql.mkClient "https://api.start.gg/gql/alpha"

getEventData :: forall x. B.GetDataFn x
getEventData source = getByBracketingSite source.site
  where
  getByBracketingSite H2h.Challonge = Challonge.getEventData source
  getByBracketingSite H2h.Startgg = Startgg.getEventData source