module Z.Node.H2h.Index
  ( getEventData
  , mkClient
  , module H2h
  ) where

import Z.H2h.Index (BracketingSite(..), Error, Event, EventSource, startggSource, challongeSource) as H2h
import Z.Node.Gql.Index as Gql
import Z.Node.H2h.Builder.Challonge as Challonge
import Z.Node.H2h.Builder.Startgg as Startgg
import Z.Node.H2h.Util as U
import Z.Z as Z

mkClient :: Z.ModX Gql.Client -> Gql.Client
mkClient = Gql.mkClient "https://api.start.gg/gql/alpha"

getEventData :: forall x. U.GetDataFn x
getEventData source = getByBracketingSite source.site
  where
  getByBracketingSite H2h.Challonge = Challonge.getEventData source
  getByBracketingSite H2h.Startgg = Startgg.getEventData source