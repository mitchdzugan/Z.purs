module Z.Node.H2h.Index
  ( getEventData
  , mkClient
  ) where

import Z.H2h as H2h
import Z.Node.Gql as Gql
import Z.Node.H2h.Builder.Challonge.Index as Challonge
import Z.Node.H2h.Builder.Startgg.Index as Startgg
import Z.Node.H2h.Util as U
import Z.Z as Z

mkClient :: Z.Edit Gql.Client -> Gql.Client
mkClient = Gql.mkClient "https://api.start.gg/gql/alpha"

getEventData :: forall x. U.GetDataFn x
getEventData source = getByBracketingSite source.site
  where
  getByBracketingSite H2h.Challonge = Challonge.getEventData source
  getByBracketingSite H2h.Startgg = Startgg.getEventData source