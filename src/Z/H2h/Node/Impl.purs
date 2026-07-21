module Z.H2h.Node.Impl
  ( getEventData
  , mkClient
  ) where

import Z.H2h.Module as H2h
import Z.Gql.Node.Module as Gql
import Z.H2h.Node.Builder.Challonge.Index as Challonge
import Z.H2h.Node.Builder.Startgg.Index as Startgg
import Z.H2h.Node.Util as U
import Z as Z

mkClient :: Z.Edit Gql.Client -> Gql.Client
mkClient = Gql.mkClient "https://api.start.gg/gql/alpha"

getEventData :: forall x. U.GetDataFn x
getEventData source = getByBracketingSite source.site
  where
  getByBracketingSite H2h.Challonge = Challonge.getEventData source
  getByBracketingSite H2h.Startgg = Startgg.getEventData source