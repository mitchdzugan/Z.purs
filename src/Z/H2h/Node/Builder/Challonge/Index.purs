module Z.H2h.Node.Builder.Challonge.Index where

import Prelude

import Z.H2h.Module as H2h
import Z.H2h.Node.Util as U
import Z as Z

getEventData :: forall x. U.GetDataFn x
getEventData = U.adaptBuilder
  do
    pure
      { id: Z.asOrNum "tourneyId"
      , name: "Melee Singles"
      , slug: "11111111"
      , state: "COMPLETE"
      , tournamentName: "Bracket at the Emporium 3"
      , site: H2h.Startgg
      }