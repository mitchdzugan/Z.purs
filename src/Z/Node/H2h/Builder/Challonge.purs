module Z.Node.H2h.Builder.Challonge where

import Prelude

import Z.H2h.Index as H2h
import Z.Node.H2h.Util as U
import Z.Z as Z

getEventData :: forall x. U.GetDataFn x
getEventData = U.adaptBuilder
  do
    pure
      { id: Z.asOrNum "tourneyId"
      , name: "Melee Singles"
      , slug: "tournament/bracket-at-the-emporium-3/event/melee-singles"
      , state: "COMPLETE"
      , tournamentName: "Bracket at the Emporium 3"
      , site: H2h.Startgg
      }