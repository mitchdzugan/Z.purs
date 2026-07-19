module Z.Node.H2h.Builder.Challonge where

import Prelude

import Z.H2h as H2h
import Z.Node.H2h.Util as U
import Z.Z as Z

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