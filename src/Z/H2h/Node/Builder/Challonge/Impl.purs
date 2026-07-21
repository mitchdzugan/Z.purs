module Z.H2h.Node.Builder.Challonge.Impl where

import Prelude

import Z.H2h.Module as H2h
import Z.H2h.Node.Builder.API as B
import Z as Z

x :: Int
x = 1

{-
getEventData :: forall x. B.GetDataFn x
getEventData = B.adaptBuilder
  do
    pure
      { id: Z.asOrNum "tourneyId"
      , name: "Melee Singles"
      , slug: "11111111"
      , state: "COMPLETE"
      , tournamentName: "Bracket at the Emporium 3"
      , site: H2h.Startgg
      , phaseGroups: Z.arrEmpty @H2h.PhaseGroup
      , entrants: Z.mapEmpty @Z.StringOrNum @H2h.Entrant
      , numEntrants: 0
      }
-}