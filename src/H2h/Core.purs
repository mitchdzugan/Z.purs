module H2h.Core where

import Prelude

data BracketingSite = StartGG | Challonge

type EventSource =
  { site :: BracketingSite
  , slug :: String
  }