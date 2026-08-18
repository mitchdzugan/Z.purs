module Node.Z.CLM.Stats.Queries where

import Z.Prelude

import Node.Z.CLM.Stats.Queries.ClmEvents as CLMEQ
import Node.Z.Gql as Gql
import Node.Z.H2h.Startgg.Queries as GGQ

type CLMEventsRes =
  { tournaments :: GGQ.PageInfo (events :: Array { slug :: String }) }

type CLMEventsVars = { page :: Int, after :: Int, before :: Int }

clmEvents :: Gql.Operation CLMEventsVars CLMEventsRes
clmEvents = Gql.defOperation CLMEQ.q Proxy Proxy
