module Node.Z.CLM.Stats.Manager.Spec where

import Z.Prelude

import Heterogeneous.Mapping (class Mapping)

type Spec =
  { eventSlugs :: HashSet String
  , challongeSlugs :: HashSet String
  , ineligibleSlugs :: HashSet String
  , doneUpdating :: HashSet String
  , eventsToRefetch :: HashSet String
  , currentPeriodId :: Int
  , tournamentNameOverrides :: Map String String
  }

data SetOp a = SetRm a | SetAdd a

data MapOp k v = MapRm k | MapSet k v

type SpecB =
  { eventSlugs :: List $ SetOp String
  , challongeSlugs :: List $ SetOp String
  , ineligibleSlugs :: List $ SetOp String
  , doneUpdating :: List $ SetOp String
  , eventsToRefetch :: List $ SetOp String
  , currentPeriodId :: Int
  , tournamentNameOverrides :: List $ MapOp String String
  }

data SetDef a
data MapDef k v
data ConstDef t

type SpecF f =
  { eventSlugs :: SetDef String -> f
  , challongeSlugs :: SetDef String -> f
  , ineligibleSlugs :: SetDef String -> f
  , ineligibleSlugs :: SetDef String -> f
  , doneUpdating :: SetDef String -> f
  , eventsToRefetch :: SetDef String -> f
  , tournamentNameOverrides :: MapDef String String -> f
  , currentPeriodId :: ConstDef Int
  }
