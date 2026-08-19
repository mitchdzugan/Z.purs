module Node.Z.CLM.Stats.Manager.Spec
  ( Spec
  , Spec'B'Def
  , Spec'ListOp
  ) where

import Z.Prelude

type Spec'B'Def k =
  { eventSlugs :: B'HashSet'Def k String
  , challongeSlugs :: B'HashSet'Def k String
  , ineligibleSlugs :: B'HashSet'Def k String
  , doneUpdating :: B'HashSet'Def k String
  , eventsToRefetch :: B'HashSet'Def k String
  , tournamentNameOverrides :: B'Map'Def k String String
  , currentPeriodId :: B'ConstVia'Def k D'Int'0 Int
  }

type Spec'ListOp = Spec'B'Def B'Def'ListOp
type Spec = Spec'B'Def B'Def'Built
