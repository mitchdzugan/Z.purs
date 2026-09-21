module Node.Z.CLM.Stats.Manager.Spec
  ( Spec
  , Spec'Def
  , Spec'Evaluable
  , Spec'M
  ) where

import Z.Prelude

import Z.Z.X.Core (Def'Sel'Evaluable, Def'Sel'm, Def'Sel'result)
import Z.Z.X.Readables.RW.HashMap (B'HashMap)
import Z.Z.X.Readables.RW.HashSet (B'HashSet)
import Z.Z.X.Readables.RW.Ref (B'Ref'nt)

type Spec'Def :: forall k1. ((Type -> Type) -> Type -> k1) -> Row k1 -> Row k1
type Spec'Def k r =
  ( eventSlugs :: B'HashSet k String
  , challongeSlugs :: B'HashSet k String
  , ineligibleSlugs :: B'HashSet k String
  , doneUpdating :: B'HashSet k String
  , eventsToRefetch :: B'HashSet k String
  , tournamentNameOverrides :: B'HashMap k String String
  , currentPeriodId :: B'Ref'nt k Int'Default0 Int
  , undone :: B'HashSet k String
  | r
  )

type Spec'Evaluable = Record (Spec'Def Def'Sel'Evaluable ())

type Spec'M x = Spec'Def Def'Sel'm x

type Spec = Record (Spec'Def Def'Sel'result ())
