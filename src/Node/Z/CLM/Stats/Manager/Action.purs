module Node.Z.CLM.Stats.Manager.Action where

import Z.Prelude
import Z.Z.X6.Index

import Node.Z.CLM.Stats.Manager.Spec (Spec, Spec'Evaluable, Spec'M)
import Z.Z.X6.Core (x'build)

data Action r
  = Undo { targetId :: String | r }
  | Bulk { actions :: Array (Action r) | r }
  | OverrideName { slug :: String, name :: String | r }
  | MarkChallonge { slug :: String | r }
  | AddEvent { slug :: String | r }
  | RemoveEvent { slug :: String | r }
  | SetIsPrEligible { slug :: String, isEligible :: Boolean | r }
  | MarkDoneUpdating { slug :: String | r }
  | SetCurrentPeriodId { periodId :: Int | r }
  | BustCache { slug :: String | r }

-- | AddAltId { baseId :: String, newId :: String }
-- | IdForEvent { baseId :: String, newId :: String, slug :: String }
-- | OverrideSetData { setId :: String, slug :: String }

newtype PureAction = PureAction (Action ())

derive instance Newtype PureAction _
instance EncodeJson PureAction where
  encodeJson a = encodeJson $ encodePureAction a

instance DecodeJson PureAction where
  decodeJson j = baseDecodeJson j >>= decodePureAction

encodePureAction :: PureAction -> String /\ Json
encodePureAction action = case action of
  (PureAction (Undo p)) -> "Undo" /\ encodeJson p
  (PureAction (OverrideName p)) -> "OverrideName" /\ encodeJson p
  (PureAction (MarkChallonge p)) -> "MarkChallonge" /\ encodeJson p
  (PureAction (AddEvent p)) -> "AddEvent" /\ encodeJson p
  (PureAction (RemoveEvent p)) -> "RemoveEvent" /\ encodeJson p
  (PureAction (SetIsPrEligible p)) -> "SetIsPrEligible" /\ encodeJson p
  (PureAction (MarkDoneUpdating p)) -> "MarkDoneUpdating" /\ encodeJson p
  (PureAction (BustCache p)) -> "BustCache" /\ encodeJson p
  (PureAction (SetCurrentPeriodId p)) -> "SetCurrentPeriodId" /\ encodeJson p
  (PureAction (Bulk p)) -> (/\) "Bulk" $ encodeJson (PureAction <$> p.actions)

decodePureAction :: String /\ Json -> Either RawJsonDecodeError PureAction
decodePureAction = case _ of
  ("Undo" /\ p) -> d p <#> PureAction <<< Undo
  ("OverrideName" /\ p) -> d p <#> PureAction <<< OverrideName
  ("MarkChallonge" /\ p) -> d p <#> PureAction <<< MarkChallonge
  ("AddEvent" /\ p) -> d p <#> PureAction <<< AddEvent
  ("SetIsPrEligible" /\ p) -> d p <#> PureAction <<< SetIsPrEligible
  ("MarkDoneUpdating" /\ p) -> d p <#> PureAction <<< MarkDoneUpdating
  ("BustCache" /\ p) -> d p <#> PureAction <<< BustCache
  ("SetCurrentPeriodId" /\ p) -> d p <#> PureAction <<< SetCurrentPeriodId
  ("Bulk" /\ p) -> d p <#> finishBulk
  (actionLbl /\ _) -> decodeFailTypeMismatch $ "Unknown Action: " <> actionLbl
  where
  finishBulk :: Array PureAction -> PureAction
  finishBulk actions = PureAction $ Bulk { actions: actions <#> un' }
  d = baseDecodeJson

type T'plusId_h p r' r = Cons p String r' r => { | r' } -> Int -> { | r }
type T'plusId p = forall r' r. Lacks p r' => IsSymbol p => T'plusId_h p r' r

assignIds :: String -> Array (Action ()) -> Array (Action (id :: String))
assignIds idBase actionsIn =
  arr'withInd actionsIn <#> \(locId /\ a) -> case a of
    Undo props -> Undo $ plusId props locId
    OverrideName props -> OverrideName $ plusId props locId
    MarkChallonge props -> MarkChallonge $ plusId props locId
    AddEvent props -> AddEvent $ plusId props locId
    RemoveEvent props -> RemoveEvent $ plusId props locId
    SetIsPrEligible props -> SetIsPrEligible $ plusId props locId
    MarkDoneUpdating props -> MarkDoneUpdating $ plusId props locId
    BustCache props -> BustCache $ plusId props locId
    SetCurrentPeriodId props -> SetCurrentPeriodId $ plusId props locId
    Bulk { actions } ->
      Bulk { actions: assignIds (extId locId) actions, id: extId locId }
  where
  plusId :: forall p. T'useAsSym "id" p T'plusId
  plusId props locId = rec'insert @p (extId locId) props
  extId locId = idBase <> "|" <> show locId

isEphemeral :: forall r. Action r -> Boolean
isEphemeral (BustCache _) = true
isEphemeral _ = false

ejectEphemerals :: forall r. Array (Action r) -> Array (Action r)
ejectEphemerals actions = arr'filter isEphemeral actions <#> case _ of
  (Bulk props) -> Bulk $ rec'modify @"actions" ejectEphemerals props
  other -> other

impurifyActions :: Array PureAction -> Array (Action (id :: String))
impurifyActions a = assignIds "" $ a <#> un'

actionId :: Action (id :: String) -> String
actionId (Undo props) = props.id
actionId (OverrideName props) = props.id
actionId (MarkChallonge props) = props.id
actionId (AddEvent props) = props.id
actionId (RemoveEvent props) = props.id
actionId (SetIsPrEligible props) = props.id
actionId (MarkDoneUpdating props) = props.id
actionId (BustCache props) = props.id
actionId (SetCurrentPeriodId props) = props.id
actionId (Bulk props) = props.id

handleAction :: forall x r. Action r -> Spec'M x @@> Unit
handleAction (OverrideName { slug, name }) =
  x'insert @"tournamentNameOverrides" slug name
handleAction (MarkChallonge { slug }) = x'cons @"challongeSlugs" slug
handleAction (AddEvent { slug }) = x'cons @"eventSlugs" slug
handleAction (RemoveEvent { slug }) = x'remove @"eventSlugs" slug
handleAction (SetIsPrEligible { slug, isEligible }) = slug #
  if isEligible then x'remove @"ineligibleSlugs" else x'cons @"ineligibleSlugs"
handleAction (MarkDoneUpdating { slug }) = x'cons @"doneUpdating" slug
handleAction (BustCache { slug }) = x'cons @"eventsToRefetch" slug
handleAction (SetCurrentPeriodId { periodId }) =
  x'assign @"currentPeriodId" periodId
handleAction (Bulk { actions }) = forM_ actions handleAction
handleAction (Undo _) = pure unit

buildSpec :: Array PureAction -> Spec
buildSpec actions = sync'x $ x'build (g @Spec'Evaluable) do
  let impureActions = impurifyActions actions
  let revActions = arr'reverse impureActions
  forM_ revActions $ case _ of
    (Undo { id, targetId }) -> do
      isUndone <- x'has @"undone" id
      when (not isUndone) do
        x'cons @"undone" targetId
    _ -> pure unit
  forM_ impureActions \action -> do
    isUndone <- x'has @"undone" $ actionId action
    when (not $ isUndone) $ handleAction action
