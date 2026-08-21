module Node.Z.CLM.Stats.Manager.Action where

import Z.Prelude

import Node.Z.CLM.Stats.Manager.Spec (Spec, Spec'ListOp)

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
  arr'withInd actionsIn <#> \(a /\ locId) -> case a of
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

handleAction :: forall r. Action r -> Edit $ Spec'ListOp
handleAction (OverrideName { slug, name }) =
  s'overs @"tournamentNameOverrides" $ Cons $ B'HashMap'set slug name
handleAction (MarkChallonge { slug }) =
  s'overs @"challongeSlugs" $ Cons $ B'HashSet'add slug
handleAction (AddEvent { slug }) =
  s'overs @"eventSlugs" $ Cons $ B'HashSet'add slug
handleAction (RemoveEvent { slug }) =
  s'overs @"eventSlugs" $ Cons $ B'HashSet'rm slug
handleAction (SetIsPrEligible { slug, isEligible }) =
  s'overs @"eventSlugs" $ Cons $
    (if isEligible then B'HashSet'add else B'HashSet'rm) slug
handleAction (MarkDoneUpdating { slug }) =
  s'overs @"doneUpdating" $ Cons $ B'HashSet'add slug
handleAction (BustCache { slug }) =
  s'overs @"eventsToRefetch" $ Cons $ B'HashSet'add slug
handleAction (SetCurrentPeriodId { periodId }) =
  s'overs @"currentPeriodId" $ Cons $ B'ConstVia'is periodId
handleAction (Bulk { actions }) = forM_ actions handleAction
handleAction (Undo _) = pure unit

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

buildSpec :: Array PureAction -> Spec
buildSpec actions = b'hmapBuild do
  let impureActions = impurifyActions actions
  let revActions = arr'reverse impureActions
  undone <- pure $ b'finish @(B'HashSet'Op String) $ objST'run do
    init <- objST'new
    reducer <- pure \undone' action -> case action of
      (Undo { id, targetId }) -> do
        isUndone <- objST'has id undone'
        if isUndone then pure undone'
        else objST'poke targetId targetId undone'
      _ -> pure undone'
    reduceM reducer init revActions
  s'overs @"undone" $ Cons $ B'Const'is undone
  forM_ impureActions \action -> do
    when (not $ hs'has (actionId action) undone) $ handleAction action