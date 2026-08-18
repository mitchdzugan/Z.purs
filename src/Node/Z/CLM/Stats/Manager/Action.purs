module Node.Z.CLM.Stats.Manager.Action where

import Z.Prelude

import Node.Z.CLM.Stats.Manager.Spec (MapOp(..), SetOp(..), Spec, SpecB)

data Action r
  = Undo { targetId :: Int | r }
  | Bulk { actions :: Array (Action r) | r }
  | OverrideName { slug :: String, name :: String | r }
  | MarkChallonge { slug :: String | r }
  | AddEvent { slug :: String | r }
  | SetIsPrEligible { slug :: String, isEligible :: Boolean | r }
  | MarkDoneUpdating { slug :: String | r }
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
  (PureAction (SetIsPrEligible p)) -> "SetIsPrEligible" /\ encodeJson p
  (PureAction (MarkDoneUpdating p)) -> "MarkDoneUpdating" /\ encodeJson p
  (PureAction (BustCache p)) -> "BustCache" /\ encodeJson p
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
    SetIsPrEligible props -> SetIsPrEligible $ plusId props locId
    MarkDoneUpdating props -> MarkDoneUpdating $ plusId props locId
    BustCache props -> BustCache $ plusId props locId
    Bulk { actions } ->
      Bulk { actions: assignIds (extId locId) actions, id: extId locId }
  where
  plusId :: forall p. T'useAsSym "id" p T'plusId
  plusId props locId = rec'insert (p @p) (extId locId) props
  extId locId = idBase <> "|" <> show locId

isEphemeral :: forall r. Action r -> Boolean
isEphemeral (BustCache _) = true
isEphemeral _ = false

ejectEphemerals :: forall r. Array (Action r) -> Array (Action r)
ejectEphemerals actions = arr'filter isEphemeral actions <#> case _ of
  (Bulk props) -> Bulk $ rec'modify (p @"actions") ejectEphemerals props
  other -> other

impurifyActions :: Array PureAction -> Array (Action (id :: String))
impurifyActions a = assignIds "" $ a <#> un'

handleAction :: forall r. Action r -> Edit SpecB
handleAction (OverrideName { slug, name }) =
  g @(XOver_ "tournamentNameOverrides") $ Cons $ MapSet slug name
handleAction (MarkChallonge { slug }) =
  g @(XOver_ "challongeSlugs") $ Cons $ SetAdd slug
handleAction (AddEvent { slug }) =
  g @(XOver_ "eventSlugs") $ Cons $ SetAdd slug
handleAction (SetIsPrEligible { slug, isEligible }) =
  g @(XOver_ "eventSlugs") $ Cons $ (if isEligible then SetAdd else SetRm) slug
handleAction (MarkDoneUpdating { slug }) =
  g @(XOver_ "doneUpdating") $ Cons $ SetAdd slug
handleAction (BustCache { slug }) =
  g @(XOver_ "eventsToRefetch") $ Cons $ SetAdd slug
handleAction (Bulk { actions }) = forM_ actions handleAction
handleAction (Undo _) = pure unit
