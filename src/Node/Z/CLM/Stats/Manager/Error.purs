module Node.Z.CLM.Stats.Manager.Error
  ( T(..)
  ) where

import Node.Z.Prelude

import Z.H2h.Module as H2h

data T
  = H2h H2h.Error
  | LoadLegacyData FSDataError
  | MissingPlacing Int Int SorN
  | MissingPlayer Int
  | InvalidLegacyDate String Int
  | LegacyMissingClmId Int String
  | RimrafDocs JsError
  | WriteSeasonJson Int JsError
  | DebugExit

derive instance Generic T _
instance EncodeJson T where
  encodeJson = genericEncodeJson