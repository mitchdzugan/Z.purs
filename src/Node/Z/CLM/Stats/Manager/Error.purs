module Node.Z.CLM.Stats.Manager.Error
  ( T(..)
  ) where

import Node.Z.Prelude

import Node.Z.Git.Client (GitError)
import Z.H2h.Module as H2h

data T
  = H2h H2h.Error
  | LoadLegacyData FSDataError
  | MissingPlacing Int Int SorN
  | MissingPlayer Int
  | MissingPlayerSeason Int Int
  | MissingSetEventId Int SorN
  | MissingTagData Int SorN
  | InvalidLegacyDate String Int
  | LegacyMissingClmId Int String
  | RimrafDocs JsError
  | RimrafDB JsError
  | WriteSeasonJson Int JsError
  | WritePlayerSeasonJson Int Int JsError
  | WriteBuildMeta
  | WriteJekyllConfig
  | MissingLegacyPlayerSeason Int String
  | MissingLegacyEventId Int String
  | MissingLegacyEventName Int String
  | MissingLegacyOpponentName String
  | DebugExit
  | GitClientError GitError
  | GitDirUnclean String String
  | GitIncorrectBranch String String String

derive instance Generic T _
instance EncodeJson T where
  encodeJson = genericEncodeJson
