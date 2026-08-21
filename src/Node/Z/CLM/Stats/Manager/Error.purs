module Node.Z.CLM.Stats.Manager.Error
  ( T(..)
  ) where

import Node.Z.Prelude

import Z.H2h.Module as H2h

data T
  = H2h H2h.Error
  | LoadLegacyData FSDataError

derive instance Generic T _
instance EncodeJson T where
  encodeJson = genericEncodeJson