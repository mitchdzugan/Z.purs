module Z.Z.String
  ( strJoinWith
  , strSplit
  ) where

import Data.String as Str
import Data.String.Common as StrCommon

strJoinWith :: String -> Array String -> String
strJoinWith = StrCommon.joinWith

strSplit ∷ Str.Pattern -> String -> Array String
strSplit = StrCommon.split