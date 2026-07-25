module Z.Z.String
  ( strJoinWith
  , strLength
  , strSplit
  ) where

import Data.String as Str
import Data.String.Common as StrCommon
import Data.String.CodeUnits as StrCU

strJoinWith :: String -> Array String -> String
strJoinWith = StrCommon.joinWith

strSplit ∷ Str.Pattern -> String -> Array String
strSplit = StrCommon.split

strLength :: String -> Int
strLength = StrCU.length