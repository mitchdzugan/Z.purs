module Z.Z.String
  ( strEndsWith
  , strJoinWith
  , strLength
  , strSplit
  , strStartsWith
  ) where

import Data.String as Str
import Data.String.CodeUnits as StrCU
import Data.String.Common as StrCommon
import Data.String.Utils as StrUtils

strJoinWith :: String -> Array String -> String
strJoinWith = StrCommon.joinWith

strSplit ∷ Str.Pattern -> String -> Array String
strSplit = StrCommon.split

strLength :: String -> Int
strLength = StrCU.length

strStartsWith :: String -> String -> Boolean
strStartsWith = StrUtils.startsWith

strEndsWith :: String -> String -> Boolean
strEndsWith = StrUtils.endsWith