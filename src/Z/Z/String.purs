module Z.Z.String
  ( str'endsWith
  , str'joinWith
  , str'length
  , str'split
  , str'startsWith
  ) where

import Data.String as Str
import Data.String.CodeUnits as StrCU
import Data.String.Common as StrCommon
import Data.String.Utils as StrUtils

str'joinWith :: String -> Array String -> String
str'joinWith = StrCommon.joinWith

str'split ∷ Str.Pattern -> String -> Array String
str'split = StrCommon.split

str'length :: String -> Int
str'length = StrCU.length

str'startsWith :: String -> String -> Boolean
str'startsWith = StrUtils.startsWith

str'endsWith :: String -> String -> Boolean
str'endsWith = StrUtils.endsWith