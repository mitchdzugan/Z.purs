module Z.Z.Eff.WholeHistoryRating
  ( Eff'Whr
  , Whr'Opts
  , Whr'Ratings
  , eff'whr'createGame
  , eff'whr'iterate
  , eff'whr'new
  , eff'whr'ratingsForPlayer
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Z.Z.Core (encodeOpts)
import Z.Z.X.Index (Edit, edit)

foreign import data Eff'Whr :: Type

type Whr'Opts = { w2 :: Maybe Int }
type Whr'Ratings = Array (Array Number)

foreign import js_whr_new :: Json -> Effect Eff'Whr

foreign import js_whr_createGame
  :: String -> String -> Int -> Int -> Eff'Whr -> Effect Unit

foreign import js_whr_iterate :: Int -> Eff'Whr -> Effect Unit

foreign import js_whr_ratingsForPlayer
  :: String -> Eff'Whr -> Effect Whr'Ratings

eff'whr'new :: Edit Whr'Opts -> Effect Eff'Whr
eff'whr'new opts = js_whr_new $ encodeOpts $ edit { w2: Nothing } opts

eff'whr'createGame :: String -> String -> Int -> Int -> Eff'Whr -> Effect Unit
eff'whr'createGame = js_whr_createGame

eff'whr'iterate :: Int -> Eff'Whr -> Effect Unit
eff'whr'iterate = js_whr_iterate

eff'whr'ratingsForPlayer :: String -> Eff'Whr -> Effect Whr'Ratings
eff'whr'ratingsForPlayer = js_whr_ratingsForPlayer