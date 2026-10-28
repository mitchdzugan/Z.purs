module Z.Z.Fuzzy
  ( FuzzyOpts
  , FuzzyOpts'f
  , FuzzyResult
  , FuzzyResults
  , FuzzySnapshot
  , fuzzy'mkSnapshot
  , fuzzy'search
  , fuzzy'searchSnapshot
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..))
import Z.Z.Core (may'toArray)
import Z.Z.X.Index (Edit, edit)

fuzzy'mkSnapshot :: forall a. Array a -> Array (a -> String) -> FuzzySnapshot a
fuzzy'mkSnapshot = js_snapshot

fuzzy'searchSnapshot
  :: forall a. FuzzySnapshot a -> String -> FuzzyOptEdits a -> FuzzyResults a
fuzzy'searchSnapshot snapshot s =
  js_goSnapshot snapshot s <<< js_encodeArrayOpts <<< mkFuzzyOpts'array

fuzzy'search
  :: forall a
   . Array a
  -> Array (a -> String)
  -> String
  -> FuzzyOptEdits a
  -> FuzzyResults a
fuzzy'search els keys s =
  js_goRaw els keys s <<< js_encodeArrayOpts <<< mkFuzzyOpts'array

type FuzzyResult a =
  { obj :: a
  , score :: Number
  , target :: String
  , indexes :: Array Int
  }

type FuzzyResults a = Array (FuzzyResult a)

type FuzzyOpts'f f a =
  { limit :: f Int
  , threshold :: f Number
  , scoreFn :: f (FuzzyResult a -> Number)
  }

type FuzzyOpts a = FuzzyOpts'f Maybe a

type FuzzyOpts'array a = FuzzyOpts'f Array a

defaultFuzzyOpts :: forall a. FuzzyOpts a
defaultFuzzyOpts = { limit: Nothing, threshold: Nothing, scoreFn: Nothing }

type FuzzyOptEdits a = Edit (FuzzyOpts a)

mkFuzzyOpts'array :: forall a. FuzzyOptEdits a -> FuzzyOpts'array a
mkFuzzyOpts'array = edit defaultFuzzyOpts >>> case _ of
  { limit, threshold, scoreFn } ->
    { limit: may'toArray limit
    , threshold: may'toArray threshold
    , scoreFn: may'toArray scoreFn
    }

foreign import data FuzzySnapshot :: Type -> Type

foreign import js_snapshot
  :: forall a. Array a -> Array (a -> String) -> FuzzySnapshot a

foreign import js_goRaw
  :: forall a
   . Array a
  -> Array (a -> String)
  -> String
  -> Json
  -> FuzzyResults a

foreign import js_goSnapshot
  :: forall a. FuzzySnapshot a -> String -> Json -> FuzzyResults a

foreign import js_encodeArrayOpts :: forall a. FuzzyOpts'array a -> Json