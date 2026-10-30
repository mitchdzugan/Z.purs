module Z.Z.X.Readables.RW.WholeHistoryRating
  ( R'Whr(..)
  , X'Whr
  , whr'createGame
  , whr'createGame''
  , whr'eval
  , whr'eval''
  , whr'iterate
  , whr'iterate''
  , whr'ratingsForPlayer
  , whr'ratingsForPlayer''
  ) where

import Z.Z.X.UtilPrelude

import Z.Z.Core (T'useAsSym)
import Z.Z.Eff.WholeHistoryRating
  ( Whr'Opts
  , Whr'Ratings
  , eff'whr'createGame
  , eff'whr'iterate
  , eff'whr'new
  , eff'whr'ratingsForPlayer
  )
import Z.Z.X.Core
  ( class X'Readable
  , x'eval
  )
import Z.Z.X.Index (Edit)
import Z.Z.X.Method
  ( x'effCall
  )

---------------------------------------------------------------------

newtype R'Whr t = R'Whr
  { createGame :: t /\ t /\ Int /\ Int -> Effect Unit
  , iterate :: Int -> Effect Unit
  , ratingsForPlayer :: t -> Effect Whr'Ratings
  }

derive instance Newtype (R'Whr t) _

instance X'Readable (R'Whr t) (Edit Whr'Opts /\ (t -> String)) where
  x'readable'mk (optsEdit /\ toKey) = do
    st <- eff'whr'new optsEdit
    pure $ R'Whr
      { createGame: \(pw /\ pl /\ timestep /\ handicap) ->
          eff'whr'createGame (toKey pw) (toKey pl) timestep handicap st
      , iterate: \count -> eff'whr'iterate count st
      , ratingsForPlayer: \p ->
          eff'whr'ratingsForPlayer (toKey p) st
      }

type X'Whr t = Reader (R'Whr t)

---------------------------------------------------------------------

type P'whr = "_'x'whr"

type T'use'whr'AsSym p f = T'useAsSym P'whr p f

--------------------------------------------

type T'whr'eval t p =
  forall x' x a
   . ConsSymbol p (X'Whr t) x' x
  => (t -> String)
  -> Edit Whr'Opts
  -> Run x a
  -> Run x' a

whr'eval'' :: forall @p @t. T'whr'eval t p
whr'eval'' toKey optsEdit = x'eval @p $ optsEdit /\ toKey

whr'eval :: forall @t p. T'use'whr'AsSym p (T'whr'eval t)
whr'eval = whr'eval'' @p @t

--------------------------------------------

type T'whr'createGame p =
  forall t x' x
   . ConsSymbol p (X'Whr t) x' x
  => (t -> t -> Int -> Int -> Run x Unit)

whr'createGame'' :: forall @p. T'whr'createGame p
whr'createGame'' winner loser timestep handicap =
  x'effCall @p @"createGame"
    (winner /\ loser /\ timestep /\ handicap)

whr'createGame :: forall p. T'use'whr'AsSym p T'whr'createGame
whr'createGame = whr'createGame'' @p

--------------------------------------------

type T'whr'iterate p =
  forall t x' x. ConsSymbol p (X'Whr t) x' x => Int -> Run x Unit

whr'iterate'' :: forall @p. T'whr'iterate p
whr'iterate'' = x'effCall @p @"iterate"

whr'iterate :: forall p. T'use'whr'AsSym p T'whr'iterate
whr'iterate = whr'iterate'' @p

--------------------------------------------

type T'whr'ratingsForPlayer p =
  forall t x' x. ConsSymbol p (X'Whr t) x' x => t -> Run x Whr'Ratings

whr'ratingsForPlayer'' :: forall @p. T'whr'ratingsForPlayer p
whr'ratingsForPlayer'' = x'effCall @p @"ratingsForPlayer"

whr'ratingsForPlayer :: forall p. T'use'whr'AsSym p T'whr'ratingsForPlayer
whr'ratingsForPlayer = whr'ratingsForPlayer'' @p