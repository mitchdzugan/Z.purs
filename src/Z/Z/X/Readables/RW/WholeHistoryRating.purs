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
  ( Eff'Whr
  , Whr'Opts
  , Whr'Ratings
  , eff'whr'createGame
  , eff'whr'iterate
  , eff'whr'new
  , eff'whr'ratingsForPlayer
  )
import Z.Z.X.Core
  ( class X'R'RespondsTo'rw
  , class X'Readable'rw
  , RW'Tagged
  , x'eval
  , x'respondTo
  )
import Z.Z.X.Index (Edit)
import Z.Z.X.Responds
  ( Responds
  , responds'run'eff
  )

---------------------------------------------------------------------

newtype R'Whr t = R'Whr (Eff'Whr /\ (t -> String))

instance X'Readable'rw (R'Whr t) (Edit Whr'Opts /\ (t -> String)) where
  x'readable'rw'mk (optsEdit /\ toKey) = do
    st <- eff'whr'new optsEdit
    pure $ R'Whr $ st /\ toKey

instance
  X'R'RespondsTo'rw (R'Whr t)
    ( VariantF ()
    )
    ( VariantF
        ( whr'createGame :: Responds (t /\ t /\ Int /\ Int) Unit
        , whr'iterate :: Responds Int Unit
        , whr'ratingsForPlayer :: Responds t Whr'Ratings
        )
    )
    ( self :: t
    ) where
  x'r'mkResponds'rw'types = Proxy
  x'r'mkResponds'r _ = match {}
  x'r'mkResponds'w (R'Whr (whr /\ toKey)) = match
    { whr'createGame: responds'run'eff \(pw /\ pl /\ timestep /\ handicap) ->
        eff'whr'createGame (toKey pw) (toKey pl) timestep handicap whr
    , whr'iterate: responds'run'eff \count -> eff'whr'iterate count whr
    , whr'ratingsForPlayer: responds'run'eff \p ->
        eff'whr'ratingsForPlayer (toKey p) whr
    }

type X'Whr t = Reader (RW'Tagged (R'Whr t))

---------------------------------------------------------------------

type T'use'whr'AsSym p f = T'useAsSym "_'x'whr" p f

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
  x'respondTo @p @"whr'createGame" $ winner /\ loser /\ timestep /\ handicap

whr'createGame :: forall p. T'use'whr'AsSym p T'whr'createGame
whr'createGame = whr'createGame'' @p

--------------------------------------------

type T'whr'iterate p =
  forall t x' x. ConsSymbol p (X'Whr t) x' x => Int -> Run x Unit

whr'iterate'' :: forall @p. T'whr'iterate p
whr'iterate'' = x'respondTo @p @"whr'iterate"

whr'iterate :: forall p. T'use'whr'AsSym p T'whr'iterate
whr'iterate = whr'iterate'' @p

--------------------------------------------

type T'whr'ratingsForPlayer p =
  forall t x' x. ConsSymbol p (X'Whr t) x' x => t -> Run x Whr'Ratings

whr'ratingsForPlayer'' :: forall @p. T'whr'ratingsForPlayer p
whr'ratingsForPlayer'' = x'respondTo @p @"whr'ratingsForPlayer"

whr'ratingsForPlayer :: forall p. T'use'whr'AsSym p T'whr'ratingsForPlayer
whr'ratingsForPlayer = whr'ratingsForPlayer'' @p