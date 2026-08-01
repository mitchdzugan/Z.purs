module Z.SSBM.Slp.DB.App where

import Z.XDOM.Prelude

appImpl :: forall x. XDom (red :: XDomStateSetter Int | x)
appImpl = do
  xDBoundError (\e -> text $ "Error thrown" <> e) do
    red <- xAt @"red" AtR
    div do
      pkey "asdf"
      text $ "div with stuff!!!"
      when (red.get > 7) do xFail "NUMBER TOO BIG"
      xDKeyed "asdfasdf" $ div do
        button do
          cn \c -> c "btn"
            *> when (red.get > 25) do c "btn-accent"
            *> c "btn-outline"
          onClick $ (\_ -> red.set $ red.get + 2)
          xDOnMount $ xOut { i: "btn-mount", c: red.get }
          text $ "Count: " <> show red.get

app :: forall x. XDom x
app = do
  let initialState = 1
  xDRespondWithNewStateSetterAt @"red" initialState appImpl
