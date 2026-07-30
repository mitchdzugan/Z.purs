module Z.SSBM.Slp.DB.App where

import Z.XDOM.Prelude

appImpl :: forall x. XDom (red :: XDReducer Int Int | x)
appImpl = do
  red <- xAskAt @"red"
  div do
    text "div with stuff!!!"
    div do
      button do
        cn \c -> c "btn"
          *> when (red.get > 25) do c "btn-accent"
          *> c "btn-outline"
        onClick $ (\_ -> red.act $ red.get + 2)
        text $ "Count: " <> show red.get

app :: forall x. XDom x
app = xDRespondWithReducerAt @"red" 1 (\_ -> id) appImpl
