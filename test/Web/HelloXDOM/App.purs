module Test.Web.HelloXDOM.App where

import Z.XDOM

app :: forall x. XComp x Unit
app = do
  withEnv {} do
    withReducer @"red" 21 (\_ -> id) do
      red <- askForReducer @"red"
      div do
        text "div with stuff"
        div do
          button do
            cn \c -> c "btn"
              *> when (red.get > 25) do c "btn-accent"
              *> c "btn-outline"
            onClick $ (\_ -> red.act $ red.get + 1)
            text $ "Count: " <> show red.get