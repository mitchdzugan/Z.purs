module Z.SSBM.Slp.DB.App where

import Z.XDOM.Prelude

xAppImpl :: forall x. XDom (red :: XDomStateSetter Int | x)
xAppImpl = do
  x DomBindError (\e -> xText $ "Error thrown" <> e) do
    red <- xAt @"red" Ask
    xDiv do
      xKey "asdf"
      xText $ "div with stuff!!!"
      xDomKeyed "asdfasdf" $ xDiv $ xButton do
        xCnX \c -> c "btn"
          *> when (red.get > 25) do c "btn-accent"
          *> c "btn-outline"
        xOnClick $ (\_ -> red.set $ red.get + 2)
        xOnMount $ xOut { i: "btn-mount", c: red.get }
        xText $ "Count: " <> show red.get

xApp :: forall x. XDom x
xApp = do
  let initialState = 1
  xAt @"red" DomRunRWithNewStateSetter initialState xAppImpl
