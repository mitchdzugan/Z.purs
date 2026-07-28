module Test.Web.HelloXDOM where

import Web.Z.Prelude hiding (div)

import Web.Z.XDOM.Preact as XDOM
import Z.XDOM.Preact

hello :: forall x. x ##> Unit
hello = do
  root <- xGetElementById "root"
  xOut { root }
  pure unit

main :: Effect Unit
main = xExecAndExit do
  hello
  domEl <- xGetElementById "root" <#> jOrE (jsError "element not found" "#root")
    >>= xOk
  flip XDOM.renderIn domEl $ xRenderStrict do
    withEnv {} do
      withReducer @"red" 1 (\_ -> id) do
        div do
          cn_ "xDDD"
          withState 23 \state setState -> do
            withEnv { setState } do
              r <- xAsk
              xOut { state }
              text "div with stuff"
              div do
                button do
                  cn \c -> c "btn"
                    *> when (state > 25) do c "btn-accent"
                    *> c "btn-outline"
                  onClick $ (\_ -> r.setState $ state + 1)
                  text $ "Count: " <> show state
