module Z.SSBM.Slp.DB.App where

import Z.XDom.Prelude
import Z.XDom.Reducer as Rdc

data CountAction = Inc | Dec | Reset

aCount :: Int -> CountAction -> Int
aCount n Inc = n + 1
aCount n Dec = n - 1
aCount _ Reset = 3

xOnE :: forall x. String -> XDom (count :: (Rdc.R CountAction Int) | x)
xOnE message = do
  d.div do
    d.text message
    d.button do
      da.cn "btn btn-soft"
      da.onClick $ \_ -> xAt @"count" Rdc.Dispatch Reset
      d.text "reset"

xAppImpl :: forall x. XDom (E String (count :: (Rdc.R CountAction Int) | x))
xAppImpl = do
  count <- xAt @"count" Rdc.Get
  d.div do
    da.key "asdfasdfasdfasdf..."
    when (count < 0) do x Fail "negative number invalid"
    d.a do
      da.href "#23145"
      d.text "link"
    d.span %%-& \t -> t "div with stuff" *> t unit *> t "!"
    d.button do
      da.cnW \w -> w "btn btn-outline" *> when (count > 5) do w "btn-accent"
      da.onClick $ (\_ -> xAt @"count" Rdc.Dispatch Dec)
      d.text "dec"
    d.div %% "Count:" <-> count
    "asdfasdf" <!& d.button do
      da.cnW \w -> w "btn btn-outline" *> when (count > 5) do w "btn-accent"
      da.onClick $ (\_ -> xAt @"count" Rdc.Dispatch Inc)
      d.text ("inc" <-> "by" <-> 1)

xApp :: forall x. XDom x
xApp = xAt @"count" Rdc.Run 3 aCount $ x DomBindE xOnE $ xAppImpl
