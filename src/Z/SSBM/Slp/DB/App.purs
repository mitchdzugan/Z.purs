module Z.SSBM.Slp.DB.App where

import Z.XDom.Prelude

import Routing.Duplex as Dup
import Routing.Duplex.Generic as G
import Z.XDom.Reducer as Rdc
import Z.XDom.UrlState as UrlSt

data CountAction = Inc | Dec | Reset

countAct :: Int -> CountAction -> Int
countAct n Inc = n + 1
countAct n Dec = n - 1
countAct _ Reset = 3

xOnE :: forall x. String -> XDom (count :: (Rdc.R CountAction Int) | x)
xOnE message = do
  d.div do
    d.text message
    d.button do
      da.cn "btn btn-soft"
      da.onClick $ \_ -> xdom @"count" @"dispatch" Reset
      d.text "reset"

xApp :: forall x. UrlSt.XProvider x -> XDom x
xApp = xdom' @"Router.run" route (\{ url } -> Just $ urlToString url) do
  r <- xdom' @"Router.routeOrE"
  xOut $ show r
  d.div do
    da.cn "flex flex-col gap-4"
    d.div $ d.a do
      da.cn "btn link btn-primary"
      xdom' @"Router.href" $ Profile "Jimmy"
      d.text "profile link"
    d.div $ xdom @"count" @"Reducer.run" 3 countAct $ xdom' @"bindE" xOnE do
      count <- xdom @"count" @"get"
      d.div do
        da.key "asdfasdfasdfasdf..."
        when (count < 0) do x' @"fail" "negative number invalid"
        d.a do
          da.href "/#23145"
          d.text "link"
        d.span %%-& \t -> t "div with stuff" *> t unit *> t "!"
        d.button do
          da.cnW \w -> do w "btn btn-soft" *> when (count > 5) do w "btn-accent"
          da.onClick $ (\_ -> xdom @"count" @"dispatch" Dec)
          d.text "dec"
        d.div %% "Count:" <-> count
        "asdfasdf" <!& d.button do
          da.cnW \w -> do w "btn btn-soft" *> when (count > 5) do w "btn-accent"
          da.onClick $ (\_ -> xdom @"count" @"dispatch" Inc)
          d.text "inc"

data Route = Home | Profile String

derive instance genericRoute :: Generic Route _
instance showRoute :: Show Route where
  show = genericShow

route :: RouteDuplex' Route
route = Dup.root $ G.sum
  { "Home": G.noArgs
  , "Profile": Dup.path "profile" (Dup.string Dup.segment)
  }
