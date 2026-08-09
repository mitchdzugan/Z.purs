module Z.SSBM.Slp.DB.App where

import Z.XDom.Prelude

import Routing.Duplex as Dup
import Routing.Duplex.Generic as G
import Z.XDom.Reducer as Rdc
import Z.XDom.Router as Rt

data CountAction = Inc | Dec | Reset

countAct :: Int -> CountAction -> Int
countAct n Inc = n + 1
countAct n Dec = n - 1
countAct _ Reset = 3

xOnE
  :: forall x
   . String
  -> XDom (count :: Reducer CountAction Int | x)
xOnE message = do
  ddd.div do
    ddd.text message
    ddd.button do
      da.cn "btn btn-soft"
      da.onClick $ \_ -> z @(XDomDispatch @@ "count") Reset
      ddd.text "reset"

xApp :: forall x. XurlStProviderX x -> XDom x
xApp = z @XDomRunRouter route
  (\{ url } -> Just $ urlToString url)
  do
    -- - x @(DomX "routeOrE")
    r <- z @XDomRouteOrE
    xOut $ show r
    ddd.div do
      da.cn "flex flex-col gap-4"
      ddd.div $ ddd.a do
        da.cn "btn link btn-primary"
        z @XDomRouteHref (Profile "Jimmy")
        ddd.text "profile link"
      ddd.div $ z @(XDomRunReducer @@ "count") 3 countAct $ z @XDomBindE xOnE do
        count <- z @(XDomGetState @@ "count")
        ddd.div do
          da.key "asdfasdfasdfasdf..."
          when (count < 0) do x' @"fail" "negative number invalid"
          ddd.a do
            da.href "/#23145"
            ddd.text "link"
          ddd.span %%-& \t -> t "div with stuff" *> t unit *> t "!"
          ddd.button do
            da.cnW \w -> do
              w "btn btn-soft" *> when (count > 5) do w "btn-accent"
            da.onClick $ (\_ -> z @(XDomDispatch @@ "count") Dec)
            ddd.text "dec"
          ddd.div %% "Count:" <-> count
          "asdfasdf" <!& ddd.button do
            da.cnW \w -> do
              w "btn btn-soft" *> when (count > 5) do w "btn-accent"
            da.onClick $ (\_ -> z @(XDomDispatch @@ "count") Inc)
            ddd.text "inc"

data Route = Home | Profile String

derive instance Generic Route _
instance Show Route where
  show = genericShow

route :: RouteDuplex' Route
route = Dup.root $ G.sum
  { "Home": G.noArgs
  , "Profile": Dup.path "profile" (Dup.string Dup.segment)
  }
