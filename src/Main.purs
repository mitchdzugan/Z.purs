module Main where

import Prelude

import Z.Node.Sys.Index as Sys
import Z.Z as Z

main :: Z.Effect Unit
main = Z.launchAff_ $ Z.runBaseAff do
  Z.logInfo "hi"
  txtRes <- Z.runExcept $ Sys.readTextFile "/home/dz/Repo/PS-WS/index.js"
  Z.logInfo { txtRes }
