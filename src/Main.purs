module Main where

import Prelude
import Debug as Debug
import Z.Z as Z
import Z.Node.Sys.Index as Sys

main :: Z.Effect Unit
main = Z.launchAff_ $ Z.runBaseAff do
  txtRes <- Z.runExcept $ Sys.readTextFile "/home/dz/Repo/PS-WS/index.js"
  Debug.traceM { txtRes }
