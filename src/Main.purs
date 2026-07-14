module Main where

import Prelude
import Debug as Debug
import Z as Z
import Sys.Node as Sys

main :: Z.Effect Unit
main = Z.launchAff_ $ Z.runBaseAff do
  txtRes <- Z.runExcept $ Sys.readTextFile "/home/dz/Repo/PS-WS/Cargo.toml"
  Debug.traceM { txtRes }
