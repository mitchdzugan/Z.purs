module Sys.Node
  ( mkdir
  , mkdirp
  , readTextFile
  , writeTextFile
  ) where

import Prelude

import Z as Z

foreign import js_readTextFile :: String -> Z.Effect (Z.Promise String)
foreign import js_mkdir :: String -> Z.Effect (Z.Promise Unit)
foreign import js_mkdirp :: String -> Z.Effect (Z.Promise Unit)
foreign import js_writeTextFile :: String -> String -> Z.Effect (Z.Promise Unit)

readTextFile :: forall x. String -> Z.Xea x Z.JsError String
readTextFile = Z.effectPromiseX <<< js_readTextFile

mkdir :: forall x. String -> Z.Xea x Z.JsError Unit
mkdir = Z.effectPromiseX <<< js_mkdir

mkdirp :: forall x. String -> Z.Xea x Z.JsError Unit
mkdirp = Z.effectPromiseX <<< js_mkdirp

writeTextFile :: forall x. String -> String -> Z.Xea x Z.JsError Unit
writeTextFile p = Z.effectPromiseX <<< js_writeTextFile p