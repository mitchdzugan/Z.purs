module Sys.Node
  ( lookupEnv
  , mkdir
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

readTextFile :: forall x. String -> Z.X (Z.EA Z.JsError x) String
readTextFile = Z.effectPromiseX <<< js_readTextFile

mkdir :: forall x. String -> Z.X' (Z.EA Z.JsError x)
mkdir = Z.effectPromiseX <<< js_mkdir

mkdirp :: forall x. String -> Z.X' (Z.EA Z.JsError x)
mkdirp = Z.effectPromiseX <<< js_mkdirp

writeTextFile :: forall x. String -> String -> Z.X' (Z.EA Z.JsError x)
writeTextFile p = Z.effectPromiseX <<< js_writeTextFile p

foreign import js_lookupEnv
  :: (String -> Z.Maybe String)
  -> Z.Maybe String
  -> String
  -> Z.Effect (Z.Maybe String)

lookupEnv :: String -> Z.Effect (Z.Maybe String)
lookupEnv = js_lookupEnv Z.Just Z.Nothing