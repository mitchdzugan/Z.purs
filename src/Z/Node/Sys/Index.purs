module Z.Node.Sys.Index
  ( lookupEnv
  , mkdir
  , mkdirp
  , readTextFile
  , writeTextFile
  , xExecAndExit
  , xLookupEnv
  ) where

import Prelude

import Z.Z as Z

foreign import js_readTextFile
  :: String -> Z.Effect Z.$ Z.Promise String

foreign import js_mkdir
  :: String -> Z.Effect Z.$ Z.Promise Unit

foreign import js_mkdirp
  :: String -> Z.Effect Z.$ Z.Promise Unit

foreign import js_writeTextFile
  :: String -> String -> Z.Effect Z.$ Z.Promise Unit

readTextFile :: forall x. String -> Z.EA Z.JsError x Z.@> String
readTextFile = Z.effectPromiseX <<< js_readTextFile

mkdir :: forall x. String -> Z.EA Z.JsError x Z.@> Unit
mkdir = Z.effectPromiseX <<< js_mkdir

mkdirp :: forall x. String -> Z.EA Z.JsError x Z.@> Unit
mkdirp = Z.effectPromiseX <<< js_mkdirp

writeTextFile :: forall x. String -> String -> Z.EA Z.JsError x Z.@> Unit
writeTextFile p = Z.effectPromiseX <<< js_writeTextFile p

foreign import js_lookupEnv
  :: (String -> Z.Maybe String)
  -> Z.Maybe String
  -> String
  -> Z.Effect Z.$ Z.Maybe String

lookupEnv :: String -> Z.Effect Z.$ Z.Maybe String
lookupEnv = js_lookupEnv Z.Just Z.Nothing

xLookupEnv :: forall x. String -> Z.A x Z.@> Z.Maybe String
xLookupEnv k = lookupEnv k # Z.tryEff # Z.xTry <#> getRes
  where
  getRes (Z.Right (Z.Just v)) = Z.Just v
  getRes _ = Z.Nothing

execAndExit :: forall e a. Z.Aff (Z.Either e a) -> Z.Effect Unit
execAndExit a = Z.runAff_ onDone a
  where
  onDone (Z.Left e) = do
    js_errorLog "UNHANDLED ERROR"
    js_errorLog e
    js_exit 125
  onDone (Z.Right (Z.Left e)) = do
    js_errorLog e
    js_exit 1
  onDone _ = pure unit

xExecAndExit :: forall e a. a Z.<@ Z.EA e () -> Z.Effect Unit
xExecAndExit = Z.xExecAff >>> execAndExit

foreign import js_exit :: Int -> Z.Effect Unit
foreign import js_errorLog :: forall a. a -> Z.Effect Unit