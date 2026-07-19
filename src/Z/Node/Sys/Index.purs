module Z.Node.Sys.Index
  ( Path
  , basename
  , class Pathlike
  , dirname
  , join
  , lookupEnv
  , mkdir
  , mkdirp
  , pathStr
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

newtype Path = Path String

class Pathlike a where
  pathStr :: a -> String

instance pathlikePath :: Pathlike Path where
  pathStr (Path p) = p

instance pathlikeString :: Pathlike String where
  pathStr s = s

readTextFile :: forall x p. Pathlike p => p -> x Z.# Z.EA Z.JsError Z.@> String
readTextFile = Z.effectPromiseX <<< js_readTextFile <<< pathStr

mkdir :: forall x p. Pathlike p => p -> x Z.# Z.EA Z.JsError Z.@> Unit
mkdir = Z.effectPromiseX <<< js_mkdir <<< pathStr

mkdirp :: forall x p. Pathlike p => p -> x Z.# Z.EA Z.JsError Z.@> Unit
mkdirp = Z.effectPromiseX <<< js_mkdirp <<< pathStr

writeTextFile
  :: forall x p. Pathlike p => p -> String -> x Z.# Z.EA Z.JsError Z.@> Unit
writeTextFile p = Z.effectPromiseX <<< js_writeTextFile (pathStr p)

foreign import js_lookupEnv
  :: (String -> Z.Maybe String)
  -> Z.Maybe String
  -> String
  -> Z.Effect Z.$ Z.Maybe String

lookupEnv :: String -> Z.Effect Z.$ Z.Maybe String
lookupEnv = js_lookupEnv Z.Just Z.Nothing

xLookupEnv :: forall x. String -> x Z.# Z.A Z.@> Z.Maybe String
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

xExecAndExit :: forall e a. a Z.<@ Z.EA e Z.$ () -> Z.Effect Unit
xExecAndExit = Z.xExecAff >>> execAndExit

foreign import js_exit :: Int -> Z.Effect Unit
foreign import js_errorLog :: forall a. a -> Z.Effect Unit

foreign import js_pathDirname :: String -> String

foreign import js_pathBasename :: String -> String

foreign import js_pathJoin :: String -> String -> String

dirname :: forall p. Pathlike p => p -> Path
dirname p = Path $ js_pathDirname $ pathStr p

basename :: forall p. Pathlike p => p -> Path
basename p = Path $ js_pathBasename $ pathStr p

join :: forall p1 p2. Pathlike p1 => Pathlike p2 => p1 -> p2 -> Path
join p1 p2 = Path $ js_pathJoin (pathStr p1) (pathStr p2)