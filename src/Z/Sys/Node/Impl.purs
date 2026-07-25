module Z.Sys.Node.Impl
  ( Path
  , basename
  , class Pathlike
  , decodeTextFile
  , dirname
  , encodeTextFile
  , encodeTextFileP
  , join
  , lookupEnv
  , mkdir
  , mkdirP
  , pathStr
  , readFile
  , readTextFile
  , writeTextFile
  , writeTextFileP
  , xExecAndExit
  , xLookupEnv
  ) where

import Prelude

import Run as R
import Z as Z
import Z.Sys.Module as Sys
import Z.Z.Shorthand (type (+), type (#>))

foreign import js_readTextFile
  :: String -> Z.Effect Z.$ Z.Promise String

foreign import js_readFile
  :: String -> Z.Effect Z.$ Z.Promise Z.Buffer

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

readFile :: forall x p. Pathlike p => p -> Z.EA Z.JsError x #> Z.Buffer
readFile = Z.xEffectPromise <<< js_readFile <<< pathStr

readTextFile :: forall x p. Pathlike p => p -> Z.EA Z.JsError x #> String
readTextFile = Z.xEffectPromise <<< js_readTextFile <<< pathStr

decodeTextFile
  :: forall x p @d
   . Pathlike p
  => Z.DecodeJson d
  => p
  -> Z.EA Sys.FSDataError x #> d
decodeTextFile p = do
  contents <- Z.xMapE Sys.ReadError $ readTextFile p
  Z.xOk $ Z.mapL Sys.DecodeError $ Z.decode contents

mkdir :: forall x p. Pathlike p => p -> Z.EA Z.JsError x #> Unit
mkdir = Z.xEffectPromise <<< js_mkdir <<< pathStr

mkdirP :: forall x p. Pathlike p => p -> Z.EA Z.JsError x #> Unit
mkdirP = Z.xEffectPromise <<< js_mkdirp <<< pathStr

writeTextFile
  :: forall x p. Pathlike p => p -> String -> Z.EA Z.JsError x #> Unit
writeTextFile p = Z.xEffectPromise <<< js_writeTextFile (pathStr p)

writeTextFileP
  :: forall x p. Pathlike p => p -> String -> Z.EA Z.JsError x #> Unit
writeTextFileP p s = do
  mkdirP $ dirname p
  writeTextFile p s

encodeTextFile
  :: forall x p d
   . Pathlike p
  => Z.EncodeJson d
  => p
  -> d
  -> Z.EA Z.JsError x #> Unit
encodeTextFile p d = writeTextFile p $ Z.encode d

encodeTextFileP
  :: forall x p d
   . Pathlike p
  => Z.EncodeJson d
  => p
  -> d
  -> Z.EA Z.JsError x #> Unit
encodeTextFileP p d = writeTextFileP p $ Z.encode d

foreign import js_lookupEnv
  :: (String -> Z.Maybe String)
  -> Z.Maybe String
  -> String
  -> Z.Effect (Z.Maybe String)

lookupEnv :: String -> Z.Effect Z.$ Z.Maybe String
lookupEnv = js_lookupEnv Z.Just Z.Nothing

xLookupEnv :: forall x. String -> Z.A x #> Z.Maybe String
xLookupEnv k = lookupEnv k # Z.xAEff # Z.xTry <#> getRes
  where
  getRes (Z.Right (Z.Just v)) = Z.Just v
  getRes _ = Z.Nothing

execAndExit :: forall e a. Z.Aff (Z.Either e a) -> Z.Effect Unit
execAndExit a = Z.runAff_ onDone a
  where
  onDone (Z.Left e) = do
    js_errorLog "⌄ UNHANDLED error !!! ⌄"
    js_errorLog e
    js_exit 125
  onDone (Z.Right (Z.Left e)) = do
    js_errorLog "⌄ error ⌄"
    js_errorLog e
    js_exit 1
  onDone _ = pure unit

xExecAndExit
  :: forall @w @e a. Z.XWa w (Z.EA e) a -> Z.Effect Unit
xExecAndExit m = execAndExit $ Z.xExecAff $ do
  w Z./\ res <- Z.xListen @(Array w) (R.expand m)
  when (Z.arrSize w > 0) do
    Z.xLogWarning "⌄ unhandled warnings ⌄"
    Z.xLogWarning w
  pure res

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
