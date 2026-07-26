module Z.Sys.Node.Impl
  ( (~)
  , EnvPaths
  , Path
  , Platform(..)
  , XNode
  , XNodeF
  , argv
  , basename
  , class Pathlike
  , decodeTextFile
  , dirname
  , encodeTextFile
  , encodeTextFileP
  , envCfg
  , envData
  , envPaths
  , envTmp
  , join
  , lookupEnv
  , mkdir
  , mkdirP
  , pathStr
  , platform
  , readFile
  , readTextFile
  , wd
  , writeTextFile
  , writeTextFileP
  , xExecAndExit
  , xLookupEnv
  ) where

import Prelude hiding (join)

import Z as Z
import Z.Sys.Module as Sys
import Z.Z.Shorthand (type (+), type (#>))
import Effect.Unsafe as Unsafe

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

instance showPath :: Show Path where
  show (Path s) = s

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

type XNodeEA e x = Z.EA e (XNODE x)

xExecAndExit
  :: forall @w @e a. Z.XWa w (XNodeEA e) a -> Z.Effect Unit
xExecAndExit m = execAndExit $ Z.xExecAff $ do
  w Z./\ res <- Z.xListen $ Z.expand $ runXNode m
  when (Z.arrSize w > 0) do
    Z.xLogWarning "⌄ unhandled warnings ⌄"
    Z.xLogWarning w
  pure res

data Platform = Win32 | Darwin | Linux | Android | FreeBSD | OpenBSD | Unknown

toPlatform :: String -> Platform
toPlatform "win32" = Win32
toPlatform "darwin" = Darwin
toPlatform "linux" = Linux
toPlatform "android" = Android
toPlatform "freebsd" = FreeBSD
toPlatform "openbsd" = OpenBSD
toPlatform _ = Unknown

argv :: forall x. XNode x (Array String)
argv = Z.lift _xNode (FullArgvCmd (Z.arrDrop 2))

wd :: forall x. XNode x Path
wd = Z.lift _xNode (WdCmd Path)

envPaths :: forall x. String -> Z.Maybe String -> XNode x EnvPaths
envPaths appName suffix = Z.lift _xNode (EnvPathsCmd appName suffix Z.id)

platform :: forall x. XNode x Platform
platform = Z.lift _xNode (PlatformCmd toPlatform)

envData :: EnvPaths -> Path
envData = Path <<< js_envData

envCfg :: EnvPaths -> Path
envCfg = Path <<< js_envCfg

envTmp :: EnvPaths -> Path
envTmp = Path <<< js_envTmp

foreign import js_platform :: Z.Effect String

foreign import js_envCfg :: EnvPaths -> String

foreign import js_envData :: EnvPaths -> String

foreign import js_envTmp :: EnvPaths -> String

foreign import js_exit :: Int -> Z.Effect Unit

foreign import js_errorLog :: forall a. a -> Z.Effect Unit

foreign import js_pathDirname :: String -> String

foreign import js_pathBasename :: String -> String

foreign import js_pathJoin :: String -> String -> String

foreign import js_wd :: Z.Effect String

foreign import js_argv :: Z.Effect (Array String)

foreign import data EnvPaths :: Type

foreign import js_envPaths :: String -> Z.Json -> Z.Effect EnvPaths

data XNodeF a
  = WdCmd (String -> a)
  | FullArgvCmd (Array String -> a)
  | PlatformCmd (String -> a)
  | EnvPathsCmd String (Z.Maybe String)
      ( EnvPaths
        -> a
      )

handleXNode :: forall r. XNodeF ~> Z.Run r
handleXNode = case _ of
  WdCmd f -> pure $ f (Unsafe.unsafePerformEffect js_wd)
  FullArgvCmd f -> pure $ f (Unsafe.unsafePerformEffect js_argv)
  PlatformCmd f -> pure $ f (Unsafe.unsafePerformEffect js_platform)
  EnvPathsCmd appName suffix f -> pure $ f $ Unsafe.unsafePerformEffect $
    js_envPaths appName (Z.jsonRmNils $ Z.encodeJson { suffix })

derive instance functorXBaseF :: Functor XNodeF

type XNODE x = (xNode :: XNodeF | x)

_xNode = Z.Proxy :: Z.Proxy "xNode"

runXNode :: forall r. Z.Run (XNODE + r) ~> Z.Run r
runXNode = Z.run (Z.on _xNode handleXNode Z.send)

dirname :: forall p. Pathlike p => p -> Path
dirname p = Path $ js_pathDirname $ pathStr p

basename :: forall p. Pathlike p => p -> Path
basename p = Path $ js_pathBasename $ pathStr p

join :: forall p1 p2. Pathlike p1 => Pathlike p2 => p1 -> p2 -> Path
join p1 p2 = Path $ js_pathJoin (pathStr p1) (pathStr p2)

infixr 0 join as ~

type XNode x a = Z.X (xNode :: XNodeF | x) a