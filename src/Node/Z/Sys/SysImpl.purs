module Node.Z.Sys.SysImpl
  ( (/./)
  , (/.|//)
  , EnvPaths
  , Path
  , Platform(..)
  , X'Node
  , XNodeF
  , basename
  , class Pathlike
  , dirname
  , envCfg
  , envData
  , envTmp
  , pathJoin
  , pathJoinAbs
  , pathStr
  , runXAThenExit
  , runXAThenExitWithArgv
  , xArgParse
  , xArgv
  , xDecodeAnyYamlExt
  , xDecodeTextFile
  , xDecodeYamlFile
  , xEncodeTextFile
  , xEncodeTextFileP
  , xEnvPaths
  , xLookupEnv
  , xMkdir
  , xMkdirP
  , xPlatform
  , xReadFile
  , xReadTextFile
  , xWd
  , xWriteTextFile
  , xWriteTextFileP
  ) where

import Z.Prelude

import Effect.Unsafe as Unsafe
import Z.Sys.Module as Sys
import Z.Z.Opt as O

foreign import js_readTextFile
  :: String -> Effect $ Promise String

foreign import js_readFile
  :: String -> Effect $ Promise Buffer

foreign import js_mkdir
  :: String -> Effect $ Promise Unit

foreign import js_mkdirp
  :: String -> Effect $ Promise Unit

foreign import js_writeTextFile
  :: String -> String -> Effect $ Promise Unit

foreign import js_loadYaml
  :: String
  -> (JsError -> Either JsError Json)
  -> (Json -> Either JsError Json)
  -> Either JsError Json

newtype Path = Path String

instance Show Path where
  show (Path s) = s

class Pathlike a where
  pathStr :: a -> String

instance Pathlike Path where
  pathStr (Path p) = p

instance Pathlike String where
  pathStr s = s

xReadFile :: forall x p. Pathlike p => p -> EA' JsError x @@> Buffer
xReadFile = e'runEffPromise <<< js_readFile <<< pathStr

xReadTextFile :: forall x p. Pathlike p => p -> EA' JsError x @@> String
xReadTextFile = e'runEffPromise <<< js_readTextFile <<< pathStr

xDecodeTextFile
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA' Sys.FSDataError x @@> d
xDecodeTextFile p = do
  contents <- e'map Sys.ReadError $ xReadTextFile p
  e'ok $ mapL Sys.DecodeError $ decode contents

xDecodeYamlString
  :: forall x @d
   . DecodeJson d
  => String
  -> EA' Sys.FSDataError x @@> d
xDecodeYamlString contents = do
  json <- e'ok $ mapL Sys.ReadError $ js_loadYaml contents Left Right
  e'ok $ mapL Sys.DecodeError $ decodeJson json

xDecodeYamlFile
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA' Sys.FSDataError x @@> d
xDecodeYamlFile p = do
  contents <- e'map Sys.ReadError $ xReadTextFile p
  xDecodeYamlString contents

xDecodeAnyYamlExt
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA' Sys.FSDataError x @@> d
xDecodeAnyYamlExt p = do
  contents <- e'tryUntil
    (e'map Sys.ReadError $ xReadTextFile $ (pathStr p) <> ".yaml")
    [ const $ e'map Sys.ReadError $ xReadTextFile $ (pathStr p) <>
        ".json"
    , const $ e'map Sys.ReadError $ xReadTextFile $ p
    ]
  xDecodeYamlString contents

xMkdir :: forall x p. Pathlike p => p -> EA' JsError x @@> Unit
xMkdir = e'runEffPromise <<< js_mkdir <<< pathStr

xMkdirP :: forall x p. Pathlike p => p -> EA' JsError x @@> Unit
xMkdirP = e'runEffPromise <<< js_mkdirp <<< pathStr

xWriteTextFile
  :: forall x p. Pathlike p => p -> String -> EA' JsError x @@> Unit
xWriteTextFile p = e'runEffPromise <<< js_writeTextFile (pathStr p)

xWriteTextFileP
  :: forall x p. Pathlike p => p -> String -> EA' JsError x @@> Unit
xWriteTextFileP p s = do
  xMkdirP $ dirname p
  xWriteTextFile p s

xEncodeTextFile
  :: forall x p d
   . Pathlike p
  => EncodeJson d
  => p
  -> d
  -> EA' JsError x @@> Unit
xEncodeTextFile p d = xWriteTextFile p $ encode d

xEncodeTextFileP
  :: forall x p d
   . Pathlike p
  => EncodeJson d
  => p
  -> d
  -> EA' JsError x @@> Unit
xEncodeTextFileP p d = xWriteTextFileP p $ encode d

foreign import js_lookupEnv
  :: (String -> Maybe String)
  -> Maybe String
  -> String
  -> Effect (Maybe String)

lookupEnv :: String -> Effect $ Maybe String
lookupEnv = js_lookupEnv Just Nothing

xLookupEnv :: forall x. String -> A' x @@> Maybe String
xLookupEnv k = e'try (e'runEffA $ lookupEnv k) <#> getRes
  where
  getRes (Right (Just v)) = Just v
  getRes _ = Nothing

effAffThenExit :: forall e a. RtError e => Aff (Either e a) -> Effect Unit
effAffThenExit a = runAff_ onDone a
  where
  onDone (Left e) = do
    js_errorLog "process failed with UNHANDLED UNKNOWN error ⌄"
    js_errorLog e
    js_exit 125
  onDone (Right (Left e)) = do
    js_errorLog
      $ "process failed with known error [| "
      <> rtErrName e
      <> " |] ⌄"
    js_errorLog $ rtErrMessage e
    js_exit 1
  onDone _ = pure unit

type XNodeEA e x = EA' e (X'Node x)

runXAThenExit
  :: forall @w @e a. RtError e => (WaEA' w e (X'Node ()) @@> a) -> Effect Unit
runXAThenExit m = effAffThenExit $ async'x $ do
  res /\ w <- w'run $ e'try $ runXNode m
  when (arr'size w > 0) do
    x'logWarning "collected warnings ⌄"
    x'logWarning w
  pure res

runXAThenExitWithArgv
  :: forall @w @e a
   . RtError e
  => (Array String -> WaEA' w e (X'Node ()) @@> a)
  -> Effect Unit
runXAThenExitWithArgv fm = runXAThenExit $ xArgv >>= fm

data Platform = Win32 | Darwin | Linux | Android | FreeBSD | OpenBSD | Unknown

derive instance Eq Platform

toPlatform :: String -> Platform
toPlatform "win32" = Win32
toPlatform "darwin" = Darwin
toPlatform "linux" = Linux
toPlatform "android" = Android
toPlatform "freebsd" = FreeBSD
toPlatform "openbsd" = OpenBSD
toPlatform _ = Unknown

xArgv :: forall x. X'Node x @@> Array String
xArgv = lift p'X'Node (FullArgvCmd (arr'drop 2))

xWd :: forall x. X'Node x @@> Path
xWd = lift p'X'Node (WdCmd Path)

xEnvPaths :: forall x. String -> Maybe String -> X'Node x @@> EnvPaths
xEnvPaths appName suffix = lift p'X'Node (EnvPathsCmd appName suffix id)

xPlatform :: forall x. X'Node x @@> Platform
xPlatform = lift p'X'Node (PlatformCmd toPlatform)

envData :: EnvPaths -> Path
envData = Path <<< js_envData

envCfg :: EnvPaths -> Path
envCfg = Path <<< js_envCfg

envTmp :: EnvPaths -> Path
envTmp = Path <<< js_envTmp

foreign import js_platform :: Effect String

foreign import js_envCfg :: EnvPaths -> String

foreign import js_envData :: EnvPaths -> String

foreign import js_envTmp :: EnvPaths -> String

foreign import js_exit :: Int -> Effect Unit

foreign import js_errorLog :: forall a. a -> Effect Unit

foreign import js_pathDirname :: String -> String

foreign import js_pathBasename :: String -> String

foreign import js_pathJoin :: String -> String -> String
foreign import js_pathJoinAbs :: String -> String -> String

foreign import js_wd :: Effect String

foreign import js_argv :: Effect (Array String)

foreign import data EnvPaths :: Type

foreign import js_envPaths :: String -> Json -> Effect EnvPaths

data XNodeF a
  = WdCmd (String -> a)
  | FullArgvCmd (Array String -> a)
  | PlatformCmd (String -> a)
  | EnvPathsCmd String (Maybe String) (EnvPaths -> a)

handleXNode :: forall r. XNodeF ~> Run r
handleXNode = case _ of
  WdCmd f -> pure $ f (Unsafe.unsafePerformEffect js_wd)
  FullArgvCmd f -> pure $ f (Unsafe.unsafePerformEffect js_argv)
  PlatformCmd f -> pure $ f (Unsafe.unsafePerformEffect js_platform)
  EnvPathsCmd appName suffix f -> pure $ f $ Unsafe.unsafePerformEffect $
    js_envPaths appName (encodeOpts { suffix })

derive instance Functor XNodeF

type X'Node x = (_'x'node :: XNodeF | x)

p'X'Node :: Proxy "_'x'node"
p'X'Node = Proxy @"_'x'node"

runXNode :: forall x a. Run (X'Node x) a -> Run x a
runXNode = run (on p'X'Node handleXNode send)

dirname :: forall p. Pathlike p => p -> Path
dirname p = Path $ js_pathDirname $ pathStr p

basename :: forall p. Pathlike p => p -> Path
basename p = Path $ js_pathBasename $ pathStr p

pathJoin :: forall p1 p2. Pathlike p1 => Pathlike p2 => p1 -> p2 -> Path
pathJoin p1 p2 = Path $ js_pathJoin (pathStr p1) (pathStr p2)

pathJoinAbs :: forall p1 p2. Pathlike p1 => Pathlike p2 => p1 -> p2 -> Path
pathJoinAbs p1 p2 = Path $ js_pathJoinAbs (pathStr p1) (pathStr p2)

infixr 0 pathJoin as /./

-- join paths unless rightside is absolute in which case, use rightside
infixr 0 pathJoinAbs as /.|//

xArgParse
  :: forall x a
   . String
  -> O.ParserInfo a
  -> Array String
  -> (a -> X'Node x @@> Unit)
  -> X'Node x @@> Unit
xArgParse progName opts args fm =
  handleParse $ O.execParserPure O.defaultPrefs opts args
  where
  handleParse (O.Success a) = fm a
  handleParse (O.Failure f) = do
    let msg /\ _exit = O.renderFailure f progName
    x'outErr msg
    pure unit
  handleParse _ = pure unit
