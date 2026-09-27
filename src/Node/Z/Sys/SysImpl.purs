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
  , x'argParse
  , x'argv
  , x'decodeAnyYamlExt
  , x'decodeTextFile
  , x'decodeYamlFile
  , x'encodeTextFile
  , x'encodeTextFileP
  , x'envPaths
  , x'isDirectory
  , x'lookupEnv
  , x'mkdir
  , x'mkdirP
  , x'pid
  , x'platform
  , x'readFile
  , x'readTextFile
  , x'readdir
  , x'rimraf
  , x'symlink
  , x'wd
  , x'writeTextFile
  , x'writeTextFileP
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

foreign import js_rimraf
  :: String -> Effect $ Promise Unit

foreign import js_isDirectory :: String -> Effect $ Promise Boolean

foreign import js_symlink :: String -> String -> String -> Effect $ Promise Unit

foreign import js_readdir :: String -> Effect $ Promise $ Array String

newtype Path = Path String

instance Show Path where
  show (Path s) = s

class Pathlike a where
  pathStr :: a -> String

instance Pathlike Path where
  pathStr (Path p) = p

instance Pathlike String where
  pathStr s = s

x'readFile :: forall x p. Pathlike p => p -> EA' JsError x @@> Buffer
x'readFile = e'runEffPromise <<< js_readFile <<< pathStr

x'readTextFile :: forall x p. Pathlike p => p -> EA' JsError x @@> String
x'readTextFile = e'runEffPromise <<< js_readTextFile <<< pathStr

x'decodeTextFile
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA' Sys.FSDataError x @@> d
x'decodeTextFile p = do
  contents <- Sys.ReadError <!$> x'readTextFile p
  e'ok $ mapL Sys.DecodeError $ decode contents

x'decodeYamlString
  :: forall x @d
   . DecodeJson d
  => String
  -> EA' Sys.FSDataError x @@> d
x'decodeYamlString contents = do
  json <- e'ok $ mapL Sys.ReadError $ js_loadYaml contents Left Right
  e'ok $ mapL Sys.DecodeError $ decodeJson json

x'decodeYamlFile
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA' Sys.FSDataError x @@> d
x'decodeYamlFile p = do
  contents <- Sys.ReadError <!$> x'readTextFile p
  x'decodeYamlString contents

x'decodeAnyYamlExt
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA' Sys.FSDataError x @@> d
x'decodeAnyYamlExt p = do
  contents <- e'tryUntil
    (Sys.ReadError <!$> x'readTextFile $ (pathStr p) <> ".yaml")
    [ const $ Sys.ReadError <!$> x'readTextFile $ (pathStr p) <> ".json"
    , const $ Sys.ReadError <!$> x'readTextFile $ p
    ]
  x'decodeYamlString contents

x'mkdir :: forall x p. Pathlike p => p -> EA' JsError x @@> Unit
x'mkdir = e'runEffPromise <<< js_mkdir <<< pathStr

x'mkdirP :: forall x p. Pathlike p => p -> EA' JsError x @@> Unit
x'mkdirP = e'runEffPromise <<< js_mkdirp <<< pathStr

x'rimraf :: forall x p. Pathlike p => p -> EA' JsError x @@> Unit
x'rimraf = e'runEffPromise <<< js_rimraf <<< pathStr

x'isDirectory :: forall x p. Pathlike p => p -> A' x @@> Boolean
x'isDirectory p =
  pathStr p # js_isDirectory # e'runEffPromise # e'try >>= case _ of
    Left _ -> pure false
    Right res -> pure res

x'symlink
  :: forall x p1 p2
   . Pathlike p1
  => Pathlike p2
  => p1
  -> p2
  -> EA' JsError x @@> Unit
x'symlink p1 p2 =
  e'runEffPromise $ js_symlink (pathStr p1) (pathStr p2) "junction"

x'readdir :: forall x p. Pathlike p => p -> A' x @@> Array Path
x'readdir p = pathStr p # js_readdir # e'runEffPromise # e'try >>= case _ of
  Left _ -> pure []
  Right res -> pure $ res <#> (/./) p

x'writeTextFile
  :: forall x p. Pathlike p => p -> String -> EA' JsError x @@> Unit
x'writeTextFile p = e'runEffPromise <<< js_writeTextFile (pathStr p)

x'writeTextFileP
  :: forall x p. Pathlike p => p -> String -> EA' JsError x @@> Unit
x'writeTextFileP p s = do
  x'mkdirP $ dirname p
  x'writeTextFile p s

x'encodeTextFile
  :: forall x p d
   . Pathlike p
  => EncodeJson d
  => p
  -> d
  -> EA' JsError x @@> Unit
x'encodeTextFile p d = x'writeTextFile p $ encode d

x'encodeTextFileP
  :: forall x p d
   . Pathlike p
  => EncodeJson d
  => p
  -> d
  -> EA' JsError x @@> Unit
x'encodeTextFileP p d = x'writeTextFileP p $ encode d

foreign import js_lookupEnv
  :: (String -> Maybe String)
  -> Maybe String
  -> String
  -> Effect (Maybe String)

lookupEnv :: String -> Effect $ Maybe String
lookupEnv = js_lookupEnv Just Nothing

x'lookupEnv :: forall x. String -> A' x @@> Maybe String
x'lookupEnv k = e'try (e'runEffA $ lookupEnv k) <#> getRes
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
runXAThenExitWithArgv fm = runXAThenExit $ x'argv >>= fm

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

x'argv :: forall x. X'Node x @@> Array String
x'argv = lift p'X'Node (FullArgvCmd (arr'drop 2))

x'pid :: forall x. X'Node x @@> Int
x'pid = lift p'X'Node $ PidCmd identity

x'wd :: forall x. X'Node x @@> Path
x'wd = lift p'X'Node (WdCmd Path)

x'envPaths :: forall x. String -> Maybe String -> X'Node x @@> EnvPaths
x'envPaths appName suffix = lift p'X'Node (EnvPathsCmd appName suffix id)

x'platform :: forall x. X'Node x @@> Platform
x'platform = lift p'X'Node (PlatformCmd toPlatform)

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
foreign import js_pid :: Effect Int
foreign import js_argv :: Effect (Array String)

foreign import data EnvPaths :: Type

foreign import js_envPaths :: String -> Json -> Effect EnvPaths

data XNodeF a
  = WdCmd (String -> a)
  | FullArgvCmd (Array String -> a)
  | PidCmd (Int -> a)
  | PlatformCmd (String -> a)
  | EnvPathsCmd String (Maybe String) (EnvPaths -> a)

handleXNode :: forall r. XNodeF ~> Run r
handleXNode = case _ of
  WdCmd f -> pure $ f (Unsafe.unsafePerformEffect js_wd)
  FullArgvCmd f -> pure $ f (Unsafe.unsafePerformEffect js_argv)
  PidCmd f -> pure $ f $ Unsafe.unsafePerformEffect js_pid
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

basename :: forall p. Pathlike p => p -> String
basename p = js_pathBasename $ pathStr p

pathJoin :: forall p1 p2. Pathlike p1 => Pathlike p2 => p1 -> p2 -> Path
pathJoin p1 p2 = Path $ js_pathJoin (pathStr p1) (pathStr p2)

pathJoinAbs :: forall p1 p2. Pathlike p1 => Pathlike p2 => p1 -> p2 -> Path
pathJoinAbs p1 p2 = Path $ js_pathJoinAbs (pathStr p1) (pathStr p2)

infixr 0 pathJoin as /./

-- join paths unless rightside is absolute in which case, use rightside
infixr 0 pathJoinAbs as /.|//

x'argParse
  :: forall x a
   . String
  -> O.ParserInfo a
  -> Array String
  -> (a -> X'Node x @@> Unit)
  -> X'Node x @@> Unit
x'argParse progName opts args fm =
  handleParse $ O.execParserPure O.defaultPrefs opts args
  where
  handleParse (O.Success a) = fm a
  handleParse (O.Failure f) = do
    let msg /\ _exit = O.renderFailure f progName
    x'outErr msg
    pure unit
  handleParse _ = pure unit
