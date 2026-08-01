module Node.Z.Sys.SysImpl
  ( (/./)
  , (/.|//)
  , EnvPaths
  , Path
  , Platform(..)
  , XNode
  , XNodeF
  , argParse
  , basename
  , class Pathlike
  , decodeAnyYamlExt
  , decodeTextFile
  , decodeYamlFile
  , dirname
  , encodeTextFile
  , encodeTextFileP
  , envCfg
  , envData
  , envTmp
  , lookupEnv
  , mkdir
  , mkdirP
  , pathJoin
  , pathJoinAbs
  , pathStr
  , readFile
  , readTextFile
  , writeTextFile
  , writeTextFileP
  , xArgv
  , xEnvPaths
  , xExecAndExit
  , xExecAndExitArgv
  , xLookupEnv
  , xPlatform
  , xWd
  ) where

import Z.Prelude
import Z.Sys.Module as Sys
import Z.Z.Opt as O
import Effect.Unsafe as Unsafe

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

instance showPath :: Show Path where
  show (Path s) = s

class Pathlike a where
  pathStr :: a -> String

instance pathlikePath :: Pathlike Path where
  pathStr (Path p) = p

instance pathlikeString :: Pathlike String where
  pathStr s = s

readFile :: forall x p. Pathlike p => p -> EA JsError x #> Buffer
readFile = xEffectPromise <<< js_readFile <<< pathStr

readTextFile :: forall x p. Pathlike p => p -> EA JsError x #> String
readTextFile = xEffectPromise <<< js_readTextFile <<< pathStr

decodeTextFile
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA Sys.FSDataError x #> d
decodeTextFile p = do
  contents <- xMapE Sys.ReadError $ readTextFile p
  xOk $ mapL Sys.DecodeError $ decode contents

decodeYamlString
  :: forall x @d
   . DecodeJson d
  => String
  -> EA Sys.FSDataError x #> d
decodeYamlString contents = do
  json <- xOk $ mapL Sys.ReadError $ js_loadYaml contents Left Right
  xOk $ mapL Sys.DecodeError $ decodeJson json

decodeYamlFile
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA Sys.FSDataError x #> d
decodeYamlFile p = do
  contents <- xMapE Sys.ReadError $ readTextFile p
  decodeYamlString contents

decodeAnyYamlExt
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA Sys.FSDataError x #> d
decodeAnyYamlExt p = do
  contents <- xTryUntil
    (xMapE Sys.ReadError $ readTextFile $ (pathStr p) <> ".yaml")
    [ const $ xMapE Sys.ReadError $ readTextFile $ (pathStr p) <> ".json"
    , const $ xMapE Sys.ReadError $ readTextFile $ p
    ]
  decodeYamlString contents

mkdir :: forall x p. Pathlike p => p -> EA JsError x #> Unit
mkdir = xEffectPromise <<< js_mkdir <<< pathStr

mkdirP :: forall x p. Pathlike p => p -> EA JsError x #> Unit
mkdirP = xEffectPromise <<< js_mkdirp <<< pathStr

writeTextFile
  :: forall x p. Pathlike p => p -> String -> EA JsError x #> Unit
writeTextFile p = xEffectPromise <<< js_writeTextFile (pathStr p)

writeTextFileP
  :: forall x p. Pathlike p => p -> String -> EA JsError x #> Unit
writeTextFileP p s = do
  mkdirP $ dirname p
  writeTextFile p s

encodeTextFile
  :: forall x p d
   . Pathlike p
  => EncodeJson d
  => p
  -> d
  -> EA JsError x #> Unit
encodeTextFile p d = writeTextFile p $ encode d

encodeTextFileP
  :: forall x p d
   . Pathlike p
  => EncodeJson d
  => p
  -> d
  -> EA JsError x #> Unit
encodeTextFileP p d = writeTextFileP p $ encode d

foreign import js_lookupEnv
  :: (String -> Maybe String)
  -> Maybe String
  -> String
  -> Effect (Maybe String)

lookupEnv :: String -> Effect $ Maybe String
lookupEnv = js_lookupEnv Just Nothing

xLookupEnv :: forall x. String -> A x #> Maybe String
xLookupEnv k = lookupEnv k # xAEff # xTry <#> getRes
  where
  getRes (Right (Just v)) = Just v
  getRes _ = Nothing

execAndExit :: forall e a. RtError e => Aff (Either e a) -> Effect Unit
execAndExit a = runAff_ onDone a
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

type XNodeEA e x = EA e (XNODE x)

xExecAndExit
  :: forall @w @e a. RtError e => XWa w (XNodeEA e) a -> Effect Unit
xExecAndExit m = execAndExit $ xExecAff $ do
  w /\ res <- xListen $ expand $ runXNode m
  when (arrSize w > 0) do
    xLogWarning "collected warnings ⌄"
    xLogWarning w
  pure res

xExecAndExitArgv
  :: forall @w @e a
   . RtError e
  => (Array String -> XWa w (XNodeEA e) a)
  -> Effect Unit
xExecAndExitArgv fm = xExecAndExit $ xArgv >>= fm

data Platform = Win32 | Darwin | Linux | Android | FreeBSD | OpenBSD | Unknown

derive instance eqPlatform :: Eq Platform

toPlatform :: String -> Platform
toPlatform "win32" = Win32
toPlatform "darwin" = Darwin
toPlatform "linux" = Linux
toPlatform "android" = Android
toPlatform "freebsd" = FreeBSD
toPlatform "openbsd" = OpenBSD
toPlatform _ = Unknown

xArgv :: forall x. XNode x (Array String)
xArgv = lift _xNode (FullArgvCmd (arrDrop 2))

xWd :: forall x. XNode x Path
xWd = lift _xNode (WdCmd Path)

xEnvPaths :: forall x. String -> Maybe String -> XNode x EnvPaths
xEnvPaths appName suffix = lift _xNode (EnvPathsCmd appName suffix id)

xPlatform :: forall x. XNode x Platform
xPlatform = lift _xNode (PlatformCmd toPlatform)

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
  | EnvPathsCmd String (Maybe String)
      ( EnvPaths
        -> a
      )

handleXNode :: forall r. XNodeF ~> Run r
handleXNode = case _ of
  WdCmd f -> pure $ f (Unsafe.unsafePerformEffect js_wd)
  FullArgvCmd f -> pure $ f (Unsafe.unsafePerformEffect js_argv)
  PlatformCmd f -> pure $ f (Unsafe.unsafePerformEffect js_platform)
  EnvPathsCmd appName suffix f -> pure $ f $ Unsafe.unsafePerformEffect $
    js_envPaths appName (encodeOpts { suffix })

derive instance functorXBaseF :: Functor XNodeF

type XNODE x = (xNode :: XNodeF | x)

_xNode = Proxy :: Proxy "xNode"

runXNode :: forall r. Run (XNODE + r) ~> Run r
runXNode = run (on _xNode handleXNode send)

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

type XNode x a = X (xNode :: XNodeF | x) a

argParse
  :: forall x a
   . String
  -> O.ParserInfo a
  -> Array String
  -> (a -> XNode x Unit)
  -> XNode x Unit
argParse progName opts args fm =
  handleParse $ O.execParserPure O.defaultPrefs opts args
  where
  handleParse (O.Success a) = fm a
  handleParse (O.Failure f) = do
    let msg /\ _exit = O.renderFailure f progName
    xOutErr msg
    pure unit
  handleParse _ = pure unit
