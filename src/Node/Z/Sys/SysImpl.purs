module Node.Z.Sys.SysImpl
  ( (/./)
  , (/.|//)
  , EnvPaths
  , Path
  , Platform(..)
  , XNode
  , XNodeF
  , xArgParse
  , basename
  , class Pathlike
  , dirname
  , envCfg
  , envData
  , envTmp
  , pathJoin
  , pathJoinAbs
  , pathStr
  , xArgv
  , xDecodeAnyYamlExt
  , xDecodeTextFile
  , xDecodeYamlFile
  , xEncodeTextFile
  , xEncodeTextFileP
  , xEnvPaths
  , runXAThenExit
  , runXAThenExitWithArgv
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

xReadFile :: forall x p. Pathlike p => p -> EA JsError x #> Buffer
xReadFile = g @XRunEffPromise <<< js_readFile <<< pathStr

xReadTextFile :: forall x p. Pathlike p => p -> EA JsError x #> String
xReadTextFile = g @XRunEffPromise <<< js_readTextFile <<< pathStr

xDecodeTextFile
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA Sys.FSDataError x #> d
xDecodeTextFile p = do
  contents <- g @XMapE Sys.ReadError $ xReadTextFile p
  g @XOk $ mapL Sys.DecodeError $ decode contents

xDecodeYamlString
  :: forall x @d
   . DecodeJson d
  => String
  -> EA Sys.FSDataError x #> d
xDecodeYamlString contents = do
  json <- g @XOk $ mapL Sys.ReadError $ js_loadYaml contents Left Right
  g @XOk $ mapL Sys.DecodeError $ decodeJson json

xDecodeYamlFile
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA Sys.FSDataError x #> d
xDecodeYamlFile p = do
  contents <- g @XMapE Sys.ReadError $ xReadTextFile p
  xDecodeYamlString contents

xDecodeAnyYamlExt
  :: forall x p @d
   . Pathlike p
  => DecodeJson d
  => p
  -> EA Sys.FSDataError x #> d
xDecodeAnyYamlExt p = do
  contents <- g @XTryUntil
    (g @XMapE Sys.ReadError $ xReadTextFile $ (pathStr p) <> ".yaml")
    [ const $ g @XMapE Sys.ReadError $ xReadTextFile $ (pathStr p) <>
        ".json"
    , const $ g @XMapE Sys.ReadError $ xReadTextFile $ p
    ]
  xDecodeYamlString contents

xMkdir :: forall x p. Pathlike p => p -> EA JsError x #> Unit
xMkdir = g @XRunEffPromise <<< js_mkdir <<< pathStr

xMkdirP :: forall x p. Pathlike p => p -> EA JsError x #> Unit
xMkdirP = g @XRunEffPromise <<< js_mkdirp <<< pathStr

xWriteTextFile
  :: forall x p. Pathlike p => p -> String -> EA JsError x #> Unit
xWriteTextFile p = g @XRunEffPromise <<< js_writeTextFile (pathStr p)

xWriteTextFileP
  :: forall x p. Pathlike p => p -> String -> EA JsError x #> Unit
xWriteTextFileP p s = do
  xMkdirP $ dirname p
  xWriteTextFile p s

xEncodeTextFile
  :: forall x p d
   . Pathlike p
  => EncodeJson d
  => p
  -> d
  -> EA JsError x #> Unit
xEncodeTextFile p d = xWriteTextFile p $ encode d

xEncodeTextFileP
  :: forall x p d
   . Pathlike p
  => EncodeJson d
  => p
  -> d
  -> EA JsError x #> Unit
xEncodeTextFileP p d = xWriteTextFileP p $ encode d

foreign import js_lookupEnv
  :: (String -> Maybe String)
  -> Maybe String
  -> String
  -> Effect (Maybe String)

lookupEnv :: String -> Effect $ Maybe String
lookupEnv = js_lookupEnv Just Nothing

xLookupEnv :: forall x. String -> A x #> Maybe String
xLookupEnv k = g @XTry (g @XRunEffA $ lookupEnv k) <#> getRes
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

type XNodeEA e x = EA e (XNODE x)

runXAThenExit
  :: forall @w @e a. RtError e => XRunWA w (XNodeEA e) a -> Effect Unit
runXAThenExit m = effAffThenExit $ runXA $ do
  w /\ res <- g @XRunW $ expand $ runXNode m
  when (arrSize w > 0) do
    xLogWarning "collected warnings ⌄"
    xLogWarning w
  pure res

runXAThenExitWithArgv
  :: forall @w @e a
   . RtError e
  => (Array String -> XRunWA w (XNodeEA e) a)
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

derive instance Functor XNodeF

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

type XNode x a = XRun (xNode :: XNodeF | x) a

xArgParse
  :: forall x a
   . String
  -> O.ParserInfo a
  -> Array String
  -> (a -> XNode x Unit)
  -> XNode x Unit
xArgParse progName opts args fm =
  handleParse $ O.execParserPure O.defaultPrefs opts args
  where
  handleParse (O.Success a) = fm a
  handleParse (O.Failure f) = do
    let msg /\ _exit = O.renderFailure f progName
    xOutErr msg
    pure unit
  handleParse _ = pure unit
