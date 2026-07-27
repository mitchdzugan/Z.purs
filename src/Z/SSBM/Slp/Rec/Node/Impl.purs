module Z.SSBM.Slp.Rec.Node.Impl where

import Z.Prelude
import Z.SSBM.Slp.Port as Port
import Z.Sys.Node.Module as Sys

addConfigs
  :: forall x
   . Boolean
  -> Sys.Path
  -> Array String
  -> SEA EnvBuildState Error x #> Unit
addConfigs allowFNF wd configPaths = do
  forM_ configPaths \configPath -> do
    let fullPath = wd Sys./- configPath
    Sys.decodeAnyYamlExt @RecordConfig fullPath # xTry >>= onDecode fullPath
  where
  onDecode fullPath (Right c) = do
    xModify $ updateEnv c
    addConfigs false (Sys.dirname fullPath) (gmOr'_ @"includes?" c)
  onDecode fp (Left (Sys.ReadError _)) = do
    when (not allowFNF) $ xFail $ ConfigNotFound $ show fp
  onDecode _ (Left (Sys.DecodeError e)) = xFail $ ConfigDecodeErr e

run :: forall x. Array String -> Sys.XNode (EA Error x) Unit
run args = do
  wd <- Sys.wd
  envPaths <- Sys.envPaths "slp-rec" $ Just ""
  platform <- Sys.platform
  let cfgPath = Sys.envCfg envPaths
  let tmpPath = Sys.envTmp envPaths
  let
    launcherSettingsPath =
      cfgPath
        Sys./ ".."
        Sys./ (if platform == Sys.Win32 then ".." else ".")
        Sys./ "Slippi Launcher"
        Sys./ "Settings"
  launcherSettings <-
    Sys.decodeTextFile @LauncherSettings' launcherSettingsPath # xTry <#>
      hush
  let isoPath = launcherSettings <#> g_ @"settings.isoPath"
  let
    envStateInit =
      { isoPath: isoPath
      , tempPath: show tmpPath
      , texturePaths: Nil
      , iniMods: Nil
      , geckoCodes: Nil
      , geckoEnables: Nil
      , geckoDisables: Nil
      }
  Sys.argParse (slpRecInfo wd) args \opts -> do
    let optConfigs = arrFromFoldable $ g_ @"!.configPaths" opts
    let noOptConfigs = arrSize optConfigs == 0
    let baseConfigPath = show $ cfgPath Sys./ "config"
    let configs = if noOptConfigs then [ baseConfigPath ] else optConfigs
    envState <- xRunS envStateInit $ addConfigs noOptConfigs wd configs
    env <- finalizeEnv envState opts $ show $ wd Sys./ "output.mp4"
    xInfo env

mergeListOps
  :: forall a f. Foldable f => List a -> f (ListOp a) -> List a
mergeListOps l ops = foldlDefault folder l ops
  where
  folder _ LReset = Nil
  folder l' (LCons v) = Cons v l'

mergeMListOps
  :: forall a f. Foldable f => List a -> Maybe (f (ListOp a)) -> List a
mergeMListOps l Nothing = l
mergeMListOps l (Just ops) = mergeListOps l ops

arrMergeListOpts
  :: forall a f. Foldable f => List a -> f (ListOp a) -> Array a
arrMergeListOpts a b = arrFromFoldable $ mergeListOps a b

updateEnv :: RecordConfig -> EnvBuildState -> EnvBuildState
updateEnv cfg st =
  { isoPath: cfg.isoPath >|> st.isoPath
  , tempPath: st.tempPath
  , texturePaths: mergeMListOps st.texturePaths cfg.texturePaths
  , iniMods: mergeMListOps st.iniMods cfg.iniMods
  , geckoCodes: mergeMListOps st.geckoCodes cfg.geckoCodes
  , geckoEnables: mergeMListOps st.geckoEnables cfg.geckoEnables
  , geckoDisables: mergeMListOps st.geckoDisables cfg.geckoDisables
  }

finalizeEnv
  :: forall x. EnvBuildState -> CliOpts -> String -> E Error x #> RecordEnv
finalizeEnv st (CliOpts opts) defaultOutputPath = do
  isoPath <- xOk $ jOrE NoIso st.isoPath
  pure
    { isoPath
    , outputPath: jOr defaultOutputPath opts.outputPath
    , startFrame: opts.startFrame
    , totalFrames: opts.totalFrames
    , recPath: opts.recPath
    , tempPath: jOr st.tempPath opts.tempPath
    , texturePaths: arrMergeListOpts st.texturePaths opts.texturePaths
    , iniMods: arrMergeListOpts st.iniMods opts.iniMods
    , geckoCodes: arrMergeListOpts st.geckoCodes opts.geckoCodes
    , geckoEnables: arrMergeListOpts st.geckoEnables opts.geckoEnables
    , geckoDisables: arrMergeListOpts st.geckoDisables opts.geckoDisables
    , colorOverrides: mapFromFoldable $ unwrap
        <$> mergeListOps Nil opts.colorOverrides
    , slippiPlaybackBin: "slippi-playback"
    , ffmpegBin: "ffmpeg"
    }

type LauncherSettings' =
  { settings :: { isoPath :: String }
  }

type EnvBuildState =
  { isoPath :: Maybe String
  , tempPath :: String
  , texturePaths :: List String
  , iniMods :: List IniMod
  , geckoCodes :: List String
  , geckoEnables :: List String
  , geckoDisables :: List String
  }

type RecordEnv =
  { startFrame :: Maybe Int
  , totalFrames :: Maybe Int
  , outputPath :: String
  , recPath :: String
  , isoPath :: String
  , tempPath :: String
  , texturePaths :: Array String
  , iniMods :: Array IniMod
  , geckoCodes :: Array String
  , geckoEnables :: Array String
  , geckoDisables :: Array String
  , colorOverrides :: Map Port.T Int
  , slippiPlaybackBin :: String
  , ffmpegBin :: String
  }

type CfgMany a = Maybe (Array (ListOp a))
type CliMany a = List (ListOp a)

type RecordConfig =
  { isoPath :: Maybe String
  , texturePaths :: CfgMany String
  , iniMods :: CfgMany IniMod
  , tempPath :: Maybe String
  , geckoCodes :: CfgMany String
  , geckoEnables :: CfgMany String
  , geckoDisables :: CfgMany String
  , slippiPlaybackBin :: Maybe String
  , ffmpegBin :: Maybe String
  , includes :: Maybe (Array String)
  }

newtype CliOpts = CliOpts
  { startFrame :: Maybe Int
  , totalFrames :: Maybe Int
  , outputPath :: Maybe String
  , isoPath :: Maybe String
  , texturePaths :: CliMany String
  , iniMods :: CliMany IniMod
  , geckoCodes :: CliMany String
  , geckoEnables :: CliMany String
  , geckoDisables :: CliMany String
  , colorOverrides :: CliMany PortCostume
  , tempPath :: Maybe String
  , configPaths :: List String
  , slippiPlaybackBin :: Maybe String
  , ffmpegBin :: Maybe String
  , recPath :: String
  }

derive instance newtypeCliOpts :: Newtype CliOpts _

type IniFilename = String
type IniProperty = String
type IniValue = String

data IniMod = IniMod IniFilename IniProperty IniValue

derive instance eqIniMod :: Eq IniMod
derive instance ordIniMod :: Ord IniMod

derive instance genericIniMod :: Generic IniMod _

iniModToStr :: IniMod -> String
iniModToStr (IniMod i p v) = i <> ":" <> p <> "=" <> v

iniModOfStr :: String -> Either String IniMod
iniModOfStr s = do
  let csplit = strSplit (Pattern ":") s
  i <- jOrE emsg $ nth csplit 0
  rest <- jOrE emsg $ nth csplit 1
  let rsplit = strSplit (Pattern "=") rest
  p <- jOrE emsg $ nth rsplit 0
  v <- jOrE emsg $ nth rsplit 1
  pure $ IniMod i p v
  where
  emsg = "Expected `$ini:$prop=$val`"

instance decodeJsonIniMod :: DecodeJson IniMod where
  decodeJson x = do
    (baseDecodeJson x <#> iniModOfStr) >>= onEor
    where
    onEor (Right v) = pure v
    onEor (Left msg) = decodeFailTypeMismatch msg

instance encodeJsonIniMod :: EncodeJson IniMod where
  encodeJson x = encodeJson $ iniModToStr x

newtype PortCostume = PortCostume (Port.T /\ Int)

derive instance newtypePortCostume :: Newtype PortCostume _

derive instance eqPortCostume :: Eq PortCostume
derive instance ordPortCostume :: Ord PortCostume

derive instance genericPortCostume :: Generic PortCostume _

portCostumeToStr :: PortCostume -> String
portCostumeToStr (PortCostume (p /\ c)) = (show $ Port.asInt p) <> "=" <> show c

portCostumeOfStr :: String -> Either String PortCostume
portCostumeOfStr s = do
  let esplit = strSplit (Pattern "=") s
  p <- jOrE emsg $ nth esplit 0 >>= intFromString
  c <- jOrE emsg $ nth esplit 1 >>= intFromString
  pure $ PortCostume $ (Port.ofInt p) /\ c
  where
  emsg = "Expected `$port:$costume` => `[1|2|3|4]=[1|2|3|4|5|6]"

instance decodeJsonPortCostume :: DecodeJson PortCostume where
  decodeJson x = do
    (baseDecodeJson x <#> portCostumeOfStr) >>= onEor
    where
    onEor (Right v) = pure v
    onEor (Left msg) = decodeFailTypeMismatch msg

instance encodeJsonPortCostume :: EncodeJson PortCostume where
  encodeJson x = encodeJson $ portCostumeToStr x

data ListOp a = LReset | LCons a

instance decodeListOp :: DecodeJson a => DecodeJson (ListOp a) where
  decodeJson x = do
    caseJsonString decodeCons onString x
    where
    onString ":" = pure LReset
    onString _ = decodeCons
    decodeCons = baseDecodeJson x <#> LCons

instance encodeListOp :: EncodeJson a => EncodeJson (ListOp a) where
  encodeJson LReset = encodeJson ":"
  encodeJson (LCons a) = encodeJson a

optJson :: forall @a. DecodeJson a => OptReadM a
optJson = optEitherReader \s -> mapL show $ decode @a ("\"" <> s <> "\"")

optJsonListOp :: forall @a. DecodeJson a => OptReadM (ListOp a)
optJsonListOp = optJson @(ListOp a)

optReadIniMod :: OptReadM IniMod
optReadIniMod = pure $ IniMod "" "" ""

optReadColorOverride :: OptReadM (Port.T /\ Int)
optReadColorOverride = pure $ Port.P1 /\ 1

cliOpts :: Sys.Path -> OptParser CliOpts
cliOpts wd = map CliOpts $ optsProd
  <$> optStrArgument
    (optMetavar "SLP_FILE" <> optHelp ".slp file to record")
  <*> optional
    ( optOption optInt
        $ (optLong "start-frame" <> optShort 's' <> optMetavar "INT")
        <> optHelp
          "First frame to begin recording (default: `GAME_FRAME_START`)"

    )
  <*> optional
    ( optOption optInt
        $ (optLong "total-frames" <> optShort 't' <> optMetavar "INT")
        <> optHelp "Total frames to record (default: `all remaining`)"
    )
  <*> optional
    ( optStrOption $ (optLong "output" <> optShort 'o' <> optMetavar "MP4")
        <> optHelp
          ( "Output file (default: "
              <> show (wd Sys./ "output.mp4")
              <> ")"
          )
    )
  <*> optional
    ( optStrOption $ (optLong "iso" <> optShort 'i' <> optMetavar "ISO")
        <> optHelp
          ( "melee iso file (default: `slippi-launcher config`)"
          )
    )
  <*> optMany
    ( optOption (optJsonListOp @String)
        $ (optLong "texture-path" <> optShort 'x' <> optMetavar "DIR")
        <> optHelp "directory with texture overrides"
    )
  <*> optMany
    ( optOption (optJsonListOp @PortCostume)
        $ (optLong "port-costume" <> optShort 'p' <> optMetavar "PORTC")
        <> optHelp
          ( "port costume overrides. PORTC => `$port=$costime`"
              <> " => `[1|2|3|4]=[1|2|3|4|5|6]`"
          )
    )
  <*> optMany
    ( optOption (optJsonListOp @IniMod)
        $ (optLong "ini-mod" <> optShort 'I' <> optMetavar "INI_MOD")
        <> optHelp
          ( "slippi ini overrides. INI_MOD => `$ini:$prop=$val`"
              <> " => `[Dolphin|GFX|Logger]:$prop=$val"
          )
    )
  <*> optMany
    ( optOption (optJsonListOp @String)
        $ (optLong "gecko-code" <> optShort 'g' <> optMetavar "CODE")
        <> optHelp
          "raw string containing code to directly include while recording"
    )
  <*> optMany
    ( optOption (optJsonListOp @String)
        $ (optLong "gecko-enable" <> optShort '+' <> optMetavar "NAME")
        <> optHelp "name of gecko codes to force enable"
    )
  <*> optMany
    ( optOption (optJsonListOp @String)
        $ (optLong "gecko-disable" <> optShort '_' <> optMetavar "NAME")
        <> optHelp "name of gecko codes to force disable"
    )
  <*> optional
    ( optStrOption
        $ (optLong "temp-path" <> optShort 'T' <> optMetavar "DIR")
        <> optHelp "directory with store temporary recording files"
    )
  <*> optMany
    ( optStrOption
        $ (optLong "config" <> optShort 'c' <> optMetavar "FILE")
        <> optHelp "config files to source"
    )
  <*> optional
    ( optStrOption
        $ (optLong "slippi-playback" <> optShort 'S' <> optMetavar "BIN")
        <> optHelp "slippi-playback binary path"
    )
  <*> optional
    ( optStrOption
        $ (optLong "ffmpeg" <> optShort 'F' <> optMetavar "BIN")
        <> optHelp "ffmpeg binary path"
    )
  where
  optsProd a b c d e f g h i j k l m n o =
    { recPath: a
    , startFrame: b
    , totalFrames: c
    , outputPath: d
    , isoPath: e
    , texturePaths: f
    , colorOverrides: g
    , iniMods: h
    , geckoCodes: i
    , geckoEnables: j
    , geckoDisables: k
    , tempPath: l
    , configPaths: m
    , slippiPlaybackBin: n
    , ffmpegBin: o
    }

slpRecInfo :: Sys.Path -> OptParserInfo CliOpts
slpRecInfo wd = cliInfo (cliOpts wd <**> cliHelper)
  ( cliFullDesc
      <> cliProgDesc "record SLP to MP4"
      <> cliHeader "slp-rec | @dz-ssbm | .slp recording"
  )

data Error = NoIso | ConfigNotFound String | ConfigDecodeErr JsonDecodeError

instance errorRtError :: RtError Error where
  rtErrExtra _ = encodeJson {}
  rtErrName NoIso = "melee iso not found"
  rtErrName (ConfigNotFound _) = "config file not found"
  rtErrName (ConfigDecodeErr _) = "config file invalid type"
  -- rtErrName _ = "_err_name_not_implemented_"
  rtErrMessage NoIso = "please supply via opt `-i %ISO_PATH%`"
  rtErrMessage (ConfigNotFound p) = p
  rtErrMessage (ConfigDecodeErr e) = show e
-- rtErrMessage _ = "_err_message_not_implemented_"
