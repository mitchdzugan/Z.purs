module Node.Z.SSBM.Slp.Rec where

import Node.Z.Prelude

import Z.SSBM.Slp.Port as Port
import Z.Z.Opt as O

launchAndRecord :: forall x. REA RecordEnv Error x #> Unit
launchAndRecord = pure unit

addConfigs
  :: forall x
   . Boolean
  -> Path
  -> Array String
  -> SEA EnvBuildState Error x #> Unit
addConfigs allowFNF wd configPaths = do
  forM_ configPaths \configPath -> do
    let fullPath = wd /.|// configPath
    x' @"try" (xDecodeAnyYamlExt @RecordConfig fullPath) >>= onDecode fullPath
  where
  onDecode fullPath (Right c) = do
    x' @"modify" $ updateEnv c
    addConfigs false (dirname fullPath) (gmOr'_ @"includes?" c)
  onDecode fp (Left (ReadError _)) = do
    when (not allowFNF) $ x' @"fail" $ ConfigNotFound $ show fp
  onDecode _ (Left (DecodeError e)) = x' @"fail" $ ConfigDecodeErr e

xRun :: forall x. Array String -> EA Error x ##> Unit
xRun args = do
  wd <- xWd
  envPaths <- xEnvPaths "slp-rec" $ Just ""
  platform <- xPlatform
  let cfgPath = envCfg envPaths
  let tmpPath = envTmp envPaths
  let
    launcherSettingsPath =
      cfgPath
        /./ ".."
        /./ (if platform == Win32 then ".." else ".")
        /./ "Slippi Launcher"
        /./ "Settings"
  launcherSettings <-
    x' @"try" (xDecodeTextFile @LauncherSettings' launcherSettingsPath) <#>
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
      , slippiPlaybackBin: "slippi-playback"
      , ffmpegBin: "ffmpeg"
      }
  xArgParse "slp-rec" (slpRecInfo wd) args \opts -> do
    let optConfigs = arrFromFoldable $ g_ @"!.configPaths" opts
    let noOptConfigs = arrSize optConfigs == 0
    let baseConfigPath = show $ cfgPath /./ "config"
    let configs = if noOptConfigs then [ baseConfigPath ] else optConfigs
    envState <- x' @"execS" envStateInit $ addConfigs noOptConfigs wd configs
    env <- finalizeEnv envState opts $ show $ wd /./ "output.mp4"
    xInfo env
    z @XRunR env launchAndRecord

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
  , slippiPlaybackBin: jOr st.slippiPlaybackBin cfg.slippiPlaybackBin
  , ffmpegBin: jOr st.ffmpegBin cfg.ffmpegBin
  }

finalizeEnv
  :: forall x. EnvBuildState -> CliOpts -> String -> E Error x #> RecordEnv
finalizeEnv st (CliOpts opts) defaultOutputPath = do
  isoPath <- x' @"ok" $ jOrE NoIso st.isoPath
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
    , slippiPlaybackBin: jOr st.slippiPlaybackBin opts.slippiPlaybackBin
    , ffmpegBin: jOr st.ffmpegBin opts.ffmpegBin
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
  , slippiPlaybackBin :: String
  , ffmpegBin :: String
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

derive instance Newtype CliOpts _

type IniFilename = String
type IniProperty = String
type IniValue = String

data IniMod = IniMod IniFilename IniProperty IniValue

derive instance Eq IniMod
derive instance Ord IniMod

derive instance Generic IniMod _

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

instance DecodeJson IniMod where
  decodeJson x = do
    (baseDecodeJson x <#> iniModOfStr) >>= onEor
    where
    onEor (Right v) = pure v
    onEor (Left msg) = decodeFailTypeMismatch msg

instance EncodeJson IniMod where
  encodeJson x = encodeJson $ iniModToStr x

newtype PortCostume = PortCostume (Port.T /\ Int)

derive instance Newtype PortCostume _
derive instance Eq PortCostume
derive instance Ord PortCostume
derive instance Generic PortCostume _

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

instance DecodeJson PortCostume where
  decodeJson x = do
    (baseDecodeJson x <#> portCostumeOfStr) >>= onEor
    where
    onEor (Right v) = pure v
    onEor (Left msg) = decodeFailTypeMismatch msg

instance EncodeJson PortCostume where
  encodeJson x = encodeJson $ portCostumeToStr x

data ListOp a = LReset | LCons a

instance DecodeJson a => DecodeJson (ListOp a) where
  decodeJson x = do
    caseJsonString decodeCons onString x
    where
    onString "=" = pure LReset
    onString _ = decodeCons
    decodeCons = baseDecodeJson x <#> LCons

instance EncodeJson a => EncodeJson (ListOp a) where
  encodeJson LReset = encodeJson "="
  encodeJson (LCons a) = encodeJson a

optJson :: forall @a. DecodeJson a => O.ReadM a
optJson = O.eitherReader \s -> mapL show $ decode @a ("\"" <> s <> "\"")

optJsonListOp :: forall @a. DecodeJson a => O.ReadM (ListOp a)
optJsonListOp = optJson @(ListOp a)

optReadIniMod :: O.ReadM IniMod
optReadIniMod = pure $ IniMod "" "" ""

optReadColorOverride :: O.ReadM (Port.T /\ Int)
optReadColorOverride = pure $ Port.P1 /\ 1

cliOpts :: Path -> O.Parser CliOpts
cliOpts wd = map CliOpts $ optsProd
  <$> O.strArgument
    (O.metavar "SLP_FILE" <> O.help ".slp file to record")
  <*> optional
    ( O.option O.int $ (O.long "start-frame" <> O.short 's' <> O.metavar "INT")
        <> O.help
          "First frame to begin recording (default: `GAME_FRAME_START`)"

    )
  <*> optional
    ( O.option O.int $ (O.long "total-frames" <> O.short 't' <> O.metavar "INT")
        <> O.help "Total frames to record (default: `all remaining`)"
    )
  <*> optional
    ( O.strOption $ (O.long "output" <> O.short 'o' <> O.metavar "MP4")
        <> O.help
          ( "Output file (default: "
              <> show (wd /./ "output.mp4")
              <> ")"
          )
    )
  <*> optional
    ( O.strOption $ (O.long "iso" <> O.short 'i' <> O.metavar "ISO")
        <> O.help
          ( "melee iso file (default: `slippi-launcher config`)"
          )
    )
  <*> O.many
    ( O.option (optJsonListOp @String)
        $ (O.long "texture-path" <> O.short 'x' <> O.metavar "DIR+")
        <> O.help "directory with texture overrides"
    )
  <*> O.many
    ( O.option (optJsonListOp @PortCostume)
        $ (O.long "port-costume" <> O.short 'p' <> O.metavar "PORTC+")
        <> O.help
          ( "port costume overrides. PORTC => `$port=$costime`"
              <> " => `[1|2|3|4]=[1|2|3|4|5|6]`"
          )
    )
  <*> O.many
    ( O.option (optJsonListOp @IniMod)
        $ (O.long "ini-mod" <> O.short 'I' <> O.metavar "INI_MOD+")
        <> O.help
          ( "slippi ini overrides. INI_MOD => `$ini:$prop=$val`"
              <> " => `[Dolphin|GFX|Logger]:$prop=$val"
          )
    )
  <*> O.many
    ( O.option (optJsonListOp @String)
        $ (O.long "gecko-code" <> O.short 'g' <> O.metavar "CODE+")
        <> O.help
          "raw string containing code to directly include while recording"
    )
  <*> O.many
    ( O.option (optJsonListOp @String)
        $ (O.long "gecko-enable" <> O.short '+' <> O.metavar "NAME+")
        <> O.help "name of gecko codes to force enable"
    )
  <*> O.many
    ( O.option (optJsonListOp @String)
        $ (O.long "gecko-disable" <> O.short '_' <> O.metavar "NAME+")
        <> O.help "name of gecko codes to force disable"
    )
  <*> optional
    ( O.strOption
        $ (O.long "temp-path" <> O.short 'T' <> O.metavar "DIR")
        <> O.help "directory with store temporary recording files"
    )
  <*> O.many
    ( O.strOption
        $ (O.long "config" <> O.short 'c' <> O.metavar "FILE+")
        <> O.help "config files to source"
    )
  <*> optional
    ( O.strOption
        $ (O.long "slippi-playback" <> O.short 'S' <> O.metavar "BIN")
        <> O.help "slippi-playback binary path"
    )
  <*> optional
    ( O.strOption
        $ (O.long "ffmpeg" <> O.short 'F' <> O.metavar "BIN")
        <> O.help "ffmpeg binary path"
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

slpRecInfo :: Path -> O.ParserInfo CliOpts
slpRecInfo wd = O.info (cliOpts wd O.<**> O.helper)
  ( O.fullDesc <> O.footer
      ( "options with `+` can be repeated. ie:  ```slp-rec"
          <> " -g rmCrowdChants.gk"
          <> " -g rmCrowdNoises.gk"
          <> " ...```  will add both gecko codes. List options are added on"
          <> " top of ones found in configs. At any point supplying `=` to"
          <> " one of these options will discard all previous entries and"
          <> " begin a new list. ie:  ```slp-rec"
          <> " -g rmCrowdChants.gk -g ="
          <> " -g rmCrowdNoises.gk"
          <> " ...```  will only add rmCrowdNoises.gk"
      )
  )

data Error = NoIso | ConfigNotFound String | ConfigDecodeErr JsonDecodeError

instance RtError Error where
  rtErrExtra _ = encodeJson {}
  rtErrName NoIso = "melee iso not found"
  rtErrName (ConfigNotFound _) = "config file not found"
  rtErrName (ConfigDecodeErr _) = "config file invalid type"
  -- rtErrName _ = "_err_name_not_implemented_"
  rtErrMessage NoIso = "please supply via opt `-i %ISO_PATH%`"
  rtErrMessage (ConfigNotFound p) = p
  rtErrMessage (ConfigDecodeErr e) = show e
-- rtErrMessage _ = "_err_message_not_implemented_"
