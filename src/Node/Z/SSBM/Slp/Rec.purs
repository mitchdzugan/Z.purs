module Node.Z.SSBM.Slp.Rec where

import Node.Z.Prelude

import Z.SSBM.Slp.Port as Port
import Z.SSBM.Slp.Rec.Ini as Ini
import Z.Z.Opt as O

launchAndRecord :: forall x. REA' RecordEnv Error x @$> Unit
launchAndRecord = do
  ts <- x'nowMS
  pid <- x'pid
  filename'hash <- r'ask <#> \r -> ident'uuid $ simpleHash r.recPath
  let workId = "wd" <-> ts <-> filename'hash <-> pid
  workDir <- r'ask <#> \r -> r.tempPath /./ "work" /./ workId
  let userDir = workDir /./ "User"
  x'mkdirP userDir <!#> IoError
  -- e'withFinalizer (const $ x'rimraf workDir <!#> CleanupError) do
  e'withFinalizer (const $ pass) do
    let gsDir = userDir /./ "GameSettings"
    let gsFile = gsDir /./ "GALE01.ini"
    geckoCodes <- r'ask >>= \r ->
      forM r.geckoCodes x'readTextFile <!#> ReadGeckoCode
    { geckoEnables, geckoDisables } <- r'ask
    let
      gameSettingsStr = w'str'nl $ \w -> do
        w "[Gecko]" *> forM_ geckoCodes w *> w ""
        w "[Gecko_Enabled]" *> forM_ geckoEnables w *> w ""
        w "[Gecko_Disabled]" *> forM_ geckoDisables w *> w ""
    x'writeTextFileP gsFile gameSettingsStr <!#> WriteGameSettings
    texturePathsWithInd <- r'ask <#> \r -> arr'withInd r.texturePaths
    let texturesDir = userDir /./ "Load" /./ "Textures"
    x'mkdirP texturesDir <!#> IoError
    forM_ texturePathsWithInd \(ind /\ texturePath) -> do
      whenM (x'isDirectory texturePath <#> not) do
        e'fail $ InvalidTexturePath $ pathStr texturePath
      let symlinkDst = texturesDir /./ ("tx_link" <:> ind)
      x'symlink texturePath symlinkDst <!#> SymlinkTexturePath
    inis <- r'ask <#> \r -> Ini.mkInis r.iniMods
    forM_ (obj'entries inis) \(iniFilename /\ iniData) -> do
      let fullIniFilename = userDir /./ "Config" /./ iniFilename <> ".ini"
      let iniFileStr = ini'stringify iniData
      x'writeTextFileP fullIniFilename iniFileStr <!#> WriteIniFile iniFilename
    x'out { workDir }
    pure unit

addConfigs
  :: forall x
   . Boolean
  -> Path
  -> Array String
  -> SEA' EnvBuildState Error x @@> Unit
addConfigs allowFNF wd configPaths = do
  forM_ configPaths \configPath -> do
    let fullPath = wd /.|// configPath
    e'try (x'decodeAnyYamlExt @RecordConfig fullPath) >>= onDecode fullPath
  where
  onDecode fullPath (Right c) = do
    let configDir = (dirname fullPath)
    s'update $ updateEnv configDir c
    addConfigs false configDir (gmOr'_ @"includes?" c)
  onDecode fp (Left (ReadError _)) = do
    when (not allowFNF) $ e'fail $ ConfigNotFound $ show fp
  onDecode _ (Left (DecodeError e)) = e'fail $ ConfigDecodeErr e

getDefaultSlippiPlaybackBin :: forall x. Path -> A' x @$> String
getDefaultSlippiPlaybackBin slippiLauncherPath = x'withReturn \x'return -> do
  filenames <- x'readdir $ slippiLauncherPath /./ "playback"
  betaFilenames <- x'readdir $ slippiLauncherPath /./ "playback-beta"
  forM_ (arr'concat [ filenames, betaFilenames ]) \filename -> do
    let file'str = pathStr filename
    let file'ends = flip str'endsWith $ file'str
    let file'starts = flip str'startsWith $ basename file'str
    x'platform >>= case _ of
      Win32 -> when (file'ends "Dolphin.exe") $ x'return file'str
      Darwin -> when (file'ends "Dolphin.app") $ x'return file'str
      Linux -> do
        let isAppImage = file'ends "AppImage" && file'starts "Slippi_Playback"
        when (isAppImage || file'ends "dolphin-emu") $ x'return file'str
      _ -> pure unit
  pure "slippi-playback"

xRun :: forall x. Array String -> EA' Error x @$> Unit
xRun args = do
  wd <- x'wd
  envPaths <- x'envPaths "slp-rec" $ Just ""
  platform <- x'platform
  let
    cfgPath = envCfg envPaths
    tmpPath = envTmp envPaths
    slippiLauncherPath =
      cfgPath
        /./ ".."
        /./ (if platform == Win32 then ".." else ".")
        /./ "Slippi Launcher"
    launcherSettingsPath = slippiLauncherPath /./ "Settings"
  defaultSlippiPlaybackBin <- getDefaultSlippiPlaybackBin slippiLauncherPath
  x'out { defaultSlippiPlaybackBin }
  launcherSettings <-
    e'try (x'decodeTextFile @LauncherSettings' launcherSettingsPath) <#> hush
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
      , slippiPlaybackBin: defaultSlippiPlaybackBin
      , ffmpegBin: "ffmpeg"
      }
  x'argParse "slp-rec" (slpRecInfo wd) args \opts -> do
    let optConfigs = arr'fromFoldable $ g_ @"!.configPaths" opts
    let noOptConfigs = arr'size optConfigs == 0
    let baseConfigPath = show $ cfgPath /./ "config"
    let configs = if noOptConfigs then [ baseConfigPath ] else optConfigs
    envState <- s'exec envStateInit $ addConfigs noOptConfigs wd configs
    env <- finalizeEnv envState opts $ show $ wd /./ "output.mp4"
    x'info env
    r'run env launchAndRecord

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
arrMergeListOpts a b = arr'fromFoldable $ mergeListOps a b

updateEnv :: Path -> RecordConfig -> EnvBuildState -> EnvBuildState
updateEnv path cfg st =
  { isoPath: cfg.isoPath >|> st.isoPath
  , tempPath: st.tempPath
  , texturePaths: mergeMListOps st.texturePaths cfg.texturePaths
  , iniMods: mergeMListOps st.iniMods cfg.iniMods
  , geckoCodes: mergeMListOps st.geckoCodes $ cfg.geckoCodes
      <#> ffmap (show <<< (/.|//) path)
  , geckoEnables: mergeMListOps st.geckoEnables cfg.geckoEnables
  , geckoDisables: mergeMListOps st.geckoDisables cfg.geckoDisables
  , slippiPlaybackBin: jOr st.slippiPlaybackBin cfg.slippiPlaybackBin
  , ffmpegBin: jOr st.ffmpegBin cfg.ffmpegBin
  }

finalizeEnv
  :: forall x. EnvBuildState -> CliOpts -> String -> E' Error x @@> RecordEnv
finalizeEnv st (CliOpts opts) defaultOutputPath = do
  isoPath <- e'ok $ jOrE NoIso $ opts.isoPath >|> st.isoPath
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
    , colorOverrides: hm'fromFoldable $ unwrap
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
  , iniMods :: List Ini.IniMod
  , geckoCodes :: List String
  , geckoEnables :: List String
  , geckoDisables :: List String
  , slippiPlaybackBin :: String
  , ffmpegBin :: String
  }

type RecordEnv =
  { startFrame :: Maybe FrameNumSpec
  , totalFrames :: Maybe FrameNumSpec
  , outputPath :: String
  , recPath :: String
  , isoPath :: String
  , tempPath :: String
  , texturePaths :: Array String
  , iniMods :: Array Ini.IniMod
  , geckoCodes :: Array String
  , geckoEnables :: Array String
  , geckoDisables :: Array String
  , colorOverrides :: HashMap Port.T Int
  , slippiPlaybackBin :: String
  , ffmpegBin :: String
  }

type CfgMany a = Maybe (Array (ListOp a))
type CliMany a = List (ListOp a)

type RecordConfig =
  { isoPath :: Maybe String
  , texturePaths :: CfgMany String
  , iniMods :: CfgMany Ini.IniMod
  , tempPath :: Maybe String
  , geckoCodes :: CfgMany String
  , geckoEnables :: CfgMany String
  , geckoDisables :: CfgMany String
  , slippiPlaybackBin :: Maybe String
  , ffmpegBin :: Maybe String
  , includes :: Maybe (Array String)
  }

newtype CliOpts = CliOpts
  { startFrame :: Maybe FrameNumSpec
  , totalFrames :: Maybe FrameNumSpec
  , outputPath :: Maybe String
  , isoPath :: Maybe String
  , texturePaths :: CliMany String
  , iniMods :: CliMany Ini.IniMod
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

newtype PortCostume = PortCostume (Port.T /\ Int)

derive instance Newtype PortCostume _
derive instance Generic PortCostume _

portCostumeToStr :: PortCostume -> String
portCostumeToStr (PortCostume (p /\ c)) = (show $ Port.asInt p) <> "=" <> show c

portCostumeOfStr :: String -> Either String PortCostume
portCostumeOfStr s = do
  let esplit = str'split (Pattern "=") s
  whenNot (arr'size esplit == 2) $ Left emsg
  p <- jOrE emsg $ nth esplit 0 >>= tryParseInt
  c <- jOrE emsg $ nth esplit 1 >>= tryParseInt
  pure $ PortCostume $ (Port.ofInt p) /\ c
  where
  emsg = "Expected `$port=$costume` => `[1|2|3|4]=[1|2|3|4|5|6]"

instance DecodeJson PortCostume where
  decodeJson = decodeViaString portCostumeOfStr

instance EncodeJson PortCostume where
  encodeJson x = encodeJson $ portCostumeToStr x

data FrameNumSpec = RawFrameNum Int | InGameTime Int Number

frameNumSpecToStr :: FrameNumSpec -> String
frameNumSpecToStr (RawFrameNum n) = show n
frameNumSpecToStr (InGameTime mn secs) = "@" <> show mn <> ":" <> show secs

frameNumSpecOfStr :: String -> Either String FrameNumSpec
frameNumSpecOfStr s = do
  if str'startsWith "@" s then do
    let tsplit = str'split (Pattern ":") (str'drop 1 s)
    whenNot (arr'size tsplit == 2) $ Left emsg
    mn <- jOrE emsg $ nth tsplit 0 >>= tryParseInt
    sec <- jOrE emsg $ nth tsplit 1 >>= tryParseNum
    when (mn < 0) $ Left "Invalid InGameTime Minutes (negative)"
    when (sec < 0.0) $ Left "Invalid InGameTime Seconds (negative)"
    when (sec > 60.0) $ Left "Invalid InGameTime Seconds (> 60)"
    pure $ InGameTime mn sec
  else jOrE emsg $ tryParseInt s <#> RawFrameNum
  where
  emsg = "Expected `$frameNum | @$mins:$secs.$ms`"

instance DecodeJson FrameNumSpec where
  decodeJson = decodeViaString frameNumSpecOfStr

instance EncodeJson FrameNumSpec where
  encodeJson x = encodeJson $ frameNumSpecToStr x

data ListOp a = LReset | LCons a

derive instance Functor ListOp

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

cliOpts :: Path -> O.Parser CliOpts
cliOpts wd = map CliOpts $ optsProd
  <$> O.strArgument
    (O.metavar "SLP_FILE" <> O.help ".slp file to record")
  <*> optional
    ( O.option (optJson @FrameNumSpec)
        $ (O.long "start-frame" <> O.short 's' <> O.metavar "FRAME")
        <> O.help
          "First frame to begin recording (default: `GAME_FRAME_START`)"

    )
  <*> optional
    ( O.option (optJson @FrameNumSpec)
        $ (O.long "total-frames" <> O.short 't' <> O.metavar "FRAME")
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
    ( O.option (optJsonListOp @Ini.IniMod)
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

data Error
  = NoIso
  | ConfigNotFound String
  | ConfigDecodeErr JsonDecodeError
  | IoError JsError
  | ReadGeckoCode JsError
  | WriteGameSettings JsError
  | WriteIniFile String JsError
  | InvalidTexturePath String
  | SymlinkTexturePath JsError
  | CleanupError JsError

instance RtError Error where
  rtErrExtra _ = encodeJson {}
  rtErrName NoIso = "melee iso not found"
  rtErrName (ConfigNotFound _) = "config file not found"
  rtErrName (ConfigDecodeErr _) = "config file invalid type"
  rtErrName (IoError _) = "IO Error"
  rtErrName (ReadGeckoCode _) = "Read Gecko Code"
  rtErrName (WriteGameSettings _) = "Write Game Settings"
  rtErrName (WriteIniFile filename _) = "Write Ini File: " <> filename
  rtErrName (InvalidTexturePath _) = "Invalid Texture Path"
  rtErrName (SymlinkTexturePath _) = "Symlink Texture Path"
  rtErrName (CleanupError _) = "Cleanup Error"
  -- rtErrName _ = "_err_name_not_implemented_"
  rtErrMessage NoIso = "please supply via opt `-i %ISO_PATH%`"
  rtErrMessage (ConfigNotFound p) = p
  rtErrMessage (ConfigDecodeErr e) = show e
  rtErrMessage (IoError e) = jsErrorMessage e
  rtErrMessage (ReadGeckoCode e) = jsErrorMessage e
  rtErrMessage (WriteGameSettings e) = jsErrorMessage e
  rtErrMessage (WriteIniFile _ e) = jsErrorMessage e
  rtErrMessage (InvalidTexturePath s) = "No such directory: " <> s
  rtErrMessage (SymlinkTexturePath e) = jsErrorMessage e
  rtErrMessage (CleanupError e) = jsErrorMessage e
-- rtErrMessage _ = "_err_message_not_implemented_"
