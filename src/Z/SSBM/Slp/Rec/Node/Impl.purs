module Z.SSBM.Slp.Rec.Node.Impl where

import Prelude

import Data.Argonaut.Decode (JsonDecodeError(..)) as Dec
import Data.Foldable (foldlDefault)
import Z as Z
import Z.SSBM.Slp.Port as Port
import Z.Sys.Node.Module as Sys
import Z.Z.Shorthand (type (#>), g_, jOr, jOrE, (>|>), (/\))

data Error = NoIso

instance errorRtError :: Z.RtError Error where
  rtErrExtra _ = Z.encodeJson {}
  rtErrName _ = "melee iso not found"
  rtErrMessage _ = "please supply via opt `-i %ISO_PATH%`"

buildEnv :: forall x. Z.SEA EnvBuildState Error x #> Unit
buildEnv = do
  pure unit

run :: forall x. Array String -> Sys.XNode (Z.EA Error x) Unit
run args = do
  wd <- Sys.wd
  envPaths <- Sys.envPaths "slp-rec" $ Z.Just ""
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
    Sys.decodeTextFile @LauncherSettings' launcherSettingsPath # Z.xTry <#>
      Z.hush
  let isoPath = launcherSettings <#> g_ @"settings.isoPath"
  envState <- flip Z.xRunS buildEnv
    { isoPath: isoPath
    , tempPath: show tmpPath
    , texturePath: Z.Nil
    , iniMods: Z.Nil
    , geckoCodes: Z.Nil
    , geckoEnable: Z.Nil
    , geckoDisable: Z.Nil
    }
  Z.xInfo { isoPath }
  Sys.argParse (cliInfo wd) args \opts -> do
    env <- finalizeEnv envState opts $ show $ wd Sys./ "output.mp4"
    Z.xInfo env

mergeListOps
  :: forall a f. Z.Foldable f => Z.List a -> f (ListOp a) -> Z.List a
mergeListOps l ops = foldlDefault folder l ops
  where
  folder _ Reset = Z.Nil
  folder l' (Cons v) = Z.Cons v l'

mergeMListOps
  :: forall a f. Z.Foldable f => Z.List a -> Z.Maybe (f (ListOp a)) -> Z.List a
mergeMListOps l Z.Nothing = l
mergeMListOps l (Z.Just ops) = mergeListOps l ops

arrMergeListOpts
  :: forall a f. Z.Foldable f => Z.List a -> f (ListOp a) -> Array a
arrMergeListOpts a b = Z.arrFromFoldable $ mergeListOps a b

updateEnv :: EnvBuildState -> RecordConfig -> EnvBuildState
updateEnv st cfg =
  { isoPath: cfg.isoPath >|> st.isoPath
  , tempPath: st.tempPath
  , texturePath: mergeMListOps st.texturePath cfg.texturePath
  , iniMods: mergeMListOps st.iniMods cfg.iniMods
  , geckoCodes: mergeMListOps st.geckoCodes cfg.geckoCodes
  , geckoEnable: mergeMListOps st.geckoEnable cfg.geckoEnables
  , geckoDisable: mergeMListOps st.geckoDisable cfg.geckoDisables
  }

finalizeEnv
  :: forall x. EnvBuildState -> CliOpts -> String -> Z.E Error x #> RecordEnv
finalizeEnv st (CliOpts opts) defaultOutputPath = do
  isoPath <- Z.xOk $ jOrE NoIso st.isoPath
  pure
    { isoPath
    , outputPath: jOr defaultOutputPath opts.outputPath
    , startFrame: opts.startFrame
    , totalFrames: opts.totalFrames
    , recPath: opts.recPath
    , tempPath: jOr st.tempPath opts.tempPath
    , texturePath: arrMergeListOpts st.texturePath opts.texturePath
    , iniMods: arrMergeListOpts st.iniMods opts.iniMods
    , geckoCodes: arrMergeListOpts st.geckoCodes opts.geckoCodes
    , geckoEnable: arrMergeListOpts st.geckoEnable opts.geckoEnables
    , geckoDisable: arrMergeListOpts st.geckoDisable opts.geckoDisables
    , colorOverrides: Z.mapFromFoldable $
        (mergeListOps Z.Nil opts.colorOverrides) <#> Z.unwrap
    , slippiPlaybackBin: "slippi-playback"
    , ffmpegBin: "ffmpeg"
    }

type LauncherSettings' =
  { settings :: { isoPath :: String }
  }

type EnvBuildState =
  { isoPath :: Z.Maybe String
  , tempPath :: String
  , texturePath :: Z.List String
  , iniMods :: Z.List IniMod
  , geckoCodes :: Z.List String
  , geckoEnable :: Z.List String
  , geckoDisable :: Z.List String
  }

type RecordEnv =
  { startFrame :: Z.Maybe Int
  , totalFrames :: Z.Maybe Int
  , outputPath :: String
  , recPath :: String
  , isoPath :: String
  , tempPath :: String
  , texturePath :: Array String
  , iniMods :: Array IniMod
  , geckoCodes :: Array String
  , geckoEnable :: Array String
  , geckoDisable :: Array String
  , colorOverrides :: Z.Map Port.T Int
  , slippiPlaybackBin :: String
  , ffmpegBin :: String
  }

type CfgMany a = Z.Maybe (Array (ListOp a))
type CliMany a = Z.List (ListOp a)

type RecordConfig =
  { isoPath :: Z.Maybe String
  , texturePath :: CfgMany String
  , iniMods :: CfgMany IniMod
  , tempPath :: Z.Maybe String
  , includes :: CfgMany String
  , geckoCodes :: CfgMany String
  , geckoEnables :: CfgMany String
  , geckoDisables :: CfgMany String
  , slippiPlaybackBin :: Z.Maybe String
  , ffmpegBin :: Z.Maybe String
  }

data CliOpts = CliOpts
  { startFrame :: Z.Maybe Int
  , totalFrames :: Z.Maybe Int
  , outputPath :: Z.Maybe String
  , isoPath :: Z.Maybe String
  , texturePath :: CliMany String
  , iniMods :: CliMany IniMod
  , geckoCodes :: CliMany String
  , geckoEnables :: CliMany String
  , geckoDisables :: CliMany String
  , colorOverrides :: CliMany PortCostume
  , tempPath :: Z.Maybe String
  , configPaths :: CliMany String
  , slippiPlaybackBin :: Z.Maybe String
  , ffmpegBin :: Z.Maybe String
  , recPath :: String
  }

type IniFilename = String
type IniProperty = String
type IniValue = String

data IniMod = IniMod IniFilename IniProperty IniValue

derive instance eqIniMod :: Eq IniMod
derive instance ordIniMod :: Ord IniMod

derive instance genericIniMod :: Z.Generic IniMod _

iniModToStr :: IniMod -> String
iniModToStr (IniMod i p v) = i <> ":" <> p <> "=" <> v

iniModOfStr :: String -> Z.Either String IniMod
iniModOfStr s = do
  let csplit = Z.strSplit (Z.Pattern ":") s
  i <- jOrE emsg $ Z.nth csplit 0
  rest <- jOrE emsg $ Z.nth csplit 1
  let rsplit = Z.strSplit (Z.Pattern "=") rest
  p <- jOrE emsg $ Z.nth rsplit 0
  v <- jOrE emsg $ Z.nth rsplit 1
  pure $ IniMod i p v
  where
  emsg = "Expected `$ini:$prop=$val`"

instance decodeJsonIniMod :: Z.DecodeJson IniMod where
  decodeJson x = do
    (Z.baseDecodeJson x <#> iniModOfStr) >>= onEor
    where
    onEor (Z.Right v) = pure v
    onEor (Z.Left msg) = Z.Left $ Dec.TypeMismatch msg

instance encodeJsonIniMod :: Z.EncodeJson IniMod where
  encodeJson x = Z.encodeJson $ iniModToStr x

newtype PortCostume = PortCostume (Port.T Z./\ Int)

derive instance newtypePortCostume :: Z.Newtype PortCostume _

derive instance eqPortCostume :: Eq PortCostume
derive instance ordPortCostume :: Ord PortCostume

derive instance genericPortCostume :: Z.Generic PortCostume _

portCostumeToStr :: PortCostume -> String
portCostumeToStr (PortCostume (p /\ c)) = (show $ Port.asInt p) <> "=" <> show c

portCostumeOfStr :: String -> Z.Either String PortCostume
portCostumeOfStr s = do
  let esplit = Z.strSplit (Z.Pattern "=") s
  p <- jOrE emsg $ Z.nth esplit 0 >>= Z.intFromString
  c <- jOrE emsg $ Z.nth esplit 1 >>= Z.intFromString
  pure $ PortCostume $ (Port.ofInt p) /\ c
  where
  emsg = "Expected `$port:$costume` => `[1|2|3|4]=[1|2|3|4|5|6]"

instance decodeJsonPortCostume :: Z.DecodeJson PortCostume where
  decodeJson x = do
    (Z.baseDecodeJson x <#> portCostumeOfStr) >>= onEor
    where
    onEor (Z.Right v) = pure v
    onEor (Z.Left msg) = Z.Left $ Dec.TypeMismatch msg

instance encodeJsonPortCostume :: Z.EncodeJson PortCostume where
  encodeJson x = Z.encodeJson $ portCostumeToStr x

data ListOp a = Reset | Cons a

instance decodeListOp :: Z.DecodeJson a => Z.DecodeJson (ListOp a) where
  decodeJson x = do
    Z.caseJsonString decodeCons onString x
    where
    onString ":" = pure Reset
    onString _ = decodeCons
    decodeCons = Z.baseDecodeJson x <#> Cons

instance encodeListOp :: Z.EncodeJson a => Z.EncodeJson (ListOp a) where
  encodeJson Reset = Z.encodeJson ":"
  encodeJson (Cons a) = Z.encodeJson a

optJson :: forall @a. Z.DecodeJson a => Z.OptReadM a
optJson = Z.optEitherReader \s -> Z.mapL show $ Z.decode @a ("\"" <> s <> "\"")

optJsonListOp :: forall @a. Z.DecodeJson a => Z.OptReadM (ListOp a)
optJsonListOp = optJson @(ListOp a)

optReadIniMod :: Z.OptReadM IniMod
optReadIniMod = pure $ IniMod "" "" ""

optReadColorOverride :: Z.OptReadM (Port.T Z./\ Int)
optReadColorOverride = pure $ Port.P1 Z./\ 1

cliOpts :: Sys.Path -> Z.OptParser CliOpts
cliOpts wd = map CliOpts $ optsProd
  <$> Z.optStrArgument
    (Z.optMetavar "SLP_FILE" <> Z.optHelp ".slp file to record")
  <*> Z.optional
    ( Z.optOption Z.optInt $
        (Z.optLong "start-frame" <> Z.optShort 's' <> Z.optMetavar "INT")
          <> Z.optHelp
            "First frame to begin recording (default: `GAME_FRAME_START`)"

    )
  <*> Z.optional
    ( Z.optOption Z.optInt $
        (Z.optLong "total-frames" <> Z.optShort 't' <> Z.optMetavar "INT")
          <> Z.optHelp "Total frames to record (default: `all remaining`)"
    )
  <*> Z.optional
    ( Z.optStrOption $
        (Z.optLong "output" <> Z.optShort 'o' <> Z.optMetavar "MP4")
          <> Z.optHelp
            ( "Output file (default: "
                <> show (wd Sys./ "output.mp4")
                <> ")"
            )
    )
  <*> Z.optional
    ( Z.optStrOption $ (Z.optLong "iso" <> Z.optShort 'i' <> Z.optMetavar "ISO")
        <> Z.optHelp
          ( "melee iso file (default: `slippi-launcher config`)"
          )
    )
  <*> Z.optMany
    ( Z.optOption (optJsonListOp @String) $
        (Z.optLong "texture-path" <> Z.optShort 'x' <> Z.optMetavar "DIR")
          <> Z.optHelp "directory with texture overrides"
    )
  <*> Z.optMany
    ( Z.optOption (optJsonListOp @PortCostume) $
        (Z.optLong "port-costume" <> Z.optShort 'p' <> Z.optMetavar "PORTC")
          <> Z.optHelp
            ( "port costume overrides. PORTC => `$port=$costime`"
                <> " => `[1|2|3|4]=[1|2|3|4|5|6]`"
            )
    )
  <*> Z.optMany
    ( Z.optOption (optJsonListOp @IniMod) $
        (Z.optLong "ini-mod" <> Z.optShort 'I' <> Z.optMetavar "INI_MOD")
          <> Z.optHelp
            ( "slippi ini overrides. INI_MOD => `$ini:$prop=$val`"
                <> " => `[Dolphin|GFX|Logger]:$prop=$val"
            )
    )
  <*> Z.optMany
    ( Z.optOption (optJsonListOp @String) $
        (Z.optLong "gecko-code" <> Z.optShort 'g' <> Z.optMetavar "CODE")
          <> Z.optHelp
            "raw string containing code to directly include while recording"
    )
  <*> Z.optMany
    ( Z.optOption (optJsonListOp @String) $
        (Z.optLong "gecko-enable" <> Z.optShort '+' <> Z.optMetavar "NAME")
          <> Z.optHelp "name of gecko codes to force enable"
    )
  <*> Z.optMany
    ( Z.optOption (optJsonListOp @String) $
        (Z.optLong "gecko-disable" <> Z.optShort '_' <> Z.optMetavar "NAME")
          <> Z.optHelp "name of gecko codes to force disable"
    )
  <*> Z.optional
    ( Z.optStrOption $
        (Z.optLong "temp-path" <> Z.optShort 'T' <> Z.optMetavar "DIR")
          <> Z.optHelp "directory with store temporary recording files"
    )
  <*> Z.optMany
    ( Z.optOption (optJsonListOp @String) $
        (Z.optLong "config" <> Z.optShort 'c' <> Z.optMetavar "FILE")
          <> Z.optHelp "config files to source"
    )
  <*> Z.optional
    ( Z.optStrOption $
        (Z.optLong "slippi-playback" <> Z.optShort 'S' <> Z.optMetavar "BIN")
          <> Z.optHelp "slippi-playback binary path"
    )
  <*> Z.optional
    ( Z.optStrOption $
        (Z.optLong "ffmpeg" <> Z.optShort 'F' <> Z.optMetavar "BIN")
          <> Z.optHelp "ffmpeg binary path"
    )
  where
  optsProd a b c d e f g h i j k l m n o =
    { recPath: a
    , startFrame: b
    , totalFrames: c
    , outputPath: d
    , isoPath: e
    , texturePath: f
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

cliInfo :: Sys.Path -> Z.OptParserInfo CliOpts
cliInfo wd = Z.cliInfo (cliOpts wd Z.<**> Z.cliHelper)
  ( Z.cliFullDesc
      <> Z.cliProgDesc "record SLP to MP4"
      <> Z.cliHeader "slp-rec | @dz-ssbm | .slp recording"
  )