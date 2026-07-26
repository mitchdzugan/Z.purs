module Z.SSBM.Slp.Rec.Node.Impl where

import Prelude

import Data.Array (replicate)
import Data.Foldable (sequence_)
import Data.Generic.Rep (class Generic)
import Data.List.Types as L
import Data.Maybe (optional)
import Data.Show.Generic (genericShow)
import Options.Applicative (Parser, ParserInfo, ParserResult, ReadM, argument, execParser, execParserPure, fullDesc, header, help, helper, info, int, long, many, metavar, option, prefs, progDesc, short, showDefault, some, str, strArgument, strOption, switch, value, (<**>))
import Options.Applicative.Builder (PrefsMod(..))
import Z as Z
import Z.SSBM.Slp.Port as Port
import Z.Sys.Node.Module as Sys
import Z.Z.Shorthand (g_)

data Error = NoIso

instance errorRtError :: Z.RtError Error where
  rtErrExtra _ = Z.encodeJson {}
  rtErrName _ = "melee iso not found"
  rtErrMessage _ = "please supply via opt `-i %ISO_PATH%`"

run :: forall x. Array String -> Sys.XNode (Z.EA Error x) Unit
run args = do
  wd <- Sys.wd
  envPaths <- Sys.envPaths "slp-rec" $ Z.Just ""
  platform <- Sys.platform
  let cfgPath = Sys.envCfg envPaths
  let tmpPath = Sys.envTmp envPaths
  let dataPath = Sys.envData envPaths
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
  Z.xInfo { isoPath }
  Sys.argParse (cliInfo wd) args \o -> do
    Z.xInfo { o }

type LauncherSettings' =
  { settings :: { isoPath :: String }
  }

type EnvBuildState =
  { eOrIsoPath :: Z.Either Error String
  , tempPath :: String
  , texturePath :: String
  , iniMods :: Array IniMod
  , geckoCodesToInject :: Array String
  , geckoEnable :: Array String
  , geckoDisable :: Array String
  }

type RecordEnv =
  { startFrame :: Int
  , totalFrames :: Int
  , outputPath :: String
  , recPath :: String
  , isoPath :: String
  , tempPath :: String
  , texturePath :: String
  , iniMods :: Array IniMod
  , geckoCodesToInject :: Array String
  , geckoEnable :: Array String
  , geckoDisable :: Array String
  , colorOverrides :: Z.Map Port.T Int
  , slippiPlaybackBin :: String
  , ffmpegBin :: String
  }

type RecordConfig =
  { isoPath :: Z.Maybe String
  , texturePath :: Z.Maybe String
  , iniMods :: Z.Maybe String
  , tempPath :: Z.Maybe String
  , includes :: Z.Maybe (Array String)
  , geckoCodes :: Z.Maybe (Array String)
  , geckoEnables :: Z.Maybe (Array String)
  , geckoDisables :: Z.Maybe (Array String)
  , slippiPlaybackBin :: Z.Maybe String
  , ffmpegBin :: Z.Maybe String
  }

data CliOpts = CliOpts
  { startFrame :: Z.Maybe Int
  , totalFrames :: Z.Maybe Int
  , outputPath :: Z.Maybe String
  , isoPath :: Z.Maybe String
  , texturePath :: Z.Maybe String
  , iniMods :: Z.List IniMod
  , geckoCodesToInject :: Z.List String
  , geckoEnables :: Z.List String
  , geckoDisables :: Z.List String
  , colorOverrides :: Z.List (Port.T Z./\ Int)
  , tempPath :: Z.Maybe String
  , configPaths :: Z.List String
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

instance decodeJsonIniMod :: Z.DecodeJson IniMod where
  decodeJson x = Z.genericDecodeJson x

instance encodeJsonIniMod :: Z.EncodeJson IniMod where
  encodeJson x = Z.genericEncodeJson x

optReadIniMod :: ReadM IniMod
optReadIniMod = pure $ IniMod "" "" ""

optReadColorOverride :: ReadM (Port.T Z./\ Int)
optReadColorOverride = pure $ Port.P1 Z./\ 1

cliOpts :: Sys.Path -> Parser CliOpts
cliOpts wd = map CliOpts $ optsProd
  <$> strArgument (metavar "SLP_FILE" <> help ".slp file to record")
  <*> optional
    ( option int $ (long "start-frame" <> short 's' <> metavar "INT")
        <> help "First frame to begin recording (default: `GAME_FRAME_START`)"

    )
  <*> optional
    ( option int $ (long "total-frames" <> short 't' <> metavar "INT")
        <> help "Total frames to record (default: `all remaining`)"
    )
  <*> optional
    ( strOption $ (long "output" <> short 'o' <> metavar "MP4")
        <> help
          ( "Output file (default: "
              <> show (wd Sys./ "output.mp4")
              <> ")"
          )
    )
  <*> optional
    ( strOption $ (long "iso" <> short 'i' <> metavar "ISO")
        <> help
          ( "melee iso file (default: `slippi-launcher config`)"
          )
    )
  <*> optional
    ( strOption $ (long "texture-path" <> short 'x' <> metavar "DIR")
        <> help "directory with texture overrides"
    )
  <*> many
    ( option optReadColorOverride $
        (long "port-costume" <> short 'p' <> metavar "PORTC")
          <> help
            ( "port costume overrides. PORTC => `$port=$costime`"
                <> " => `[1|2|3|4]=[1|2|3|4|5|6]`"
            )
    )
  <*> many
    ( option optReadIniMod $ (long "ini-mod" <> short 'I' <> metavar "INI_MOD")
        <> help
          ( "slippi ini overrides. INI_MOD => `$ini:$prop=$val`"
              <> " => `[Dolphin|GFX|Logger]:$prop=$val"
          )
    )
  <*> many
    ( strOption $
        (long "gecko-code" <> short 'g' <> metavar "CODE")
          <> help
            "raw string containing code to directly include while recording"
    )
  <*> many
    ( strOption $ (long "gecko-enable" <> short '+' <> metavar "NAME")
        <> help "name of gecko codes to force enable"
    )
  <*> many
    ( strOption $ (long "gecko-disable" <> short '_' <> metavar "NAME")
        <> help "name of gecko codes to force disable"
    )
  <*> optional
    ( strOption $ (long "temp-path" <> short 'T' <> metavar "DIR")
        <> help "directory with store temporary recording files"
    )
  <*> many
    ( strOption $ (long "config" <> short 'c' <> metavar "FILE")
        <> help "config files to source"
    )
  <*> optional
    ( strOption $ (long "slippi-playback" <> short 'S' <> metavar "BIN")
        <> help "slippi-playback binary path"
    )
  <*> optional
    ( strOption $ (long "ffmpeg" <> short 'F' <> metavar "BIN")
        <> help "ffmpeg binary path"
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
    , geckoCodesToInject: i
    , geckoEnables: j
    , geckoDisables: k
    , tempPath: l
    , configPaths: m
    , slippiPlaybackBin: n
    , ffmpegBin: o
    }

cliInfo :: Sys.Path -> ParserInfo CliOpts
cliInfo wd = info (cliOpts wd <**> helper)
  ( fullDesc
      <> progDesc "record SLP to MP4"
      <> header "slp-rec | @dz-ssbm | .slp recording"
  )