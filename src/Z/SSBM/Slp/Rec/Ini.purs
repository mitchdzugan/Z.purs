module Z.SSBM.Slp.Rec.Ini
  ( IniFilename
  , IniMod(..)
  , IniPropGroup
  , IniPropKey
  , IniValue
  , mkInis
  ) where

import Z.Prelude

type IniFilename = String
type IniPropGroup = String
type IniPropKey = String
type IniValue = String

data IniMod = IniMod IniFilename IniPropGroup IniPropKey IniValue

derive instance Eq IniMod
derive instance Ord IniMod

derive instance Generic IniMod _

iniModToStr :: IniMod -> String
iniModToStr (IniMod i pg pk v) = i <> ":" <> pg <> "." <> pk <> "=" <> v

iniModOfStr :: String -> Either String IniMod
iniModOfStr s = do
  let csplit = str'split (Pattern ":") s
  i <- jOrE emsg $ nth csplit 0
  irest <- jOrE emsg $ nth csplit 1
  let pgsplit = str'split (Pattern ".") irest
  pg <- jOrE emsg $ nth pgsplit 0
  pgrest <- jOrE emsg $ nth pgsplit 1
  let rsplit = str'split (Pattern "=") pgrest
  pk <- jOrE emsg $ nth rsplit 0
  v <- jOrE emsg $ nth rsplit 1
  pure $ IniMod i pg pk v
  where
  emsg = "Expected `$ini:$propGroup.$propKey=$val`"

instance DecodeJson IniMod where
  decodeJson x = do
    (baseDecodeJson x <#> iniModOfStr) >>= onEor
    where
    onEor (Right v) = pure v
    onEor (Left msg) = decodeFailTypeMismatch msg

instance EncodeJson IniMod where
  encodeJson x = encodeJson $ iniModToStr x

data IniModGroup = IniModGroup IniFilename IniPropGroup

iniModV :: IniMod -> String
iniModV (IniMod _ _ _ v) = v

iniModGroupWithK :: IniMod -> IniModGroup /\ String
iniModGroupWithK (IniMod fn pg pk _) = IniModGroup fn pg /\ pk

instance Identable IniModGroup where
  ident'get (IniModGroup fn pg) = id'of $ fn <> ":" <> pg

modGroupKey :: IniModGroup -> String /\ String
modGroupKey (IniModGroup fn pg) = fn /\ pg

mkInis
  :: Array IniMod
  -> Object (Object (Object String))
mkInis addedMods = sync'x do
  let mods = arr'concat [ baseInis, addedMods ]
  dataByModGroup <- _'exec_ @(X'HashMap2D IniModGroup String String) do
    forM_ mods \mod -> _'insert (iniModGroupWithK mod) (iniModV mod)
  fileDataByFile <- _'exec_ @(X'HashMap2D String String (Object String)) do
    forM_ (hm'entries dataByModGroup) \(modGroup /\ groupData) -> do
      _'insert (modGroupKey modGroup) $ obj'fromHM groupData
  pure $ obj'fromHM $ fileDataByFile <#> obj'fromHM

baseInis :: Array IniMod
baseInis =
  [ IniMod "Dolphin" "General" "ShowLag" "False"
  , IniMod "Dolphin" "General" "ShowFrameCount" "False"
  , IniMod "Dolphin" "General" "ISOPaths" "2"
  , IniMod "Dolphin" "General" "RecursiveISOPaths" "False"
  , IniMod "Dolphin" "General" "NANDRootPath" ""
  , IniMod "Dolphin" "General" "DumpPath" ""
  , IniMod "Dolphin" "General" "WirelessMac" ""
  , IniMod "Dolphin" "General" "WiiSDCardP" ""
  --
  , IniMod "Dolphin" "Interface" "ConfirmStop" "False"
  , IniMod "Dolphin" "Interface" "UsePanicHandlers" "False"
  , IniMod "Dolphin" "Interface" "OnScreenDisplayMessages" "False"
  , IniMod "Dolphin" "Interface" "HideCursor" "True"
  , IniMod "Dolphin" "Interface" "AutoHideCursor" "False"
  , IniMod "Dolphin" "Interface" "LanguageCode" ""
  , IniMod "Dolphin" "Interface" "ShowToolbar" "True"
  , IniMod "Dolphin" "Interface" "ShowStatusbar" "True"
  , IniMod "Dolphin" "Interface" "ShowSeekbar" "True"
  , IniMod "Dolphin" "Interface" "ShowLogWindow" "False"
  , IniMod "Dolphin" "Interface" "ShowLogConfigWindow" "False"
  , IniMod "Dolphin" "Interface" "ExtendedFPSInfo" "False"
  , IniMod "Dolphin" "Interface" "ThemeName" "Clean Blue"
  , IniMod "Dolphin" "Interface" "PauseOnFocusLost" "False"
  , IniMod "Dolphin" "Interface" "DisableTooltips" "False"
  --
  , IniMod "Dolphin" "Display" "FullscreenResolution" "Auto"
  , IniMod "Dolphin" "Display" "Fullscreen" "False"
  , IniMod "Dolphin" "Display" "RenderToMain" "False"
  , IniMod "Dolphin" "Display" "RenderWindowAutoSize" "True"
  , IniMod "Dolphin" "Display" "KeepWindowOnTop" "False"
  , IniMod "Dolphin" "Display" "ProgressiveScan" "False"
  , IniMod "Dolphin" "Display" "PAL60" "True"
  , IniMod "Dolphin" "Display" "DisableScreenSaver" "True"
  , IniMod "Dolphin" "Display" "ForceNTSCJ" "False"
  --
  , IniMod "Dolphin" "GameList" "ListDrives" "False"
  , IniMod "Dolphin" "GameList" "ListWad" "True"
  , IniMod "Dolphin" "GameList" "ListElfDol" "True"
  , IniMod "Dolphin" "GameList" "ListWii" "True"
  , IniMod "Dolphin" "GameList" "ListGC" "True"
  , IniMod "Dolphin" "GameList" "ListJap" "True"
  , IniMod "Dolphin" "GameList" "ListPal" "True"
  , IniMod "Dolphin" "GameList" "ListUsa" "True"
  , IniMod "Dolphin" "GameList" "ListAustralia" "True"
  , IniMod "Dolphin" "GameList" "ListFrance" "True"
  , IniMod "Dolphin" "GameList" "ListGermany" "True"
  , IniMod "Dolphin" "GameList" "ListItaly" "True"
  , IniMod "Dolphin" "GameList" "ListKorea" "True"
  , IniMod "Dolphin" "GameList" "ListNetherlands" "True"
  , IniMod "Dolphin" "GameList" "ListRussia" "True"
  , IniMod "Dolphin" "GameList" "ListSpain" "True"
  , IniMod "Dolphin" "GameList" "ListTaiwan" "True"
  , IniMod "Dolphin" "GameList" "ListWorld" "True"
  , IniMod "Dolphin" "GameList" "ListUnknown" "True"
  , IniMod "Dolphin" "GameList" "ListSort" "3"
  , IniMod "Dolphin" "GameList" "ListSortSecondary" "0"
  , IniMod "Dolphin" "GameList" "ColorCompressed" "True"
  , IniMod "Dolphin" "GameList" "ColumnPlatform" "True"
  , IniMod "Dolphin" "GameList" "ColumnBanner" "True"
  , IniMod "Dolphin" "GameList" "ColumnNotes" "True"
  , IniMod "Dolphin" "GameList" "ColumnFileName" "True"
  , IniMod "Dolphin" "GameList" "ColumnID" "True"
  , IniMod "Dolphin" "GameList" "ColumnRegion" "True"
  , IniMod "Dolphin" "GameList" "ColumnSize" "True"
  , IniMod "Dolphin" "GameList" "ColumnState" "False"
  --
  , IniMod "Dolphin" "Core" "HLE_BS2" "False"
  , IniMod "Dolphin" "Core" "TimingVariance" "8"
  , IniMod "Dolphin" "Core" "CPUCore" "1"
  , IniMod "Dolphin" "Core" "Fastmem" "True"
  , IniMod "Dolphin" "Core" "CPUThread" "True"
  , IniMod "Dolphin" "Core" "DSPHLE" "True"
  , IniMod "Dolphin" "Core" "SyncOnSkipIdle" "True"
  , IniMod "Dolphin" "Core" "SyncGPU" "False"
  , IniMod "Dolphin" "Core" "SyncGpuMaxDistance" "200000"
  , IniMod "Dolphin" "Core" "SyncGpuMinDistance" "-200000"
  , IniMod "Dolphin" "Core" "SyncGpuOverclock" "1.00000000"
  , IniMod "Dolphin" "Core" "FPRF" "False"
  , IniMod "Dolphin" "Core" "AccurateNaNs" "False"
  , IniMod "Dolphin" "Core" "DefaultISO" ""
  , IniMod "Dolphin" "Core" "BootDefaultISO" "False"
  , IniMod "Dolphin" "Core" "DVDRoot" ""
  , IniMod "Dolphin" "Core" "Apploader" ""
  , IniMod "Dolphin" "Core" "SelectedLanguage" "0"
  , IniMod "Dolphin" "Core" "OverrideGCLang" "False"
  , IniMod "Dolphin" "Core" "DPL2Decoder" "False"
  , IniMod "Dolphin" "Core" "TimeStretching" "False"
  , IniMod "Dolphin" "Core" "RSHACK" "False"
  , IniMod "Dolphin" "Core" "Latency" "0"
  , IniMod "Dolphin" "Core" "ReduceTimingDispersion" "False"
  , IniMod "Dolphin" "Core" "SlippiOnlineDelay" "2"
  , IniMod "Dolphin" "Core" "SlippiEnableSpectator" "True"
  , IniMod "Dolphin" "Core" "SlippiSpectatorLocalPort" "51441"
  , IniMod "Dolphin" "Core" "SlippiSaveReplays" "True"
  , IniMod "Dolphin" "Core" "SlippiEnableQuickChat" "0"
  , IniMod "Dolphin" "Core" "SlippiForceNetplayPort" "False"
  , IniMod "Dolphin" "Core" "SlippiNetplayPort" "2626"
  , IniMod "Dolphin" "Core" "SlippiForceLanIp" "False"
  , IniMod "Dolphin" "Core" "SlippiLanIp" ""
  , IniMod "Dolphin" "Core" "SlippiReplayMonthFolders" "False"
  , IniMod "Dolphin" "Core" "SlippiPlaybackDisplayFrameIndex" "False"
  , IniMod "Dolphin" "Core" "BlockingPipes" "False"
  , IniMod "Dolphin" "Core" "AgpCartAPath" ""
  , IniMod "Dolphin" "Core" "AgpCartBPath" ""
  , IniMod "Dolphin" "Core" "SlotA" "255"
  , IniMod "Dolphin" "Core" "SerialPort1" "255"
  , IniMod "Dolphin" "Core" "BBA_MAC" ""
  , IniMod "Dolphin" "Core" "SIDevice0" "12"
  , IniMod "Dolphin" "Core" "AdapterRumble0" "True"
  , IniMod "Dolphin" "Core" "SimulateKonga0" "False"
  , IniMod "Dolphin" "Core" "SIDevice1" "12"
  , IniMod "Dolphin" "Core" "AdapterRumble1" "True"
  , IniMod "Dolphin" "Core" "SimulateKonga1" "False"
  , IniMod "Dolphin" "Core" "SIDevice2" "12"
  , IniMod "Dolphin" "Core" "AdapterRumble2" "True"
  , IniMod "Dolphin" "Core" "SimulateKonga2" "False"
  , IniMod "Dolphin" "Core" "SIDevice3" "12"
  , IniMod "Dolphin" "Core" "AdapterRumble3" "True"
  , IniMod "Dolphin" "Core" "SimulateKonga3" "False"
  , IniMod "Dolphin" "Core" "WiiSDCard" "False"
  , IniMod "Dolphin" "Core" "WiiKeyboard" "False"
  , IniMod "Dolphin" "Core" "WiimoteContinuousScanning" "False"
  , IniMod "Dolphin" "Core" "WiimoteEnableSpeaker" "False"
  , IniMod "Dolphin" "Core" "RunCompareServer" "False"
  , IniMod "Dolphin" "Core" "RunCompareClient" "False"
  , IniMod "Dolphin" "Core" "EmulationSpeed" "1.00000000"
  , IniMod "Dolphin" "Core" "FrameSkip" "0x00000000"
  , IniMod "Dolphin" "Core" "Overclock" "1.00000000"
  , IniMod "Dolphin" "Core" "OverclockEnable" "False"
  , IniMod "Dolphin" "Core" "GFXBackend" "DX11"
  , IniMod "Dolphin" "Core" "GPUDeterminismMode" "auto"
  , IniMod "Dolphin" "Core" "PerfMapDir" ""
  , IniMod "Dolphin" "Core" "EnableCustomRTC" "False"
  , IniMod "Dolphin" "Core" "CustomRTCValue" "0x386d4380"
  , IniMod "Dolphin" "Core" "AllowAllNetplayVersions" "False"
  , IniMod "Dolphin" "Core" "QoSEnabled" "True"
  , IniMod "Dolphin" "Core" "AdapterWarning" "True"
  , IniMod "Dolphin" "Core" "ShownLagReductionWarning" "False"
  --
  , IniMod "Dolphin" "Movie" "PauseMovie" "False"
  , IniMod "Dolphin" "Movie" "Author" ""
  , IniMod "Dolphin" "Movie" "DumpFrames" "True"
  , IniMod "Dolphin" "Movie" "DumpFramesSilent" "False"
  , IniMod "Dolphin" "Movie" "ShowInputDisplay" "False"
  , IniMod "Dolphin" "Movie" "ShowRTC" "False"
  --
  , IniMod "Dolphin" "DSP" "EnableJIT" "True"
  , IniMod "Dolphin" "DSP" "DumpAudio" "True"
  , IniMod "Dolphin" "DSP" "DumpAudioSilent" "False"
  , IniMod "Dolphin" "DSP" "DumpUCode" "False"
  , IniMod "Dolphin" "DSP" "Backend" "No audio output"
  , IniMod "Dolphin" "DSP" "Volume" "26"
  , IniMod "Dolphin" "DSP" "CaptureLog" "False"
  --
  , IniMod "Dolphin" "Input" "BackgroundInput" "False"
  --
  , IniMod "Dolphin" "FifoPlayer" "LoopReplay" "True"
  --
  , IniMod "Dolphin" "Analytics" "ID" "f21af0d6e773dd537a188b6da4530e81"
  , IniMod "Dolphin" "Analytics" "Enabled" "False"
  , IniMod "Dolphin" "Analytics" "PermissionAsked" "True"
  --
  , IniMod "Dolphin" "Network" "SSLDumpRead" "False"
  , IniMod "Dolphin" "Network" "SSLDumpWrite" "False"
  , IniMod "Dolphin" "Network" "SSLVerifyCert" "False"
  , IniMod "Dolphin" "Network" "SSLDumpRootCA" "False"
  , IniMod "Dolphin" "Network" "SSLDumpPeerCert" "False"
  --
  , IniMod "Dolphin" "BluetoothPassthrough" "Enabled" "False"
  , IniMod "Dolphin" "BluetoothPassthrough" "VID" "-1"
  , IniMod "Dolphin" "BluetoothPassthrough" "PID" "-1"
  , IniMod "Dolphin" "BluetoothPassthrough" "LinkKeys" ""
  --
  , IniMod "Dolphin" "Sysconf" "SensorBarPosition" "1"
  , IniMod "Dolphin" "Sysconf" "SensorBarSensitivity" "50331648"
  , IniMod "Dolphin" "Sysconf" "SpeakerVolume" "88"
  , IniMod "Dolphin" "Sysconf" "WiimoteMotor" "True"
  , IniMod "Dolphin" "Sysconf" "WiiLanguage" "1"
  , IniMod "Dolphin" "Sysconf" "AspectRatio" "1"
  , IniMod "Dolphin" "Sysconf" "Screensaver" "0"
  --
  , IniMod "GFX" "Hardware" "VSync" "False"
  , IniMod "GFX" "Hardware" "Adapter" "0"
  --
  , IniMod "GFX" "Settings" "AspectRatio" "5"
  , IniMod "GFX" "Settings" "Crop" "False"
  , IniMod "GFX" "Settings" "wideScreenHack" "False"
  , IniMod "GFX" "Settings" "UseXFB" "False"
  , IniMod "GFX" "Settings" "UseRealXFB" "False"
  , IniMod "GFX" "Settings" "SafeTextureCacheColorSamples" "128"
  , IniMod "GFX" "Settings" "ShowFPS" "False"
  , IniMod "GFX" "Settings" "ShowNetPlayPing" "False"
  , IniMod "GFX" "Settings" "ShowNetPlayMessages" "False"
  , IniMod "GFX" "Settings" "ShowOSDClock" "False"
  , IniMod "GFX" "Settings" "ShowFrameTimes" "False"
  , IniMod "GFX" "Settings" "LogRenderTimeToFile" "False"
  , IniMod "GFX" "Settings" "ShowInputDisplay" "False"
  , IniMod "GFX" "Settings" "OverlayStats" "False"
  , IniMod "GFX" "Settings" "OverlayProjStats" "False"
  , IniMod "GFX" "Settings" "DumpTextures" "True"
  , IniMod "GFX" "Settings" "DumpVertexLoader" "False"
  , IniMod "GFX" "Settings" "HiresTextures" "True"
  , IniMod "GFX" "Settings" "HiresMaterialMaps" "False"
  , IniMod "GFX" "Settings" "HiresMaterialMapsBuild" "False"
  , IniMod "GFX" "Settings" "ConvertHiresTextures" "False"
  , IniMod "GFX" "Settings" "CacheHiresTextures" "False"
  , IniMod "GFX" "Settings" "DumpEFBTarget" "False"
  , IniMod "GFX" "Settings" "DumpFramesAsImages" "False"
  , IniMod "GFX" "Settings" "FreeLook" "False"
  , IniMod "GFX" "Settings" "InternalResolutionFrameDumps" "True"
  , IniMod "GFX" "Settings" "CompileShaderOnStartup" "True"
  , IniMod "GFX" "Settings" "UseFFV1" "False"
  , IniMod "GFX" "Settings" "DumpFormat" "avi"
  , IniMod "GFX" "Settings" "DumpCodec" ""
  , IniMod "GFX" "Settings" "DumpPath" ""
  , IniMod "GFX" "Settings" "BitrateKbps" "1000000"
  , IniMod "GFX" "Settings" "EnablePixelLighting" "False"
  , IniMod "GFX" "Settings" "ForcedLighting" "False"
  , IniMod "GFX" "Settings" "ForcePhongShading" "False"
  , IniMod "GFX" "Settings" "RimPower" "80"
  , IniMod "GFX" "Settings" "RimIntesity" "0"
  , IniMod "GFX" "Settings" "RimBase" "10"
  , IniMod "GFX" "Settings" "SpecularMultiplier" "255"
  , IniMod "GFX" "Settings" "SimBumpEnabled" "False"
  , IniMod "GFX" "Settings" "SimBumpStrength" "0"
  , IniMod "GFX" "Settings" "SimBumpDetailFrequency" "128"
  , IniMod "GFX" "Settings" "SimBumpThreshold" "16"
  , IniMod "GFX" "Settings" "SimBumpDetailBlend" "16"
  , IniMod "GFX" "Settings" "FastDepthCalc" "True"
  , IniMod "GFX" "Settings" "MSAA" "2"
  , IniMod "GFX" "Settings" "SSAA" "True"
  , IniMod "GFX" "Settings" "EFBScale" "1"
  , IniMod "GFX" "Settings" "TexFmtOverlayEnable" "False"
  , IniMod "GFX" "Settings" "TexFmtOverlayCenter" "False"
  , IniMod "GFX" "Settings" "Wireframe" "False"
  , IniMod "GFX" "Settings" "DisableFog" "False"
  , IniMod "GFX" "Settings" "EnableOpenCL" "False"
  , IniMod "GFX" "Settings" "BorderlessFullscreen" "False"
  , IniMod "GFX" "Settings" "SWZComploc" "True"
  , IniMod "GFX" "Settings" "SWZFreeze" "True"
  , IniMod "GFX" "Settings" "SWDumpObjects" "False"
  , IniMod "GFX" "Settings" "SWDumpTevStages" "False"
  , IniMod "GFX" "Settings" "SWDumpTevTexFetches" "False"
  , IniMod "GFX" "Settings" "SWDrawStart" "0"
  , IniMod "GFX" "Settings" "SWDrawEnd" "100000"
  , IniMod "GFX" "Settings" "EnableValidationLayer" "False"
  , IniMod "GFX" "Settings" "BackendMultithreading" "True"
  , IniMod "GFX" "Settings" "CommandBufferExecuteInterval" "100"
  --
  , IniMod "GFX" "Enhancements" "ForceFiltering" "False"
  , IniMod "GFX" "Enhancements" "DisableFiltering" "False"
  , IniMod "GFX" "Enhancements" "MaxAnisotropy" "3"
  , IniMod "GFX" "Enhancements" "PostProcessingEnable" "False"
  , IniMod "GFX" "Enhancements" "PostProcessingTrigger" "0"
  , IniMod "GFX" "Enhancements" "PostProcessingShaders" ""
  , IniMod "GFX" "Enhancements" "ScalingShader" ""
  , IniMod "GFX" "Enhancements" "UseScalingFilter" "True"
  , IniMod "GFX" "Enhancements" "TextureScalingType" "0"
  , IniMod "GFX" "Enhancements" "TextureScalingFactor" "2"
  , IniMod "GFX" "Enhancements" "UseDePosterize" "True"
  , IniMod "GFX" "Enhancements" "Tessellation" "False"
  , IniMod "GFX" "Enhancements" "TessellationEarlyCulling" "False"
  , IniMod "GFX" "Enhancements" "TessellationDistance" "0"
  , IniMod "GFX" "Enhancements" "TessellationMax" "6"
  , IniMod "GFX" "Enhancements" "TessellationRoundingIntensity" "0"
  , IniMod "GFX" "Enhancements" "TessellationDisplacementIntensity" "0"
  , IniMod "GFX" "Enhancements" "ForceTrueColor" "False"
  --
  , IniMod "GFX" "Stereoscopy" "StereoMode" "0"
  , IniMod "GFX" "Stereoscopy" "StereoDepth" "20"
  , IniMod "GFX" "Stereoscopy" "StereoConvergencePercentage" "100"
  , IniMod "GFX" "Stereoscopy" "StereoSwapEyes" "False"
  , IniMod "GFX" "Stereoscopy" "StereoShader" "Anaglyph/dubois"
  --
  , IniMod "GFX" "Hacks" "EFBAccessEnable" "False"
  , IniMod "GFX" "Hacks" "EFBFastAccess" "False"
  , IniMod "GFX" "Hacks" "ForceProgressive" "True"
  , IniMod "GFX" "Hacks" "EFBToTextureEnable" "True"
  , IniMod "GFX" "Hacks" "EFBScaledCopy" "True"
  , IniMod "GFX" "Hacks" "EFBEmulateFormatChanges" "False"
  , IniMod "GFX" "Hacks" "ForceDualSourceBlend" "False"
  , IniMod "GFX" "Hacks" "FullAsyncShaderCompilation" "True"
  , IniMod "GFX" "Hacks" "WaitForShaderCompilation" "False"
  , IniMod "GFX" "Hacks" "EnableGPUTextureDecoding" "False"
  , IniMod "GFX" "Hacks" "EnableComputeTextureEncoding" "False"
  , IniMod "GFX" "Hacks" "PredictiveFifo" "False"
  , IniMod "GFX" "Hacks" "BoundingBoxMode" "0"
  , IniMod "GFX" "Hacks" "LastStoryEFBToRam" "False"
  , IniMod "GFX" "Hacks" "ForceLogicOpBlend" "False"
  , IniMod "GFX" "Hacks" "VertexRounding" "False"
  --
  , IniMod "Logger" "LogWindow" "x" "400"
  , IniMod "Logger" "LogWindow" "y" "600"
  , IniMod "Logger" "LogWindow" "pos" "2"
  --
  , IniMod "Logger" "Options" "Font" "0"
  , IniMod "Logger" "Options" "WrapLines" "False"
  ]
