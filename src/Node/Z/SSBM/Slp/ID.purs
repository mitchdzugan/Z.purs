module Node.Z.SSBM.Slp.ID where

import Node.Z.Prelude
import Z.Z.Opt as O

xRun :: forall x. Array String -> EA JsError x ##> Unit
xRun args = do
  xArgParse "slp-id" cliInfo args \(CliOpts opts) -> do
    buffer <- xReadFile opts.filename
    sha <- sha256BytesOfBuffer buffer
    xOut $ key sha
    pure unit

newtype CliOpts = CliOpts { filename :: String }

cliOpts :: O.Parser CliOpts
cliOpts = map CliOpts $ optsProd
  <$> O.strArgument
    (O.metavar "SLP_FILE" <> O.help ".slp file to get the id of")
  where
  optsProd filename = { filename }

cliInfo :: O.ParserInfo CliOpts
cliInfo = O.info (cliOpts O.<**> O.helper) O.fullDesc
