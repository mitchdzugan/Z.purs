module Node.Z.SSBM.Slp.ID where

import Node.Z.Prelude

import Z.SSBM.Slp.Read.Impl as SlpRead
import Z.Z.Opt as O

xRun :: forall x. Array String -> EA JsError x ##> Unit
xRun args = do
  xArgParse "slp-id" cliInfo args \(CliOpts opts) -> do
    buffer <- xReadFile opts.filename
    parsed <- e'map un' $ SlpRead.xParse buffer
    xOut $ ident'key parsed

newtype CliOpts = CliOpts { filename :: String }

cliOpts :: O.Parser CliOpts
cliOpts = map CliOpts $ (\filename -> { filename }) <$> O.strArgument
  (O.metavar "SLP_FILE" <> O.help ".slp file to get the id of")

cliInfo :: O.ParserInfo CliOpts
cliInfo = O.info (cliOpts O.<**> O.helper) O.fullDesc
