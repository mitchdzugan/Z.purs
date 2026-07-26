module Z.Sys.Node.Opt
  ( argParse
  ) where

import Prelude

import Data.Array (replicate)
import Data.Foldable (sequence_)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Options.Applicative (Parser, ParserInfo, ParserResult(..), defaultPrefs, execParser, execParserPure, fullDesc, header, help, helper, info, int, long, metavar, option, prefs, progDesc, renderFailure, short, showDefault, strOption, switch, value, (<**>))
import Options.Applicative.Builder (PrefsMod(..))
import Z as Z
import Z.Sys.Node.Impl as Sys

argParse
  :: forall x a
   . ParserInfo a
  -> Z.Maybe (Array String)
  -> (a -> Sys.XNode x Unit)
  -> Sys.XNode x Unit
argParse opts argm fm =
  args argm >>= handleParse <<< execParserPure defaultPrefs opts
  where
  args (Z.Just a) = pure a
  args _ = Sys.argv
  handleParse (Success a) = fm a
  handleParse (Failure f) = do
    let msg Z./\ _exit = renderFailure f "slp-rec"
    Z.xOutErr msg
    pure unit
  handleParse _ = pure unit
