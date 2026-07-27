module Z.Z.Opt (module OptionsApplicative) where

import Options.Applicative
  ( (<**>)
  , Parser
  , ParserInfo
  , ParserResult(..)
  , ReadM
  , fullDesc
  , header
  , help
  , helper
  , info
  , int
  , long
  , many
  , metavar
  , option
  , progDesc
  , short
  , strArgument
  , strOption
  , eitherReader
  , briefDesc
  , footer
  , defaultPrefs
  , execParserPure
  , renderFailure
  ) as OptionsApplicative
