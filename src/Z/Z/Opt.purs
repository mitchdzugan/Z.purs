module Z.Z.Opt (module OptionsApplicative) where

import Prelude

import Options.Applicative
  ( (<**>)
  , Parser
  , ParserInfo
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
  ) as OptionsApplicative