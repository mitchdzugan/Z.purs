module Z.Gql.Node.Module
  ( module Gql
  , module NodeGql
  ) where

import Z.Gql.Module
  ( Error(..)
  , NetworkControl(..)
  , OpenOpts
  , Opts
  , Warning(..)
  , baseOpts
  ) as Gql
import Z.Gql.Node.Impl
  ( Client
  , Operation
  , defOperation
  , fullOpts
  , mkClient
  , operate
  , operateUnknown
  ) as NodeGql

