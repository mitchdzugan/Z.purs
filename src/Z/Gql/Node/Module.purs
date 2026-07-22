module Z.Gql.Node.Module
  ( module Gql
  , module NodeGql
  ) where

import Z.Gql.Module
  ( Error(..)
  , Warning(..)
  ) as Gql
import Z.Gql.Node.Impl
  ( Client
  , NetworkControl(..)
  , Operation
  , defOperation
  , mkClient
  , operate
  , operateUnknown
  ) as NodeGql

