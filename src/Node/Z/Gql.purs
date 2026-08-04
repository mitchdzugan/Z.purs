module Node.Z.Gql
  ( module Gql
  , module NodeGql
  ) where

import Z.Gql.Module
  ( Error
  , Warning
  ) as Gql
import Node.Z.Gql.GqlImpl
  ( Client
  , NetworkControl(..)
  , Operation
  , defOperation
  , mkClient
  , xOperate
  , xOperateUnknown
  ) as NodeGql

