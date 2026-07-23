module Z.Gql.Module
  ( Error
  , Warning
  ) where

import Z.Gql.Error (T) as GqlE
import Z.Gql.Warning (T) as GqlW

type Error = GqlE.T

type Warning = GqlW.T