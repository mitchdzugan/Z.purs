module Test.Main where

import Prelude

import Debug (traceM)
import Effect (Effect)
import Effect.Class.Console (log)
import Gql.Node as Gql
import Z as Z

main :: Effect Unit
main = do
  let
    apiUrl = "apiUrl"
    authToken = Z.Just "My Auth Token"
    client = Gql.mkClient apiUrl $ Z.s_set Gql._authToken authToken
  traceM client
