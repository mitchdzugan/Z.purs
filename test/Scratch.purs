module Test.Scratch where

import Node.Z.Prelude

import Debug (traceM)
import Effect.Class.Console (log)
import Z.Z.Run
  ( Future
  , ctxE'failAt
  , ctxE'okAt
  , ctxE'runAt
  , ctxR'askAt
  , ctxR'runAt
  )
import Z.Z.ThenableR
  ( async'fromEff
  , async'run
  , thenableR'ask
  , thenableR'runAsync
  )

runMain :: Future (Either String (Array Int))
runMain = mapL jsErrorName <$> ctxE'runAt @"e" do
  a1 <- ctxE'okAt @"e" =<< async'fromEff (pure 420)
  a2 <- ctxE'okAt @"e" =<< async'fromEff (pure 69)
  ctxE'failAt @"e" $ jsError' "Hello Darkness"
  ctxR'runAt @"hi" 42069 do
    hi <- ctxR'askAt @"hi"
    pure [ hi, a1, a2 ]

main :: Effect Unit
main = do
  thenableR'runAsync 37 (log <<< show) (log <<< show) do
    a <- do
      a1 <- thenableR'ask
      traceM { a1 }
      a2 <- pure 1 -- async'fromEff $ pure 1
      pure $ a1 + a2
    b <- pure 2
    c <- pure $ "Result is: " <> show (a + b)
    traceM { a, b, c }
    pure c
  log "before or after"
  async'run (log <<< show) (log <<< show) runMain