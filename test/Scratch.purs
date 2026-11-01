module Test.Scratch where

import Node.Z.Prelude

import Debug (traceM)
import Effect.Class.Console (log)
import Z.Z.Run (RunA, ctx'askAt, ctx'attempt, ctx'runAsync, ctx'runReaderAt)
import Z.Z.ThenableR
  ( thenableR'ask
  , thenableR'async'fromEff
  , thenableR'runAsync
  , thenableR'sync
  )

runMain :: RunA () (Array Int)
runMain = do
  a1 <- ctx'attempt $ future'of 420
  a2 <- ctx'attempt $ future'of 69
  ctx'runReaderAt @"hi" 69 do
    hi <- ctx'askAt @"hi"
    pure [ hi, a1, a2 ]

main :: Effect Unit
main = do
  thenableR'runAsync 37 (log <<< show) (log <<< show) do
    a <- do
      a1 <- thenableR'ask
      traceM { a1 }
      a2 <- thenableR'async'fromEff $ pure 1
      pure $ a1 + a2
    b <- pure 2
    c <- pure $ "Result is: " <> show (a + b)
    traceM { a, b, c }
    pure c
  log "before or after"
  future'run (log <<< show) (log <<< show) $ ctx'runAsync runMain