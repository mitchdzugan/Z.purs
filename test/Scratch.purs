module Test.Scratch where

import Node.Z.Prelude

import Debug (traceM)
import Effect.Class.Console (log)
import Z.Z.It (it'forM, it'forM_)
import Z.Z.Run
  ( Future
  , ctxA'fromEff
  , ctxA'run
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
runMain = ctxA'run $ mapL jsErrorName <$> do
  ctxR'runAt @"hi" "hi" do
    res <- ctxE'runAt @"e" do
      names <- it'forM [ 1, 2, 3 ] \id -> do
        prefix <- ctxE'okAt @"e" =<< ctxA'fromEff (pure "Name")
        let name = prefix <> ":[ " <> show id <> " ]"
        pure name
      traceM $ "DONE!" /\ { names }
      a1 <- ctxE'okAt @"e" =<< ctxA'fromEff (pure 420)
      a2 <- ctxE'okAt @"e" =<< ctxA'fromEff (pure 69)
      ctxR'runAt @"hi" 42069 do
        ctxE'failAt @"e" $ jsError' "Hello Darkness"
        hi <- ctxR'askAt @"hi"
        pure [ hi, a1, a2 ]
    currHi <- ctxR'askAt @"hi"
    traceM { currHi }
    pure res

main :: Effect Unit
main = do
  {-}
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
  -}
  async'run (log <<< show) (log <<< show) runMain