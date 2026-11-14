module Test.Scratch where

import Node.Z.Prelude

import Debug (traceM)
import Effect.Class.Console (log)
import Z.Z.Eff.Dict
  ( DictK'Hole(..)
  , _0
  , dict'addDim
  , dict'collect
  , dict'entriesAt
  , dict'lookup
  , dictK'fullEntry
  , eff'dict'entriesAt
  , eff'dict'insert
  , eff'dict'keysAt
  , eff'dict'new
  , eff'dict'sizeAt
  )
import Z.Z.HashMap (hm2d'collect)
import Z.Z.Id (identP'els)
import Z.Z.It (it'forM, it'forM_, it'maximum, it'minimum)
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
import Z.Z.Variant (variant'handle, variant'mkers)

{-
runMain :: Future (Either String (Array Int))
runMain = ctxA:':run $ mapL jsErrorName <$> do
  ctxR'runAt @"hi" "hi" do
    res <- ctxE'runAt @"e" do
      traceM $ it'maximum [ 1, 2, 8, -2, 4, 5 ]
      traceM $ it'minimum [ 1, 2, 8, -2, 4, 5 ]
      names <- it'forM [ 1, 2, 3 ] \id -> do
        prefix <- ctxE'okAt @"e" =<< ctxA:':fromEff (pure "Name")
        let name = prefix <> ":[ " <> show id <> " ]"
        pure name
      traceM $ "DONE!" /\ { names }
      a1 <- ctxE'okAt @"e" =<< ctxA:':fromEff (pure 420)
      a2 <- ctxE'okAt @"e" =<< ctxA:':fromEff (pure 69)
      ctxR'runAt @"hi" 42069 do
        ctxE'failAt @"e" $ jsError' "Hello Darkness"
        hi <- ctxR'askAt @"hi"
        pure [ hi, a1, a2 ]
    currHi <- ctxR'askAt @"hi"
    traceM { currHi }
    pure res
-}

var'unwrap
  :: forall v' v l a
   . IsSymbol l
  => Cons l a v' v
  => Variant v
  -> Proxy l
var'unwrap _ = Proxy @l

type MyVar = Variant (a :: Maybe Int, bc :: Array String)

mkMyVar :: MyVar
mkMyVar = var'inj @"a" (Just 123)

main :: Effect Unit
main = do
  let x = unit
  traceM { x }
  traceM $ encode $ [ Left 1234, Right "asdf" ]
  traceM $ encode $ 1234 /\ "asdf"
  traceM $ encode $ jsError "title" "message"
  traceM $ encode $ dec'custom "hello!"
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
  async'run (log <<< show) (log <<< show) runMain
  -}
  traceM $ arr'collect [ 1, 2, 3 ]
  traceM $ list'collect [ 1, 2, 3 ]
  traceM $ hm2d'collect
    [ "a" /\ 1 /\ "a1"
    , "a" /\ 2 /\ "a2"
    , "a" /\ 3 /\ "a3"
    , "a" /\ 4 /\ "a4"
    , "b" /\ 2 /\ "b2"
    , "b" /\ 3 /\ "b3"
    , "b" /\ 4 /\ "b4"
    , "b" /\ 5 /\ "b5"
    , "b" /\ 6 /\ "b6"
    , "b" /\ 7 /\ "b7"
    ]
  traceM
    [ ("a" /\ 1) /\ "a1"
    , ("a" /\ 2) /\ "a2"
    , ("a" /\ 3) /\ "a3"
    ]
  traceM $ arr'concat $ map (\(a /\ ps) -> map ((/\) a) ps) $ hm'entries
    $ map hm'entries
    $ hm2d'collect
        [ "a" /\ 1 /\ "a1"
        , "a" /\ 2 /\ "a2"
        , "a" /\ 3 /\ "a3"
        , "a" /\ 4 /\ "a4"
        , "b" /\ 2 /\ "b2"
        , "b" /\ 3 /\ "b3"
        , "b" /\ 4 /\ "b4"
        , "b" /\ 5 /\ "b5"
        , "b" /\ 6 /\ "b6"
        , "b" /\ 7 /\ "b7"
        ]
  traceM $ arr'collect $ list'collect [ 1, 2, 3 ]
  traceM $ list'collect $ arr'collect $ list'collect [ 1, 2, 3 ]
  traceM $ arr'collect $ list'collect $ arr'collect $ list'collect [ 1, 2, 3 ]
  traceM $ decode @({ a :: Maybe (Maybe Int) }) $ "{ \"a\": null }"
  traceM $ decode @(Int /\ String) $ "[1234,\"asdf\"]"
  traceM $ decode @JsonDecodeError "{\"inj\":\"hello!\",\"at\":\"custom\"}"
  traceM $ jsErrorName <$> decode @JsError
    "{\"name\":\"title\",\"message\":\"message\",\"_\":\"JsError\"}"
  traceM $ decode @(Array (Either Int String)) $
    "[{\"tag\":\"Left\",\"value\":1234},{\"tag\":\"Right\",\"value\":\"asdf\"}]"
  traceM $ identP'els $ 123
  traceM $ identP'els $ 123 /\ "asf"
  traceM $ identP'els $ 123 /\ "asf" /\ true
  traceM $ identP'els $ 123 /\ "asf" /\ true /\ 1.77
  d <- eff'dict'new @(Int /\ Boolean /\ String) @{ name :: String }
  eff'dict'insert (123 /\ true /\ "asdf") { name: "Jimmy" } d
  eff'dict'insert (123 /\ false /\ "fdsa") { name: "Bob" } d
  eff'dict'insert (123 /\ false /\ "oooo") { name: "Oscar" } d
  eff'dict'insert (123 /\ false /\ "nnnn") { name: "Nick" } d
  eff'dict'insert (321 /\ false /\ "jjjj") { name: "Josh" } d
  eff'dict'insert (555 /\ false /\ "rrrr") { name: "Ron" } d
  eff'dict'insert (666 /\ false /\ "ssss") { name: "Steve" } d
  eff'dict'insert (666 /\ true /\ "ffff") { name: "Frank" } d
  traceM $ "a" /\ "b"
  traceM $ "a" /\ "b" /\ "c"
  traceM $ "a" /\ "b" /\ "c" /\ "d"
  traceM =<< eff'dict'entriesAt (1000023 /\ false /\ _0) d
  traceM =<< eff'dict'entriesAt (123 /\ false /\ _0) d
  traceM =<< eff'dict'sizeAt (100003 /\ false /\ "oooo") d
  traceM =<< eff'dict'sizeAt (123 /\ false /\ "oooo") d
  traceM =<< eff'dict'sizeAt (123 /\ false /\ _0) d
  traceM =<< eff'dict'sizeAt (123 /\ _0) d
  traceM =<< eff'dict'sizeAt (_0) d
  traceM =<< map ((<>) "_:") <$> eff'dict'keysAt (1000023 /\ false /\ _0) d
  traceM =<< map ((<>) "_:") <$> eff'dict'keysAt (123 /\ false /\ _0) d
  traceM =<< map ((==) false) <$> eff'dict'keysAt (123 /\ _0) d
  traceM =<< map ((+) 1) <$> eff'dict'keysAt (_0) d
  let
    d2 = dict'collect
      [ (123 /\ true /\ "oooo") /\ { name: "Oscar" }
      , (123 /\ true /\ "jjjj") /\ { name: "Josh" }
      ]
    dtE = dict'entriesAt (123 /\ _0) d2 <#>
      dictK'fullEntry @(Int /\ DictK'Hole) @(Int /\ Boolean /\ String)
        (123 /\ _0)
  traceM dtE
  traceM $ dict'lookup (123 /\ true /\ "oooo") d2
  -- let d2d = dict'addDim d2
  -- traceM d2d
  traceM $ dict'lookup (12 /\ true /\ "oooo") d2