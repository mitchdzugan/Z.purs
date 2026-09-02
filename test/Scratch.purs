module Test.Scratch where

import Node.Z.Prelude

import Heterogeneous.Mapping (class HMap)
import Z.SSBM.Slp.Read.Impl as SlpRead
import Z.Z.Id as Id

testCachePath :: String
testCachePath = "/home/dz/Repo/PS-WS/.cache-path"

type ST'Spec =
  { a :: ST'_'Ref Int
  -- , b :: ST'_'Ref String
  -- , hs :: ST'_'HashSet'2d Int String
  -- , hm :: ST'_'HashMap Boolean String
  }

type ST'Spec' r p = { a :: ST'''Ref r Int p }

type T''' x m = Run ("st" :: m | x) Int

-- type ST'''Ref :: forall k1 k2. (Type -> Type -> k1) -> -> k2 -> k1

type ST'1 spec p =
  (R' (spec ST'''r p /\ Proxy (XST'Def $ ST'Spec' ST'''l p)))

st't1
  :: forall @p x' x
   . IsSymbol p
  => Cons p (ST'1 ST'Spec' p) x' x
  => Run x Int
st't1 = do
  prev <- st'freeze'' @p @"a"
  st'put'' @p @"a" 56
  pure prev

class
  ( IsSymbol p
  , Cons p (R' $ r /\ Proxy (XST'Def spec)) x' x
  , EffAdapter (XST'Def spec) p spec r v
  ) <=
  T'''Spec'' p spec r v x' x
  | spec p x' -> r v x

class T'''Spec'' "st" spec r v x' x <= T'''Spec spec r v x' x | spec x' -> r v x

{-}
st't2 :: forall p r v x' x. T'''Spec'' p ST'Spec r v x' x => Run x Int
st't2 = do
  prev <- st'freeze'' @p @"a"
  st'put'' @p @"a" 56
  pure prev-}

main :: Effect Unit
main = runXAThenExit do
  let
    (st'spec :: ST'Spec) =
      { a: st'Ref 3
      -- , b: st'Ref_ @String
      -- , hs: st'HashSet'2d @Int @String
      -- , hm: st'HashMap @Boolean @String
      }
  res <- st'run st'spec do
    let
      ( mmm
          :: Run
               ( aff :: AffF
               , xBase :: XBaseF
               , xNode :: XNodeF
               , except :: Except JsError
               , writer :: Writer (Array Void)
               , st ::
                   R'
                     ( { a :: Ref'Eff'R Int "st"
                       -- , b :: Ref'Eff'R String "st"
                       -- , hs :: ST'HashSet'2d'R Int String "st"
                       -- , hm :: ST'HashMap'R Boolean String "st"
                       } /\ Proxy (XST'Def ST'Spec)
                     )
               )
               Int
      ) = do
        prev <- st'freeze @"a"
        st'put @"a" 56
        pure prev
    mmm
  xOut { res }
  xOut $ encode
    [ Id.ident'uuid 1
    , Id.ident'uuid 2
    , Id.ident'uuid 0
    , Id.ident'uuid 0.0
    , Id.ident'uuid 0.001
    , Id.ident'uuid 0.002
    , Id.ident'uuid (-1)
    , Id.ident'uuid 99
    , Id.ident'uuid 100
    , Id.ident'uuid 101
    , Id.ident'uuid ""
    , Id.ident'uuid "1"
    , Id.ident'uuid "asdf"
    , Id.ident'uuid true
    , Id.ident'uuid false
    , Id.ident'uuid $ Just 0 ~ Just 0
    , Id.ident'uuid $ Just 0 ~ Nothing
    ]
  xhs'eval @"test" do
    v1 <- xhs'vals @"test"
    xOut v1
    s1 <- xhs'size @"test"
    xhs'add @"test" 123
    v2 <- xhs'vals @"test"
    s2 <- xhs'size @"test"
    xOut { v1, v2, s1, s2 }
  b <- xReadFile "/home/dz/Slippi/Game_20260709T183630.slp"
  parsed <- e'map un' $ SlpRead.xParse b
  xOut $ ident'uuid parsed