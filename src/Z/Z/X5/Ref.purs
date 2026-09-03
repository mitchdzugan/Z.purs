module Z.Z.X5.Ref where

import Z.Z.X5.Core
import Z.Z.X5.UtilPrelude

import Z.Z.Core (class ConsSymbol, rec'get, rec'insert, rec'set)

foreign import data Ref'ST :: forall k. k -> Type

foreign import js_ref_new :: forall t. t -> Effect (Ref'ST t)
foreign import js_ref_get :: forall t. Ref'ST t -> Effect t
foreign import js_ref_set :: forall t. Unit -> t -> Ref'ST t -> Effect Unit

type Ref'R :: forall k. Type -> k -> Type
type Ref'R t p =
  { get :: Eff'At p t
  , set :: t -> Eff'At p Unit
  }

foreign import data Ref'T :: forall k. k -> Type

instance X'Runs'R'Adapted p (Ref'T t) t (Ref'R t p) t where
  x'runs'r'adapted'init init = do
    st <- js_ref_new init
    let get = eff'tag @p $ js_ref_get st
    let set = \v -> eff'tag @p $ js_ref_set unit v st
    pure { get, set }
  x'runs'r'adapted'complete = do
    r'doAsked @p _.get

type X'Ref t p = R'Tagged'Adapted (Ref'T t) (Ref'R t p)

type X'rn'Ref t = X'R'Adapted (Ref'T t) t

x'Ref :: forall t. t -> X'R'Adapted (Ref'T t) t
x'Ref = x'Runnable'mk

instance X'method'R'get p (Ref'T t) (Ref'R t p) t where
  x'method'R'get = x'runs'r'adapted'complete @p @(Ref'T t)

qt
  :: forall x' x'' x pa pb
   . X'Cons'Ref pa Int x' x
  => X'Cons'Ref pb String x'' x
  => SetSymbol "a" pa
  => SetSymbol "b" pb
  => Run x (Int /\ String)
qt = do
  a <- x'get @pa
  b <- x'get @pb
  pure $ a /\ b

{-
qt2
  :: forall x' x
   . X'Using { a :: X'rn'Ref Int, b :: X'rn'Ref String } x' x
  => Run x (Int /\ String)
qt2 = do
  a <- x'get @"a"
  b <- x'get @"b"
  pure $ a /\ b
-}

class ConsSymbol p (X'Ref t p) x' x <= X'Cons'Ref p t x' x

instance ConsSymbol p (X'Ref t p) x' x => X'Cons'Ref p t x' x

class
  ( IsSymbol pOut
  , IsSymbol pIn
  , TypeEquals pIn pOut
  ) <=
  SetSymbol pIn pOut
  | pIn -> pOut

instance
  ( IsSymbol pOut
  , IsSymbol pIn
  , TypeEquals pIn pOut
  ) =>
  SetSymbol pIn pOut

type SingletonVal p t = forall r. IsSymbol p => Cons p t () r => { | r }

type X'Cons'P tOut p t xIn =
  forall xOut
   . IsSymbol p
  => Cons p t xIn xOut
  => Run (| xOut) tOut

a :: SingletonVal "asdf" Int -> Int
a r = rec'get @"asdf" r
