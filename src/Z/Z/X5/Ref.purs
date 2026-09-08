module Z.Z.X5.Ref where

import Z.Z.X5.UtilPrelude

import Data.Maybe (Maybe)
import Prim.RowList as RL
import Type.RowList (class ListToRow)
import Z.Z.Core (rec'get)
import Z.Z.Eff.Ref (eff'ref'get, eff'ref'new, eff'ref'set)
import Z.Z.Eff.Tagged (Eff'Tagged, eff'tagWith)
import Z.Z.X5.Core
  ( class X'Consable'R'Adapted
  , class X'RespondsTo'R'Adapted
  , R'Adapted
  , X'Rel''tag''mf
  , X'Runnable
  , r'doAsked
  , x'eval
  , x'get
  , x'runnable
  , x'set
  )
import Z.Z.X5.Responds (Responds0, Responds1, responds0'run, responds1'run)

newtype R'Ref :: forall k. Type -> k -> Type
newtype R'Ref t p = R'Ref
  { get :: Eff'Tagged p t
  , set :: t -> Eff'Tagged p Unit
  , reset :: Eff'Tagged p Unit
  }

derive instance Newtype (R'Ref t p) _

data X'Ref :: forall k. k -> Type
data X'Ref t

instance C'Relate (X'Ref t) X'Rel''tag''mf (R'Adapted (R'Ref t))

x'ref :: forall t. t -> X'Runnable (R'Adapted (R'Ref t))
x'ref t = x'runnable @(X'Ref t) t

instance X'Consable'R'Adapted (R'Ref t) t where
  x'consable'R'adapted'impl p param = do
    st <- eff'ref'new param
    pure $ wrap
      { get: eff'tagWith p $ eff'ref'get st
      , set: \v -> eff'tagWith p $ eff'ref'set v st
      , reset: eff'tagWith p $ eff'ref'set param st
      }

type R'Ref'reponds t = VariantF
  ( result :: Responds0 t
  , get :: Responds0 t
  , set :: Responds1 t Unit
  , reset :: Responds0 Unit
  )

instance X'RespondsTo'R'Adapted (R'Ref t) (R'Ref'reponds t) where
  x'respondsTo'R'adapted'impl p = match
    { result: responds0'run $ r'doAsked p \r -> r.get
    , get: responds0'run $ r'doAsked p \r -> r.get
    , set: responds1'run \v -> r'doAsked p \r -> r.set v
    , reset: responds0'run $ r'doAsked p \r -> r.reset
    }

class
  ( TypeEquals sym p
  , IsSymbol p
  , Cons p (mf p) x' x
  ) <=
  X'Cons'Named sym p mf x' x

instance
  ( TypeEquals sym p
  , IsSymbol p
  , Cons p (mf p) x' x
  ) =>
  X'Cons'Named sym p mf x' x

data X'x sym sym'var mf'tag

data X'Rel''xx''sym
data X'Rel''xx''sym'var
data X'Rel''xx''mf'tag

class X'Cons'1 x' x consdef

instance
  ( C'Relate mf'tag X'Rel''tag''mf mf
  , ConsSymbol sym'var (mf sym'var) x' x
  ) =>
  X'Cons'1 x' x (X'x sym sym'var mf'tag)

workOn
  :: forall p'ref x' x
   . X'Cons'Named "ref" p'ref
       (R'Adapted (R'Ref Int))
       x'
       x
  => Run x Int
workOn = do
  prev <- x'get @p'ref
  x'set @p'ref $ prev + 1
  pure prev

runWorkOn :: forall x. Run x Int
runWorkOn = x'eval @"ref" (x'ref 5) workOn

type TTTT =
  { didDo :: X'Ref Boolean
  , count :: X'Ref Int
  }
