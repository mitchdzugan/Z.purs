module Z.Z.X.UtilPrelude (module M) where

import Data.Functor.Variant (class Contractable, contract) as M
import Data.Identity (Identity(..)) as M
import Data.Maybe (Maybe(..)) as M
import Data.Newtype (class Newtype, unwrap, wrap) as M
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol) as M
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry) as M
import Data.Tuple.Nested
  ( type (/\)
  , T10
  , T11
  , T2
  , T3
  , T4
  , T5
  , T6
  , T7
  , T8
  , T9
  , Tuple1
  , Tuple10
  , Tuple2
  , Tuple3
  , Tuple4
  , Tuple5
  , Tuple6
  , Tuple7
  , Tuple8
  , Tuple9
  , curry1
  , curry10
  , curry2
  , curry3
  , curry4
  , curry5
  , curry6
  , curry7
  , curry8
  , curry9
  , get1
  , get10
  , get2
  , get3
  , get4
  , get5
  , get6
  , get7
  , get8
  , get9
  , over1
  , over10
  , over2
  , over3
  , over4
  , over5
  , over6
  , over7
  , over8
  , over9
  , tuple1
  , tuple10
  , tuple2
  , tuple3
  , tuple4
  , tuple5
  , tuple6
  , tuple7
  , tuple8
  , tuple9
  , uncurry1
  , uncurry10
  , uncurry2
  , uncurry3
  , uncurry4
  , uncurry5
  , uncurry6
  , uncurry7
  , uncurry8
  , uncurry9
  , (/\)
  ) as M
import Effect (Effect) as M
import Effect.Exception (throw) as M
import Effect.Unsafe (unsafePerformEffect) as M
import Prelude
  ( class Applicative
  , class Apply
  , class Bind
  , class BooleanAlgebra
  , class Bounded
  , class Category
  , class CommutativeRing
  , class Discard
  , class DivisionRing
  , class Eq
  , class EuclideanRing
  , class Field
  , class Functor
  , class HeytingAlgebra
  , class Monad
  , class Monoid
  , class Ord
  , class Ring
  , class Semigroup
  , class Semigroupoid
  , class Semiring
  , class Show
  , type (~>)
  , Ordering(..)
  , Unit
  , Void
  , absurd
  , add
  , ap
  , append
  , apply
  , between
  , bind
  , bottom
  , clamp
  , compare
  , comparing
  , compose
  , conj
  , const
  , degree
  , discard
  , disj
  , div
  , eq
  , flap
  , flip
  , gcd
  , identity
  , ifM
  , join
  , lcm
  , liftA1
  , liftM1
  , map
  , max
  , mempty
  , min
  , mod
  , mul
  , negate
  , not
  , notEq
  , one
  , otherwise
  , pure
  , recip
  , show
  , sub
  , top
  , unit
  , unless
  , unlessM
  , void
  , when
  , whenM
  , zero
  , (#)
  , ($)
  , ($>)
  , (&&)
  , (*)
  , (*>)
  , (+)
  , (-)
  , (/)
  , (/=)
  , (<)
  , (<#>)
  , (<$)
  , (<$>)
  , (<*)
  , (<*>)
  , (<<<)
  , (<=)
  , (<=<)
  , (<>)
  , (<@>)
  , (=<<)
  , (==)
  , (>)
  , (>=)
  , (>=>)
  , (>>=)
  , (>>>)
  , (||)
  ) as M
import Prim.Row
  ( class Cons
  , class Lacks
  , class Nub
  , class Union
  , Cons
  , Lacks
  , Nub
  , Union
  ) as M
import Run
  ( AFF
  , EFFECT
  , Run(..)
  , Step(..)
  , VariantF
  , case_
  , expand
  , extract
  , inj
  , interpret
  , interpretRec
  , lift
  , liftAff
  , liftEffect
  , match
  , on
  , onMatch
  , peel
  , resume
  , run
  , runAccum
  , runAccumCont
  , runAccumPure
  , runAccumRec
  , runBaseAff
  , runBaseAff'
  , runBaseEffect
  , runCont
  , runPure
  , runRec
  , send
  ) as M
import Run.Reader (Reader, askAt, asksAt, runReaderAt) as M
import Type.Equality (class TypeEquals) as M
import Type.Proxy (Proxy(..)) as M
import Z.Z.Core
  ( class C'Relate
  , class ConsSymbol
  , T'apply
  , T'const
  , T'id
  , T2'0
  , T2'1
  ) as M
import Z.Z.Defaultable (class Generable, GDefault, default, g, mkGenerable) as M
import Z.Z.Util (type ($)) as M
import Z.Z.Wraps
  ( class Unwraps
  , class Wraps
  , wrapped'from
  , wrapped'get
  , wrapped'mk
  ) as M
