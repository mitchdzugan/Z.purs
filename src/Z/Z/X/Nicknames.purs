module Z.Z.X.Nicknames
  ( Edit
  , Edit'v
  , RS'
  , RWaEA'
  , RWaSEA'
  , S'
  , SEA'
  , StrW
  , W'
  , Wa'
  , WaE'
  , WaEA'
  , WaS'
  , X'S
  , X'W
  , X'Wa
  , edit
  , edit'v
  , s'eval
  , s'exec
  , s'put
  , s'run
  , s'update
  , w'eval
  , w'exec
  , w'map
  , w'map''
  , w'run
  , w'say
  , w'say''
  , w'str
  , w'str''
  , w'str'nl
  , w'str'sp
  , w'tell
  , w'tell''
  , we'map
  , we'map''
  , we'map'''
  , we'runResult
  , we'runResult''
  , we'runResult'''
  , we'tellMappedHush
  , we'tellMappedHush''
  , we'tellMappedHush'''
  , we'tellMappedMHush
  , we'tellMappedMHush''
  , we'tellMappedMHush'''
  , we'unresult
  , we'unresult''
  , we'unresult'''
  , x'now
  , x'nowMS
  , x'nowMS'number
  ) where

import Z.Z.X.UtilPrelude

import Data.Either (Either(..))
import Data.Int (floor)
import Z.Z.Core (Result, T'useAsSym)
import Z.Z.DateTime (DateTime(..), dateTime'eff'now)
import Z.Z.It (forM_)
import Z.Z.String (str'joinWith)
import Z.Z.X.Core (Run, x'eval, x'eval_, x'exec, x'exec_, x'run, x'run_)
import Z.Z.X.Index
  ( type (@@>)
  , A'
  , E'
  , R'
  , T'use'e'AsSym
  , T'use'w'AsSym
  , X'E
  , e'map''
  , e'ok''
  , e'try''
  , sync'x
  )
import Z.Z.X.Method (x'assign, x'cons, x'update)
import Z.Z.X.Readables.RW.Ref (X'Ref)
import Z.Z.X.Readables.RW.Writer (X'Writer)

type X'W t = X'Writer t
type X'Wa t = X'Writer (Array t)
type X'S t = X'Ref t

type W' w x = (_'x'writer :: X'W w | x)
type Wa' w x = (_'x'writer :: X'Wa w | x)
type S' s x = (_'x'state :: X'S s | x)

type RS' r s x = R' r $ S' s x

type WaS' w s x = Wa' w $ S' s x

type WaE' w e x = Wa' w $ E' e x

type WaEA' w e x = Wa' w $ E' e $ A' x

type RWaEA' r w e x = R' r $ Wa' w $ E' e $ A' x

type RWaSEA' r w s e x = R' r $ Wa' w $ S' s $ E' e $ A' x

type SEA' s e x = S' s $ E' e $ A' x

------------------------------------------------

s'put :: forall x s. s -> Run (S' s x) Unit
s'put = x'assign @"_'x'state"

s'update :: forall x s. (s -> s) -> Run (S' s x) Unit
s'update = x'update @"_'x'state"

s'run :: forall x r a. r -> Run (S' r x) a -> Run x (a /\ r)
s'run = x'run @"_'x'state"

s'eval :: forall x r a. r -> Run (S' r x) a -> Run x a
s'eval = x'eval @"_'x'state"

s'exec :: forall x r. r -> Run (S' r x) Unit -> Run x r
s'exec = x'exec @"_'x'state"

---------------------------------------

type Edit'v t v = S' t () @@> v

type Edit t = Edit'v t Unit

edit'v :: forall t v. t -> Edit'v t v -> v /\ t
edit'v init m = sync'x $ s'run init m

edit :: forall t. t -> Edit t -> t
edit init m = sync'x $ s'exec init m

--------------------------------------------

w'run :: forall x w a. Monoid w => Run (W' w x) a -> Run x (a /\ w)
w'run = x'run_ @"_'x'writer"

w'eval :: forall x w a. Monoid w => Run (W' w x) a -> Run x a
w'eval = x'eval_ @"_'x'writer"

w'exec :: forall x @w. Monoid w => Run (W' w x) Unit -> Run x w
w'exec = x'exec_ @"_'x'writer"

--------------------------------------------

type T'w'tell p =
  forall w x' x. ConsSymbol p (X'W w) x' x => Monoid w => w -> Run x Unit

w'tell'' :: forall @p. T'w'tell p
w'tell'' w = x'cons @p w

w'tell :: forall p. T'use'w'AsSym p T'w'tell
w'tell = w'tell'' @p

--------------------------------------------

type T'w'say p =
  forall m w x' x
   . ConsSymbol p (X'W (m w)) x' x
  => Monoid (m w)
  => Monad m
  => w
  -> Run x Unit

w'say'' :: forall @p. T'w'say p
w'say'' w = x'cons @p (pure w)

w'say :: forall p. T'use'w'AsSym p T'w'say
w'say = w'say'' @p

--------------------------------------------

type T'w'map p =
  forall m w1 w2 x'' x' x a
   . ConsSymbol p (X'W (m w1)) x'' x'
  => ConsSymbol p (X'W (m w2)) x' x
  => Monoid (m w1)
  => Monoid (m w2)
  => Monad m
  => (w2 -> w1)
  -> Run x a
  -> Run x' a

w'map'' :: forall @p. T'w'map p
w'map'' f m = do
  a /\ tells <- x'run_ @p m
  w'tell'' @p $ map f tells
  pure a

w'map :: forall p. T'use'w'AsSym p T'w'map
w'map = w'map'' @p

---------------------------------------------------------------------

type StrW = Wa' String () @@> Unit

type T'w'str sep = IsSymbol sep => ((String -> StrW) -> StrW) -> String

w'str''impl :: String -> ((String -> StrW) -> StrW) -> String
w'str''impl sep fm = str'joinWith sep $ sync'x $ w'exec $ fm w'say

w'str'' :: forall @sep. T'w'str sep
w'str'' = w'str''impl (reflectSymbol $ Proxy @sep)

w'str :: forall @sep. T'useAsSym "" sep T'w'str
w'str = w'str'' @sep

w'str'sp :: forall @sep. T'useAsSym " " sep T'w'str
w'str'sp = w'str'' @sep

w'str'nl :: forall @sep. T'useAsSym "" sep T'w'str
w'str'nl = w'str''impl "\n" -- TODO use os specific newline

---------------------------------------------------------------------

type T'we'map' wp ep =
  forall m w1 w2 e1 e2 x''''' x'''' x''' x'' x' x a
   . ConsSymbol wp (X'W (m w1)) x'''' x''
  => ConsSymbol wp (X'W (m w1)) x''''' x'
  => ConsSymbol ep (X'E e1) x''' x''
  => ConsSymbol wp (X'W (m w2)) x' x
  => ConsSymbol ep (X'E e2) x'' x'
  => Monoid (m w1)
  => Monoid (m w2)
  => Monad m
  => (w2 -> w1)
  -> (e2 -> e1)
  -> Run x a
  -> Run x'' a

type T'we'map wp =
  forall ep. T'use'e'AsSym ep (T'we'map' wp)

we'map''' :: forall @wp @ep. T'we'map' wp ep
we'map''' mapW mapE m = e'map'' @ep mapE $ w'map'' @wp mapW m

we'map'' :: forall @wp ep. T'use'e'AsSym ep (T'we'map' wp)
we'map'' = we'map''' @wp @ep

we'map :: forall @wp. T'use'w'AsSym wp T'we'map
we'map = we'map'' @wp

--------------------------------------------

type T'we'tellMappedMHush' wp ep =
  forall w e x'' x' x m a
   . ConsSymbol wp (X'W (m w)) x'' x'
  => ConsSymbol ep (X'E e) x' x
  => Monad m
  => Monoid (m w)
  => Generable a GDefault a
  => (e -> m w)
  -> Run x a
  -> Run x' a

type T'we'tellMappedMHush wp =
  forall ep. T'use'e'AsSym ep (T'we'tellMappedMHush' wp)

we'tellMappedMHush''' :: forall @wp @ep. T'we'tellMappedMHush' wp ep
we'tellMappedMHush''' mapW m = e'try'' @ep m >>= case _ of
  Left e -> w'tell'' @wp (mapW e) <#> const default
  Right a -> pure a

we'tellMappedMHush''
  :: forall @wp ep. T'use'e'AsSym ep (T'we'tellMappedMHush' wp)
we'tellMappedMHush'' = we'tellMappedMHush''' @wp @ep

we'tellMappedMHush :: forall @wp. T'use'w'AsSym wp T'we'tellMappedMHush
we'tellMappedMHush = we'tellMappedMHush'' @wp

--------------------------------------------

type T'we'tellMappedHush' wp ep =
  forall w e x'' x' x m a
   . ConsSymbol wp (X'W (m w)) x'' x'
  => ConsSymbol ep (X'E e) x' x
  => Monoid (m w)
  => Monad m
  => Generable a GDefault a
  => (e -> w)
  -> Run x a
  -> Run x' a

type T'we'tellMappedHush wp =
  forall ep. T'use'e'AsSym ep (T'we'tellMappedHush' wp)

we'tellMappedHush''' :: forall @wp @ep. T'we'tellMappedHush' wp ep
we'tellMappedHush''' mapW m = e'try'' @ep m >>= case _ of
  Left e -> w'say'' @wp (mapW e) <#> const default
  Right a -> pure a

we'tellMappedHush''
  :: forall @wp ep. T'use'e'AsSym ep (T'we'tellMappedHush' wp)
we'tellMappedHush'' = we'tellMappedHush''' @wp @ep

we'tellMappedHush :: forall @wp. T'use'w'AsSym wp T'we'tellMappedHush
we'tellMappedHush = we'tellMappedHush'' @wp

--------------------------------------------

type T'we'unresult' wp ep =
  forall w e x'' x' x a
   . ConsSymbol wp (X'Wa w) x'' x
  => ConsSymbol ep (X'E e) x' x
  => Result w e a
  -> Run x a

type T'we'unresult wp =
  forall ep. T'use'e'AsSym ep (T'we'unresult' wp)

we'unresult''' :: forall @wp @ep. T'we'unresult' wp ep
we'unresult''' { v, w } = do
  forM_ w $ w'say'' @wp
  e'ok'' @ep v

we'unresult'' :: forall @wp ep. T'use'e'AsSym ep (T'we'unresult' wp)
we'unresult'' = we'unresult''' @wp @ep

we'unresult :: forall @wp. T'use'w'AsSym wp T'we'unresult
we'unresult = we'unresult'' @wp

--------------------------------------------

type T'we'runResult' wp ep =
  forall w e x'' x' x a
   . ConsSymbol wp (X'Wa w) x'' x'
  => ConsSymbol ep (X'E e) x' x
  => Run x a
  -> Run x'' (Result w e a)

type T'we'runResult wp =
  forall ep. T'use'e'AsSym ep (T'we'runResult' wp)

we'runResult''' :: forall @wp @ep. T'we'runResult' wp ep
we'runResult''' m = (x'run @wp unit $ e'try'' @ep m) <#> \(v /\ w) -> { w, v }

we'runResult'' :: forall @wp ep. T'use'e'AsSym ep (T'we'runResult' wp)
we'runResult'' = we'runResult''' @wp @ep

we'runResult :: forall @wp. T'use'w'AsSym wp T'we'runResult
we'runResult = we'runResult'' @wp

-------------------------------------

x'now :: forall x. x @@> DateTime
x'now = pure unit <#> \_ -> unsafePerformEffect dateTime'eff'now

x'nowMS'number :: forall x. x @@> Number
x'nowMS'number = x'now <#> unwrap

x'nowMS :: forall x. x @@> Int
x'nowMS = x'nowMS'number <#> floor