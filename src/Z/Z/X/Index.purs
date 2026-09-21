module Z.Z.X.Index
  ( A'
  , E'
  , EA'
  , Edit
  , R'
  , REA'
  , RS'
  , RWaEA'
  , RWaSEA'
  , RunMW
  , Runner
  , S'
  , SEA'
  , StrW
  , T'use'e'AsSym
  , T'use'r'AsSym
  , T'use's'AsSym
  , T'use'w'AsSym
  , Wa'
  , WaE'
  , WaEA'
  , X
  , X'A
  , X'Base
  , X'E
  , X'Permit
  , X'R
  , X'S
  , X'W
  , X'Wa
  , X'flipped
  , async'x
  , e'fail
  , e'fail''
  , e'invert
  , e'invert''
  , e'map
  , e'map''
  , e'ok
  , e'ok''
  , e'runAff
  , e'runAff''
  , e'runEffA
  , e'runEffA''
  , e'runEffPromise
  , e'runEffPromise''
  , e'runParser
  , e'runParser''
  , e'try
  , e'try''
  , e'tryUntil
  , e'tryUntil''
  , e'unwrap
  , e'unwrap''
  , edit
  , r'ask
  , r'run
  , r'run''
  , r'view
  , r'view'b
  , runner'_
  , runner'eval
  , runner'extend
  , runner'mk
  , runner'mkDeferred
  , s'eval
  , s'exec
  , s'get
  , s'over
  , s'over'b
  , s'preview
  , s'preview'b
  , s'put
  , s'run
  , s'set
  , s'set'b
  , s'toArrayOf
  , s'toArrayOf'b
  , s'update
  , s'view
  , s'view'b
  , sync'_
  , sync'x
  , type (<@<)
  , type (<@@)
  , type (>@>)
  , type (@@>)
  , w'eval
  , w'exec
  , w'map
  , w'map''
  , w'run
  , w'say
  , w'say''
  , w'str
  , w'str''
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
  , we'tellMappedMHush'''
  , we'unresult
  , we'unresult''
  , we'unresult'''
  , x'attemptAff
  , x'do
  , x'info
  , x'logError
  , x'logWarning
  , x'now
  , x'nowMS
  , x'out
  , x'outErr
  , x'outWarn
  , x'permit
  , x'timeout
  , x'withReturn
  , x'withReturn''
  ) where

import Z.Z.X.UtilPrelude

import Control.Promise (Promise, toAff)
import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Class as EffC
import Parsing (Parser)
import Run.Except (Except, runExceptAt, throwAt)
import Z.Z.Core
  ( class Resulting
  , Deferred
  , Eff'At(..)
  , JsError(..)
  , ParseError
  , Result
  , T'useAsSym
  , fDiscard
  , forM_
  , invert
  , mapL
  , reduceM
  , resultVal
  , runParser
  )
import Z.Z.DateTime (DateTime, dateTime'toMS)
import Z.Z.Defaultable.Util as D
import Z.Z.String (str'joinWith)
import Z.Z.X.Async (AffF(..), x'aff'')
import Z.Z.X.Core (x'eval, x'eval_, x'exec, x'exec_, x'run, x'run_)
import Z.Z.X.Methods
  ( T'x'extract
  , T'x'over
  , T'x'over'b
  , T'x'preview
  , T'x'preview'b
  , T'x'set
  , T'x'set'b
  , T'x'toArrayOf
  , T'x'toArrayOf'b
  , T'x'view
  , T'x'view'b
  , x'cons
  )
import Z.Z.X.Methods
  ( x'assign
  , x'extract
  , x'over
  , x'over'b
  , x'preview
  , x'preview'b
  , x'set
  , x'set'b
  , x'toArrayOf
  , x'toArrayOf'b
  , x'update
  , x'view
  , x'view'b
  ) as Methods
import Z.Z.X.Readables.Base (LogLevel(..), X'BaseM, x'now'', x'out'')
import Z.Z.X.Readables.RW.Ref (X'Ref)
import Z.Z.X.Readables.RW.Vector (X'Writer)
import Z.Z.X.Util (js_timeout)

type X'Base x = (_'x'base :: X'BaseM | x)

type X x res = Run (X'Base x) res

type X'flipped res x = X x res

infixr 0 type X'flipped as <@<
infixr 0 type X as >@>

infixr 0 type X'flipped as <@@
infixr 0 type X as @@>

x'now :: forall x. DateTime <@< x
x'now = x'now'' @"_'x'base"

x'nowMS :: forall x. Number <@< x
x'nowMS = x'now <#> dateTime'toMS

x'out :: forall x a. a -> Unit <@< x
x'out a = x'out'' @"_'x'base" LogLevel'Info a

x'outErr :: forall x a. a -> Unit <@< x
x'outErr a = x'out'' @"_'x'base" LogLevel'Error a

x'outWarn :: forall x a. a -> Unit <@< x
x'outWarn a = x'out'' @"_'x'base" LogLevel'Warning a

x'info :: forall x a. a -> Unit <@< x
x'info = x'out

x'logWarning :: forall x a. a -> Unit <@< x
x'logWarning = x'outWarn

x'logError :: forall x a. a -> Unit <@< x
x'logError = x'outErr

type X'Permit = Reader Unit
type X'R t = Reader (Identity t)
type X'W t = X'Writer t
type X'Wa t = X'Writer (Array t)
type X'S t = X'Ref t

type X'E :: forall k. Type -> k -> Type
type X'E e = Except e

type X'A = AffF

type R' r x = (_'x'reader :: X'R r | x)
type W' w x = (_'x'writer :: X'W w | x)
type Wa' w x = (_'x'writer :: X'Wa w | x)
type S' s x = (_'x'state :: X'S s | x)

type E' :: forall k. Type -> Row (k -> Type) -> Row (k -> Type)
type E' e x = (_'x'except :: X'E e | x)

type A' x = (_'x'aff :: AffF | x)

type RS' r s x = R' r $ S' s x

type EA' e x = E' e $ A' x

type WaE' w e x = Wa' w $ E' e x

type WaEA' w e x = Wa' w $ E' e $ A' x

type RWaEA' r w e x = R' r $ Wa' w $ E' e $ A' x

type RWaSEA' r w s e x = R' r $ Wa' w $ S' s $ E' e $ A' x

type REA' r e x = R' r $ E' e $ A' x

type SEA' s e x = S' s $ E' e $ A' x

x'permit :: forall @p x' x a. ConsSymbol p X'Permit x' x => Run x a -> Run x' a
x'permit = x'eval_ @p

x'do :: forall p x' x a. ConsSymbol p X'Permit x' x => Eff'At p a -> Run x a
x'do (Eff'At eff) = pure $ unsafePerformEffect eff

s'put :: forall x s. s -> Run (S' s x) Unit
s'put = Methods.x'assign @"_'x'state"

s'update :: forall x s. (s -> s) -> Run (S' s x) Unit
s'update = Methods.x'update @"_'x'state"

s'run :: forall x r a. r -> Run (S' r x) a -> Run x (a /\ r)
s'run = x'run @"_'x'state"

s'eval :: forall x r a. r -> Run (S' r x) a -> Run x a
s'eval = x'eval @"_'x'state"

s'exec :: forall x r. r -> Run (S' r x) Unit -> Run x r
s'exec = x'exec @"_'x'state"

x'attemptAff :: forall x a. Aff a -> Run (A' x) (Either JsError a)
x'attemptAff aff = x'aff'' @"_'x'aff" $ attempt aff <#> mapL JsError

sync'_ :: forall a. Run () a -> a
sync'_ = unsafePerformEffect <<< runBaseEffect <<< expand

sync'x :: forall a. () @@> a -> a
sync'x = unsafePerformEffect <<< runBaseEffect <<< expand
  <<< x'eval_ @"_'x'base" @X'BaseM

async'x :: forall a. A' () @@> a -> Aff a
async'x m = match { _'x'aff: \(AffCmd a) -> a } # run $ expand
  $ x'eval_ @"_'x'base" @X'BaseM m

effectPromiseToAff :: forall a. Effect (Promise a) -> Aff a
effectPromiseToAff e = EffC.liftEffect e >>= toAff

---------------------------------------------------------------------

type RunMW x x' = forall a. Run x a -> Run x' a

newtype Runner x = Runner (RunMW x ())

runner'eval :: forall x a. Runner x -> Run x a -> a
runner'eval (Runner r) m = sync'_ $ r m

runner'mkDeferred :: forall x a. Runner x -> Run x a -> Deferred a
runner'mkDeferred runner m = \_ -> runner'eval runner m

runner'_ :: Runner ()
runner'_ = Runner identity

runner'mk :: forall x. (forall a. Run x a -> Run () a) -> Runner x
runner'mk = Runner

runner'extend :: forall x x'. RunMW x x' -> Runner x' -> Runner x
runner'extend fm (Runner r) = Runner (r <<< fm)

---------------------------------------------------------------------

type T'use'r'AsSym p f = T'useAsSym "_'x'reader" p f

--------------------------------------------

type T'r'run p =
  forall x' x r a. ConsSymbol p (X'R r) x' x => r -> Run x a -> Run x' a

r'run'' :: forall @p. T'r'run p
r'run'' = x'eval @p

r'run :: forall p. T'use'r'AsSym p T'r'run
r'run = r'run'' @p

--------------------------------------------

type T'r'ask p = forall x' x r. ConsSymbol p (X'R r) x' x => Run x r

r'ask :: forall p. T'use'r'AsSym p T'x'extract
r'ask = Methods.x'extract @p

r'view :: forall p. T'use'r'AsSym p T'x'view
r'view = Methods.x'view @p

r'view'b :: forall @sym p. T'use'r'AsSym p (T'x'view'b sym)
r'view'b = Methods.x'view'b @p @sym

---------------------------------------------------------------------

type T'use'e'AsSym p f = T'useAsSym "_'x'except" p f

--------------------------------------------

type T'e'try p =
  forall e x' x a. ConsSymbol p (X'E e) x' x => Run x a -> Run x' (Either e a)

e'try'' :: forall @p. T'e'try p
e'try'' = runExceptAt (Proxy @p)

e'try :: forall p. T'use'e'AsSym p T'e'try
e'try = e'try'' @p

--------------------------------------------

type T'e'fail p = forall e x' x a. ConsSymbol p (X'E e) x' x => e -> Run x a

e'fail'' :: forall @p. T'e'fail p
e'fail'' = throwAt (Proxy @p)

e'fail :: forall p. T'use'e'AsSym p T'e'fail
e'fail = e'fail'' @p

--------------------------------------------

type T'e'ok p =
  forall e x' x a. ConsSymbol p (X'E e) x' x => Either e a -> Run x a

e'ok'' :: forall @p. T'e'ok p
e'ok'' (Left e) = e'fail'' @p e
e'ok'' (Right a) = pure a

e'ok :: forall p. T'use'e'AsSym p T'e'ok
e'ok = e'ok'' @p

--------------------------------------------

type T'e'runAff p =
  forall x' x a
   . ConsSymbol p (X'E JsError) (A' x') (A' x)
  => Aff a
  -> Run (A' x) a

e'runAff'' :: forall @p. T'e'runAff p
e'runAff'' aff = x'attemptAff aff >>= e'ok'' @p

e'runAff :: forall p. T'use'e'AsSym p T'e'runAff
e'runAff = e'runAff'' @p

--------------------------------------------

type T'e'runEffPromise p =
  forall x' x a
   . ConsSymbol p (X'E JsError) (A' x') (A' x)
  => Effect (Promise a)
  -> Run (A' x) a

e'runEffPromise'' :: forall @p. T'e'runEffPromise p
e'runEffPromise'' = e'runAff'' @p <<< effectPromiseToAff

e'runEffPromise :: forall p. T'use'e'AsSym p T'e'runEffPromise
e'runEffPromise = e'runEffPromise'' @p

--------------------------------------------

type T'e'runParser p =
  forall s x' x a
   . ConsSymbol p (X'E ParseError) x' x
  => s
  -> Parser s a
  -> Run x a

e'runParser'' :: forall @p. T'e'runParser p
e'runParser'' s pr = e'ok'' @p $ runParser s pr

e'runParser :: forall p. T'use'e'AsSym p T'e'runParser
e'runParser = e'runParser'' @p

--------------------------------------------

type T'e'map p =
  forall x'' x' x e1 e2 a
   . ConsSymbol p (X'E e1) x'' x'
  => ConsSymbol p (X'E e2) x' x
  => (e2 -> e1)
  -> Run x a
  -> Run x' a

e'map'' :: forall @p. T'e'map p
e'map'' f m = e'try'' @p m <#> mapL f >>= e'ok'' @p

e'map :: forall p. T'use'e'AsSym p T'e'map
e'map = e'map'' @p

--------------------------------------------

type T'e'invert p =
  forall x'' x' x e a
   . ConsSymbol p (X'E e) x'' x'
  => ConsSymbol p (X'E a) x' x
  => Run x e
  -> Run x' a

e'invert'' :: forall @p. T'e'invert p
e'invert'' m = e'try'' @p m <#> invert >>= e'ok'' @p

e'invert :: forall p. T'use'e'AsSym p T'e'invert
e'invert = e'invert'' @p

--------------------------------------------

type T'e'unwrap p =
  forall x' x e f a
   . ConsSymbol p (X'E e) x' x
  => Resulting f
  => e
  -> f a
  -> Run x a

e'unwrap'' :: forall @p. T'e'unwrap p
e'unwrap'' e v = case resultVal v of
  Just v -> pure v
  _ -> e'fail'' @p e

e'unwrap :: forall p. T'use'e'AsSym p T'e'unwrap
e'unwrap = e'unwrap'' @p

--------------------------------------------

type T'e'tryUntil p =
  forall x''' x'' x' x e a
   . ConsSymbol p (X'E e) x''' x''
  => ConsSymbol p (X'E a) x'' x'
  => ConsSymbol p (X'E e) x' x
  => Run x a
  -> Array (e -> Run x a)
  -> Run x'' a

e'tryUntil'' :: forall @p. T'e'tryUntil p
e'tryUntil'' try1 tryRest = e'invert'' @p $ e'invert'' @p try1 >>= \e1 ->
  reduceM (\e tryN -> e'invert'' @p $ tryN e) e1 tryRest

e'tryUntil :: forall p. T'use'e'AsSym p T'e'tryUntil
e'tryUntil = e'tryUntil'' @p

--------------------------------------------

type T'e'runEffA p =
  forall x' x a
   . ConsSymbol p (X'E JsError) (A' x') (A' x)
  => Effect a
  -> Run (A' x) a

e'runEffA'' :: forall @p. T'e'runEffA p
e'runEffA'' eff = x'attemptAff (EffC.liftEffect eff) >>= e'ok'' @p

e'runEffA :: forall p. T'use'e'AsSym p T'e'runEffA
e'runEffA = e'runEffA'' @p

---------------------------------------------------------------------

type T'use'w'AsSym p f = T'useAsSym "_'x'writer" p f

--------------------------------------------

w'run :: forall x w a. Monoid w => Run (W' w x) a -> Run x (a /\ w)
w'run = x'run_ @"_'x'writer"

w'eval :: forall x w a. Monoid w => Run (W' w x) a -> Run x a
w'eval = x'eval_ @"_'x'writer"

w'exec :: forall x w. Monoid w => Run (W' w x) Unit -> Run x w
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

type T'use's'AsSym p f = T'useAsSym "_'x'state" p f

--------------------------------------------

s'get :: forall p. T'use's'AsSym p T'x'extract
s'get = Methods.x'extract @p

s'view :: forall p. T'use's'AsSym p T'x'view
s'view = Methods.x'view @p

s'view'b :: forall @sym p. T'use's'AsSym p (T'x'view'b sym)
s'view'b = Methods.x'view'b @p @sym

s'preview :: forall p. T'use's'AsSym p T'x'preview
s'preview = Methods.x'preview @p

s'preview'b :: forall @sym p. T'use's'AsSym p (T'x'preview'b sym)
s'preview'b = Methods.x'preview'b @p @sym

s'toArrayOf :: forall p. T'use's'AsSym p T'x'toArrayOf
s'toArrayOf = Methods.x'toArrayOf @p

s'toArrayOf'b :: forall @sym p. T'use's'AsSym p (T'x'toArrayOf'b sym)
s'toArrayOf'b = Methods.x'toArrayOf'b @p @sym

s'set :: forall p. T'use's'AsSym p T'x'set
s'set = Methods.x'set @p

s'set'b :: forall @sym p. T'use's'AsSym p (T'x'set'b sym)
s'set'b = Methods.x'set'b @p @sym

s'over :: forall p. T'use's'AsSym p T'x'over
s'over = Methods.x'over @p

s'over'b :: forall @sym p. T'use's'AsSym p (T'x'over'b sym)
s'over'b = Methods.x'over'b @p @sym

--------------------------------------------

type Edit t = S' t () @@> Unit

edit :: forall t. t -> Edit t -> t
edit init m = sync'x $ x'exec @"_'x'state" init m

---------------------------------------------------------------------

type StrW = Wa' String () @@> Unit

type T'w'str sep = IsSymbol sep => ((String -> StrW) -> StrW) -> String

w'str'' :: forall @sep. T'w'str sep
w'str'' fm =
  str'joinWith (reflectSymbol $ Proxy @sep) $ sync'x $ w'exec $ fm w'say

w'str :: forall @sep. T'useAsSym "" sep T'w'str
w'str = w'str'' @sep

w'str'sp :: forall @sep. T'useAsSym " " sep T'w'str
w'str'sp = w'str'' @sep

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

we'map''
  :: forall @wp ep. T'use'e'AsSym ep (T'we'map' wp)
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
  Left e -> w'tell'' @wp (mapW e) <#> const D.default
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
  Left e -> w'say'' @wp (mapW e) <#> const D.default
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

---------------------------------------------------------------------

type T'use'sc'AsSym p f = T'useAsSym "_'x'shortcircuit" p f

--------------------------------------------

type T'x'withReturn p =
  forall x' x a
   . ConsSymbol p (X'E a) x' x
  => ((a -> Run x Unit) -> Run x a)
  -> Run x' a

x'withReturn'' :: forall @p. T'x'withReturn p
x'withReturn'' fm = e'try'' @p (fm (e'fail'' @p)) >>= case _ of
  Left a -> pure a
  Right a -> pure a

x'withReturn :: forall p. T'use'sc'AsSym p T'x'withReturn
x'withReturn = x'withReturn'' @p

---------------------------------------------------------------------

x'timeout :: forall x. Int -> Run (A' x) Unit
x'timeout ms = fDiscard $ e'try $ e'runEffPromise $ js_timeout ms