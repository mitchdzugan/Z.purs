module Z.Z.X6.Index
  ( A'
  , E'
  , EA'
  , Edit
  , R'
  , RS'
  , RWaEA'
  , S'
  , Wa'
  , WaEA'
  , X
  , X'A
  , X'E
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
  , edit
  , r'ask
  , r'run
  , r'run''
  , r'view
  , r'view'b
  , s'eval
  , s'exec
  , s'get
  , s'run
  , s'set
  , s'set'b
  , s'view
  , s'view'b
  , sync'x
  , type (<@<)
  , type (<@@)
  , type (>@>)
  , type (@@>)
  , w'run
  , w'run''
  , w'say
  , w'say''
  , w'tell
  , w'tell''
  , we'runResult
  , we'runResult''
  , we'runResult'''
  , we'tellMappedHush
  , we'tellMappedHush''
  , we'tellMappedHush'''
  , we'tellMappedMHush
  , we'tellMappedMHush''
  , we'tellMappedMHush'''
  , x'attemptAff
  , x'info
  , x'logError
  , x'logWarning
  , x'now
  , x'nowMS
  , x'out
  , x'outErr
  , x'outWarn
  , x'timeout
  , x'withReturn
  , x'withReturn''
  ) where

import Z.Z.X6.UtilPrelude

import Control.Promise (Promise, toAff)
import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Class as EffC
import Parsing (Parser)
import Run.Except (Except, runExceptAt, throwAt)
import Z.Z.Core
  ( JsError(..)
  , ParseError
  , Result
  , T'useAsSym
  , fDiscard
  , invert
  , mapL
  , reduceM
  , runParser
  )
import Z.Z.DateTime (DateTime, dateTime'toMS)
import Z.Z.Defaultable.Util as D
import Z.Z.X6.Async (AffF(..), x'aff'')
import Z.Z.X6.Core (x'eval, x'eval_, x'exec, x'run)
import Z.Z.X6.Methods
  ( T'x'extract
  , T'x'set
  , T'x'set'b
  , T'x'view
  , T'x'view'b
  , x'cons
  )
import Z.Z.X6.Methods
  ( x'add
  , x'addAt
  , x'assign
  , x'assignAt
  , x'clear
  , x'clearAt
  , x'cons
  , x'consAt
  , x'd1keys
  , x'entries
  , x'entriesAt
  , x'extract
  , x'extractAt
  , x'has
  , x'insert
  , x'keys
  , x'keysAt
  , x'lookup
  , x'pop
  , x'push
  , x'remove
  , x'replace
  , x'reset
  , x'resetAt
  , x'result
  , x'set
  , x'set'b
  , x'size
  , x'sizeAt
  , x'uncons
  , x'vals
  , x'valsAt
  , x'view
  , x'view'b
  ) as Methods
import Z.Z.X6.Readables.Base (LogLevel(..), X'Base, x'now'', x'out'')
import Z.Z.X6.Readables.RW.Ref (X'Ref)
import Z.Z.X6.Readables.RW.Vector (X'Writer)
import Z.Z.X6.Util (js_timeout)

type X x res = Run (_'x'base :: X'Base | x) res

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

type WaEA' w e x = Wa' w $ E' e $ A' x

type RWaEA' r w e x = R' r $ Wa' w $ E' e $ A' x

s'put :: forall x s. s -> Run (S' s x) Unit
s'put = Methods.x'assign @"_'x'state"

s'run :: forall x r a. r -> Run (S' r x) a -> Run x (a /\ r)
s'run = x'run @"_'x'state"

s'eval :: forall x r a. r -> Run (S' r x) a -> Run x a
s'eval = x'eval @"_'x'state"

s'exec :: forall x r. r -> Run (S' r x) Unit -> Run x r
s'exec = x'exec @"_'x'state"

x'attemptAff :: forall x a. Aff a -> Run (A' x) (Either JsError a)
x'attemptAff aff = x'aff'' @"_'x'aff" $ attempt aff <#> mapL JsError

sync'x :: forall a. () @@> a -> a
sync'x m = unsafePerformEffect $ runBaseEffect $ expand
  $ x'eval_ @"_'x'base" @X'Base m

async'x :: forall a. A' () @@> a -> Aff a
async'x m = match { _'x'aff: \(AffCmd a) -> a } # run $ expand
  $ x'eval_ @"_'x'base" @X'Base m

effectPromiseToAff :: forall a. Effect (Promise a) -> Aff a
effectPromiseToAff e = EffC.liftEffect e >>= toAff

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

type T'w'run p =
  forall w x' x a
   . ConsSymbol p (X'W w) x' x
  => Monoid w
  => Run x a
  -> Run x' (a /\ w)

w'run'' :: forall @p. T'w'run p
w'run'' m = x'run @p unit m

w'run :: forall p. T'use'w'AsSym p T'w'run
w'run = w'run'' @p

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

---------------------------------------------------------------------

type T'use's'AsSym p f = T'useAsSym "_'x'state" p f

--------------------------------------------

s'get :: forall p. T'use's'AsSym p T'x'extract
s'get = Methods.x'extract @p

s'view :: forall p. T'use's'AsSym p T'x'view
s'view = Methods.x'view @p

s'view'b :: forall @sym p. T'use's'AsSym p (T'x'view'b sym)
s'view'b = Methods.x'view'b @p @sym

s'set :: forall p. T'use's'AsSym p T'x'set
s'set = Methods.x'set @p

s'set'b :: forall @sym p. T'use's'AsSym p (T'x'set'b sym)
s'set'b = Methods.x'set'b @p @sym

--------------------------------------------

type Edit t = S' t () @@> Unit

edit :: forall t. t -> Edit t -> t
edit init m = sync'x $ x'exec @"_'x'state" init m

--------------------------------------------

---------------------------------------------------------------------

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