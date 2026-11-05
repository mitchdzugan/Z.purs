module Z.Z.X.Index
  ( (!!-)
  , (!#!)
  , (!$!)
  , (-!!)
  , A'
  , E'
  , EA'
  , R'
  , REA'
  , RunMW
  , Runner
  , Runner't(..)
  , T'use'e'AsSym
  , T'use'r'AsSym
  , T'use's'AsSym
  , T'use'w'AsSym
  , X
  , X'Base
  , X'E
  , X'Permit
  , X'R
  , X'flipped
  , async'x
  , e'const
  , e'const''
  , e'const'flipped
  , e'const'flipped''
  , e'fail
  , e'fail''
  , e'invert
  , e'invert''
  , e'liftAt
  , e'liftAt''
  , e'map
  , e'map''
  , e'map'flipped
  , e'mapAt
  , e'mapAt''
  , e'ok
  , e'ok''
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
  , e'unwrapAt
  , e'unwrapAt''
  , e'unwrapAt_
  , e'unwrapAt_''
  , e'unwrap_
  , e'unwrap_''
  , e'withFinalizer
  , e'withFinalizer''
  , r'ask
  , r'preview
  , r'preview'b
  , r'run
  , r'run''
  , r'toArrayOf
  , r'toArrayOf'b
  , r'view
  , r'view'b
  , runner'_
  , runner'eval
  , runner'evalA
  , runner'extend
  , runner'mk
  , runner'mkDeferred
  , s'get
  , s'over
  , s'over'b
  , s'preview
  , s'preview'b
  , s'set
  , s'set'b
  , s'setPure
  , s'setPure'b
  , s'toArrayOf
  , s'toArrayOf'b
  , s'view
  , s'view'b
  , sc'unwrap
  , sc'unwrap_
  , sync'x
  , type (<@<)
  , type (<@@)
  , type (>@>)
  , type (@@>)
  , x'do
  , x'info
  , x'logError
  , x'logWarning
  , x'out
  , x'outErr
  , x'outWarn
  , x'permit
  , x'timeout
  , x'withReturn
  , x'withReturn''
  , x'withReturn_
  , x'withReturn_''
  ) where

import Z.Z.X.UtilPrelude

import Data.Either (Either(..))
import Data.Int (floor)
import Parsing (Parser)
import Z.Z.Core
  ( class Resulting
  , Eff'At(..)
  , JsError
  , ParseError
  , T'useAsSym
  , eff'defer
  , fDiscard
  , invert
  , mapL
  , resultVal
  , runParser
  )
import Z.Z.It (reduceM)
import Z.Z.Promise (EffPromise)
import Z.Z.Run
  ( Ctx'Except
  , Ctx'x_async
  , Future
  , ctxA'adapt
  , ctxA'fromEff
  , ctxA'fromEffPromise
  , ctxA'run
  , ctxE'okAt
  , ctxE'runAt
  , ctxE'throwAt
  , ctxS'run
  )
import Z.Z.Variant (class IsVariant, var'inj)
import Z.Z.X.Core
  ( class X'Buildable
  , Reader
  , Run
  , x_eval
  , x_eval_
  )
import Z.Z.X.Method
  ( T'x'over
  , T'x'over'b
  , T'x'preview
  , T'x'preview'b
  , T'x'set
  , T'x'set'b
  , T'x'setPure
  , T'x'setPure'b
  , T'x'toArrayOf
  , T'x'toArrayOf'b
  , T'x'view
  , T'x'view'b
  , T'x_extract
  , x'over
  , x'over'b
  , x'preview
  , x'preview'b
  , x'set
  , x'set'b
  , x'setPure
  , x'setPure'b
  , x'toArrayOf
  , x'toArrayOf'b
  , x'view
  , x'view'b
  , (@.)
  )
import Z.Z.X.Readables.Base (LogLevel(..), X'BaseM, x'out'')
import Z.Z.X.Util (js_timeout)

type X'Base x = (_'x_base :: X'BaseM | x)

type X x = Run (X'Base x)

type X'flipped res x = X x res

infixr 0 type X'flipped as <@<
infixr 0 type X as >@>

infixr 0 type X'flipped as <@@
infixr 0 type X as @@>

x'out :: forall x a. a -> Unit <@< x
x'out a = x'out'' @"_'x_base" LogLevel'Info a

x'outErr :: forall x a. a -> Unit <@< x
x'outErr a = x'out'' @"_'x_base" LogLevel'Error a

x'outWarn :: forall x a. a -> Unit <@< x
x'outWarn a = x'out'' @"_'x_base" LogLevel'Warning a

x'info :: forall x a. a -> Unit <@< x
x'info = x'out

x'logWarning :: forall x a. a -> Unit <@< x
x'logWarning = x'outWarn

x'logError :: forall x a. a -> Unit <@< x
x'logError = x'outErr

type X'Permit = Reader Unit
type X'R t = Reader t

type X'E e = Ctx'Except e

type R' r x = (_'x'reader :: X'R r | x)

type E' e x = (_'x_except :: X'E e | x)

type A' x = Ctx'x_async x

type EA' e x = E' e $ A' x

type REA' r e x = R' r $ E' e $ A' x

x'permit :: forall @p x' x a. ConsSymbol p X'Permit x' x => Run x a -> Run x' a
x'permit = x_eval_ @p

x'do :: forall @p x' x a. ConsSymbol p X'Permit x' x => Eff'At p a -> Run x a
x'do (Eff'At eff) = pure unit <#> \_ -> unsafePerformEffect eff

---------------------------------------------------------------

sync'x :: forall a. () @@> a -> a
sync'x = ctxS'run <<< x_eval_ @"_'x_base" @X'BaseM

async'x :: forall a. A' () @@> a -> Future a
async'x = ctxA'run <<< x_eval_ @"_'x_base" @X'BaseM

---------------------------------------------------------------------

type RunMW x x' = forall a. Run x a -> Run x' a

newtype Runner't x x' = Runner (RunMW x x')
type Runner x = Runner't x ()

instance X'Buildable (Runner't x x') x' x Unit where
  x_buildable'run (Runner r) m = r m <#> flip (/\) unit

runner'eval :: forall x a. Runner x -> Run x a -> a
runner'eval (Runner r) = ctxS'run <<< r

runner'evalA :: forall x a. Runner x -> Run (A' x) a -> Future a
runner'evalA (Runner r) = ctxA'run <<< ctxA'adapt r

runner'mkDeferred :: forall x a. Runner x -> Run x a -> Effect a
runner'mkDeferred runner m = eff'defer \_ -> runner'eval runner m

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
r'run'' = x_eval @p

r'run :: forall p. T'use'r'AsSym p T'r'run
r'run = r'run'' @p

--------------------------------------------

type T'r'ask p = forall x' x r. ConsSymbol p (X'R r) x' x => Run x r

r'ask :: forall p. T'use'r'AsSym p (T'x_extract T'id)
r'ask = (@.) @p

r'view :: forall p. T'use'r'AsSym p (T'x'view T'id)
r'view = x'view @p

r'view'b :: forall @sym p. T'use'r'AsSym p (T'x'view'b T'id sym)
r'view'b = x'view'b @p @sym

r'preview :: forall p. T'use'r'AsSym p (T'x'preview T'id)
r'preview = x'preview @p

r'preview'b :: forall @sym p. T'use'r'AsSym p (T'x'preview'b T'id sym)
r'preview'b = x'preview'b @p @sym

r'toArrayOf :: forall p. T'use'r'AsSym p (T'x'toArrayOf T'id)
r'toArrayOf = x'toArrayOf @p

r'toArrayOf'b :: forall @sym p. T'use'r'AsSym p (T'x'toArrayOf'b T'id sym)
r'toArrayOf'b = x'toArrayOf'b @p @sym

---------------------------------------------------------------------

type T'use'e'AsSym p f = T'useAsSym "_'x_except" p f

--------------------------------------------

type T'e'try e p =
  forall x' x a. ConsSymbol p (X'E e) x' x => Run x a -> Run x' (Either e a)

e'try'' :: forall @p @e. T'e'try e p
e'try'' = ctxE'runAt @p

e'try :: forall @e p. T'use'e'AsSym p (T'e'try e)
e'try = e'try'' @p

--------------------------------------------

type T'e'fail p = forall e x' x a. ConsSymbol p (X'E e) x' x => e -> Run x a

e'fail'' :: forall @p. T'e'fail p
e'fail'' = ctxE'throwAt @p

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

type T'e'runEffPromise p =
  forall x' x a
   . ConsSymbol p (X'E JsError) (A' x') (A' x)
  => EffPromise a
  -> Run (A' x) a

e'runEffPromise'' :: forall @p. T'e'runEffPromise p
e'runEffPromise'' effP = ctxA'fromEffPromise effP >>= ctxE'okAt @p

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

type T'e'mapAt sym p =
  forall vt v' v x'' x' x e1 e2 a
   . ConsSymbol sym e1 v' v
  => IsVariant vt v
  => ConsSymbol p (X'E vt) x'' x'
  => ConsSymbol p (X'E e2) x' x
  => (e2 -> e1)
  -> Run x a
  -> Run x' a

e'mapAt'' :: forall @p @sym. T'e'mapAt sym p
e'mapAt'' f = e'map'' @p (var'inj @sym <<< f)

e'mapAt :: forall @sym p. T'use'e'AsSym p (T'e'mapAt sym)
e'mapAt = e'mapAt'' @p @sym

--------------------------------------------

type T'e'liftAt sym p =
  forall vt v' v x'' x' x e a
   . ConsSymbol sym e v' v
  => IsVariant vt v
  => ConsSymbol p (X'E vt) x'' x'
  => ConsSymbol p (X'E e) x' x
  => Run x a
  -> Run x' a

e'liftAt'' :: forall @p @sym. T'e'liftAt sym p
e'liftAt'' = e'mapAt'' @p @sym identity

e'liftAt :: forall @sym p. T'use'e'AsSym p (T'e'liftAt sym)
e'liftAt = e'liftAt'' @p @sym

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

type T'e'const p =
  forall x'' x' x e1 e2 a
   . ConsSymbol p (X'E e1) x'' x'
  => ConsSymbol p (X'E e2) x' x
  => e1
  -> Run x a
  -> Run x' a

e'const'' :: forall @p. T'e'const p
e'const'' e = e'map'' @p $ const e

e'const :: forall p. T'use'e'AsSym p T'e'const
e'const = e'const'' @p

type T'e'map'flipped p =
  forall x'' x' x e1 e2 a
   . ConsSymbol p (X'E e1) x'' x'
  => ConsSymbol p (X'E e2) x' x
  => Run x a
  -> (e2 -> e1)
  -> Run x' a

e'map'flipped :: forall p. T'use'e'AsSym p T'e'map'flipped
e'map'flipped = flip e'map

type T'e'const'flipped p =
  forall x'' x' x e1 e2 a
   . ConsSymbol p (X'E e1) x'' x'
  => ConsSymbol p (X'E e2) x' x
  => Run x a
  -> e1
  -> Run x' a

e'const'flipped'' :: forall @p. T'e'const'flipped p
e'const'flipped'' = flip $ e'const'' @p

e'const'flipped :: forall p. T'use'e'AsSym p T'e'const'flipped
e'const'flipped = e'const'flipped'' @p

infixr 0 e'map as !$!
infixr 0 e'map'flipped as !#!
infixr 0 e'const as -!!
infixr 0 e'const'flipped as !!-

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
  Just result -> pure result
  _ -> e'fail'' @p e

e'unwrap :: forall p. T'use'e'AsSym p T'e'unwrap
e'unwrap = e'unwrap'' @p

sc'unwrap :: forall p. T'use'sc'AsSym p T'e'unwrap
sc'unwrap = e'unwrap'' @p

--------------------------------------------

type T'e'unwrapAt sym p =
  forall vt v' v x' x e f a
   . ConsSymbol sym e v' v
  => IsVariant vt v
  => ConsSymbol p (X'E vt) x' x
  => Resulting f
  => e
  -> f a
  -> Run x a

e'unwrapAt'' :: forall @p @sym. T'e'unwrapAt sym p
e'unwrapAt'' e = e'unwrap'' @p (var'inj @sym e)

e'unwrapAt :: forall @sym p. T'use'e'AsSym p (T'e'unwrapAt sym)
e'unwrapAt = e'unwrapAt'' @p @sym

--------------------------------------------

type T'e'unwrapAt_ sym p =
  forall vt v' v x' x e f a
   . ConsSymbol sym e v' v
  => IsVariant vt v
  => ConsSymbol p (X'E vt) x' x
  => Resulting f
  => Generable e GDefault e
  => f a
  -> Run x a

e'unwrapAt_'' :: forall @p @sym. T'e'unwrapAt_ sym p
e'unwrapAt_'' = e'unwrapAt'' @p @sym default

e'unwrapAt_ :: forall @sym p. T'use'e'AsSym p (T'e'unwrapAt_ sym)
e'unwrapAt_ = e'unwrapAt_'' @p @sym

--------------------------------------------

type T'e'unwrap_ p =
  forall x' x e f a
   . ConsSymbol p (X'E e) x' x
  => Resulting f
  => Generable e GDefault e
  => f a
  -> Run x a

e'unwrap_'' :: forall @p. T'e'unwrap_ p
e'unwrap_'' v = case resultVal v of
  Just result -> pure result
  _ -> e'fail'' @p default

e'unwrap_ :: forall p. T'use'e'AsSym p T'e'unwrap_
e'unwrap_ = e'unwrap_'' @p

sc'unwrap_ :: forall p. T'use'sc'AsSym p T'e'unwrap_
sc'unwrap_ = e'unwrap_'' @p

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

type T'e'withFinalizer p =
  forall e x'' x' x a
   . ConsSymbol p (X'E e) x'' x'
  => ConsSymbol p (X'E e) x' x
  => (Either e a -> Run x' Unit)
  -> Run x a
  -> Run x' a

e'withFinalizer'' :: forall @p. T'e'withFinalizer p
e'withFinalizer'' handle m = do
  res <- e'try'' @p m
  handle res
  e'ok'' @p res

e'withFinalizer :: forall p. T'use'e'AsSym p T'e'withFinalizer
e'withFinalizer = e'withFinalizer'' @p

--------------------------------------------

type T'e'runEffA p =
  forall x' x a
   . ConsSymbol p (X'E JsError) (A' x') (A' x)
  => Effect a
  -> Run (A' x) a

e'runEffA'' :: forall @p. T'e'runEffA p
e'runEffA'' eff = ctxA'fromEff eff >>= ctxE'okAt @p

e'runEffA :: forall p. T'use'e'AsSym p T'e'runEffA
e'runEffA = e'runEffA'' @p

---------------------------------------------------------------------

type T'use'w'AsSym p f = T'useAsSym "_'x'writer" p f

---------------------------------------------------------------------

type T'use's'AsSym p f = T'useAsSym "_'x'state" p f

--------------------------------------------

s'get :: forall p. T'use's'AsSym p (T'x_extract T'id)
s'get = (@.) @p

s'view :: forall p. T'use's'AsSym p (T'x'view T'id)
s'view = x'view @p

s'view'b :: forall @sym p. T'use's'AsSym p (T'x'view'b T'id sym)
s'view'b = x'view'b @p @sym

s'preview :: forall p. T'use's'AsSym p (T'x'preview T'id)
s'preview = x'preview @p

s'preview'b :: forall @sym p. T'use's'AsSym p (T'x'preview'b T'id sym)
s'preview'b = x'preview'b @p @sym

s'toArrayOf :: forall p. T'use's'AsSym p (T'x'toArrayOf T'id)
s'toArrayOf = x'toArrayOf @p

s'toArrayOf'b :: forall @sym p. T'use's'AsSym p (T'x'toArrayOf'b T'id sym)
s'toArrayOf'b = x'toArrayOf'b @p @sym

s'set :: forall p. T'use's'AsSym p (T'x'set T'id)
s'set = x'set @p

s'set'b :: forall @sym p. T'use's'AsSym p (T'x'set'b T'id sym)
s'set'b = x'set'b @p @sym

s'setPure :: forall p. T'use's'AsSym p (T'x'setPure T'id)
s'setPure = x'setPure @p

s'setPure'b :: forall @sym p. T'use's'AsSym p (T'x'setPure'b T'id sym)
s'setPure'b = x'setPure'b @p @sym

s'over :: forall p. T'use's'AsSym p (T'x'over T'id)
s'over = x'over @p

s'over'b :: forall @sym p. T'use's'AsSym p (T'x'over'b T'id sym)
s'over'b = x'over'b @p @sym

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

--------------------------------------------

type T'x'withReturn_ a p =
  forall x' x
   . ConsSymbol p (X'E a) x' x
  => Generable a GDefault a
  => (Run x Unit -> Run x a)
  -> Run x' a

x'withReturn_'' :: forall @p @a. T'x'withReturn_ a p
x'withReturn_'' fm = e'try'' @p (fm (e'fail'' @p default)) >>= case _ of
  Left a -> pure a
  Right a -> pure a

x'withReturn_ :: forall p @a. T'use'sc'AsSym p (T'x'withReturn_ a)
x'withReturn_ = x'withReturn_'' @p @a

---------------------------------------------------------------------

x'timeout :: forall x. Int -> Run (A' x) Unit
x'timeout ms = fDiscard $ e'try $ e'runEffPromise $ js_timeout ms
