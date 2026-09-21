module Z.Z.X6.Methods
  ( T'assign
  , T'extract
  , T'self
  , T'x'assign
  , T'x'extract
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
  , x'act
  , x'add
  , x'addAt
  , x'alter
  , x'assign
  , x'assignAt
  , x'clear
  , x'clearAt
  , x'cons
  , x'consAt
  , x'd1entries
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
  , x'modify
  , x'over
  , x'over'b
  , x'pop
  , x'preview
  , x'preview'b
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
  , x'toArrayOf
  , x'toArrayOf'b
  , x'uncons
  , x'update
  , x'vals
  , x'valsAt
  , x'view
  , x'view'b
  ) where

import Z.Z.X6.UtilPrelude

import Data.Lens (Forget, Optic, over, preview, set, toArrayOf, view)
import Data.List (List)
import Data.Maybe (isJust)
import Data.Monoid.Endo (Endo)
import Z.Z.Barlow (class C'Barlow, First, barlow)
import Z.Z.Core (Eff'At)
import Z.Z.X6.Core
  ( class X'RespondsTo
  , class X'Results
  , x'respondTo
  , x'respondTo_
  , x'results'impl
  )
import Z.Z.X6.Responds (Responds, Responds'Const)

type T'self :: forall k. k -> Row k -> Row k
type T'self t rest = (self :: t | rest)

type T'element :: forall k. k -> Row k -> Row k
type T'element t rest = (element :: t | rest)

type T'key :: forall k. k -> Row k -> Row k
type T'key t rest = (key :: t | rest)

type T'entry :: forall k. k -> k -> Row k -> Row k
type T'entry key element rest = T'key key $ T'element element rest

type T'd1element :: forall k. k -> Row k -> Row k
type T'd1element t rest = (d1element :: t | rest)

type T'd1key :: forall k. k -> Row k -> Row k
type T'd1key t rest = (d1key :: t | rest)

type T'd2key :: forall k. k -> Row k -> Row k
type T'd2key t rest = (d2key :: t | rest)

type T'd1entry :: forall k. k -> k -> Row k -> Row k
type T'd1entry key element rest = T'd1key key $ T'd1element element rest

type T'd2entry :: forall k. k -> k -> Row k -> Row k
type T'd2entry key element rest = T'd1key key $ T'element element rest

type T'd2keys :: forall k. k -> k -> Row k -> Row k
type T'd2keys k1 k2 rest = T'd1key k1 $ T'd2key k2 rest

type T'd2full :: forall k. k -> k -> k -> Row k -> Row k
type T'd2full k1 k2 v rest = T'd1key k1 $ T'd2key k2 $ T'element v rest

---------------------------------------------------------------------

x'result
  :: forall @p m x' x result
   . ConsSymbol p m x' x
  => X'Results m result
  => Run x result
x'result = x'results'impl @m $ Proxy @p

---------------------------------------------------------------------

type T'extract t rest = (extract :: Responds'Const t | rest)

type T'x'extract p =
  forall m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract t resp'rest) (T'self t t'rest)
  => Run x t

x'extract :: forall @p. T'x'extract p
x'extract = x'respondTo_ @p @"extract"

--------------------------------------------

type T'x'act p =
  forall m x' x t resp'rest t'rest a
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract t resp'rest) (T'self t t'rest)
  => (t -> Eff'At p a)
  -> Run x a

x'act :: forall @p. T'x'act p
x'act getEff =
  x'respondTo_ @p @"extract" <#> getEff <#> unwrap <#> unsafePerformEffect

--------------------------------------------

type T'x'view p =
  forall m x' x s t a b resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract s resp'rest) (T'self s t'rest)
  => Optic (Forget a) s t a b
  -> Run x a

x'view :: forall @p. T'x'view p
x'view l = x'respondTo_ @p @"extract" <#> view l

type T'x'view'b sym p =
  forall lenses m x' x s t a b resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract s resp'rest) (T'self s t'rest)
  => C'Barlow sym lenses (Forget a) s t a b
  => Run x a

x'view'b :: forall @p @sym. T'x'view'b sym p
x'view'b = x'respondTo_ @p @"extract" <#> view (barlow @sym)

type T'x'preview p =
  forall m x' x s t a b resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract s resp'rest) (T'self s t'rest)
  => Optic (Forget (First a)) s t a b
  -> Run x (Maybe a)

x'preview :: forall @p. T'x'preview p
x'preview l = x'respondTo_ @p @"extract" <#> preview l

type T'x'preview'b sym p =
  forall lenses m x' x s t a b resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract s resp'rest) (T'self s t'rest)
  => C'Barlow sym lenses (Forget (First a)) s t a b
  => Run x (Maybe a)

x'preview'b :: forall @p @sym. T'x'preview'b sym p
x'preview'b = x'respondTo_ @p @"extract" <#> preview (barlow @sym)

type T'x'toArrayOf p =
  forall m x' x s t a b resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract s resp'rest) (T'self s t'rest)
  => Optic (Forget (Endo Function (List a))) s t a b
  -> Run x (Array a)

x'toArrayOf :: forall @p. T'x'toArrayOf p
x'toArrayOf l = x'respondTo_ @p @"extract" <#> toArrayOf l

type T'x'toArrayOf'b sym p =
  forall lenses m x' x s t a b resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract s resp'rest) (T'self s t'rest)
  => C'Barlow sym lenses (Forget (Endo Function (List a))) s t a b
  => Run x (Array a)

x'toArrayOf'b :: forall @p @sym. T'x'toArrayOf'b sym p
x'toArrayOf'b = x'respondTo_ @p @"extract" <#> toArrayOf (barlow @sym)

---------------------------------------------------------------------

type T'extractAt d1k d1v rest = (extractAt :: Responds d1k d1v | rest)

x'extractAt
  :: forall @p m x' x d1k d1v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extractAt d1k d1v resp'rest)
       (T'd1entry d1k d1v t'rest)
  => d1k
  -> Run x d1v
x'extractAt = x'respondTo @p @"extractAt"

---------------------------------------------------------------------

type T'size rest = (size :: Responds'Const Int | rest)

x'size
  :: forall @p m x' x resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'size resp'rest) t'rest
  => Run x Int
x'size = x'respondTo_ @p @"size"

---------------------------------------------------------------------

type T'sizeAt k rest = (sizeAt :: Responds k Int | rest)

x'sizeAt
  :: forall @p k m x' x resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'sizeAt k resp'rest) (T'd1key k t'rest)
  => k
  -> Run x Int
x'sizeAt = x'respondTo @p @"sizeAt"

---------------------------------------------------------------------

type T'vals t rest = (vals :: Responds'Const (Array t) | rest)

x'vals
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'vals t resp'rest) (T'element t t'rest)
  => Run x (Array t)
x'vals = x'respondTo_ @p @"vals"

---------------------------------------------------------------------

type T'valsAt k t rest = (valsAt :: Responds k (Array t) | rest)

x'valsAt
  :: forall @p k m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'valsAt k t resp'rest) (T'd2entry k t t'rest)
  => k
  -> Run x (Array t)
x'valsAt = x'respondTo @p @"valsAt"

---------------------------------------------------------------------

type T'keysAt k1 k2 rest = (keysAt :: Responds k1 (Array k2) | rest)

x'keysAt
  :: forall @p m x' x k1 k2 resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'keysAt k1 k2 resp'rest)
       (T'd2keys k1 k2 t'rest)
  => k1
  -> Run x (Array k2)
x'keysAt = x'respondTo @p @"keysAt"

---------------------------------------------------------------------

type T'entriesAt k1 k2 v rest =
  (entriesAt :: Responds k1 (Array $ k2 /\ v) | rest)

x'entriesAt
  :: forall @p m x' x k1 k2 v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'entriesAt k1 k2 v resp'rest)
       (T'd2full k1 k2 v t'rest)
  => k1
  -> Run x (Array $ k2 /\ v)
x'entriesAt = x'respondTo @p @"entriesAt"

---------------------------------------------------------------------

type T'keys k rest = (keys :: Responds'Const (Array k) | rest)

x'keys
  :: forall @p m x' x k resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'keys k resp'rest) (T'key k t'rest)
  => Run x (Array k)
x'keys = x'respondTo_ @p @"keys"

---------------------------------------------------------------------

type T'd1keys k rest = (d1keys :: Responds'Const (Array k) | rest)

x'd1keys
  :: forall @p m x' x k resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'd1keys k resp'rest) (T'd1key k t'rest)
  => Run x (Array k)
x'd1keys = x'respondTo_ @p @"d1keys"

---------------------------------------------------------------------

type T'd1entries d1k d1v rest =
  (d1entries :: Responds'Const (Array $ d1k /\ d1v) | rest)

x'd1entries
  :: forall @p m x' x d1k d1v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'd1entries d1k d1v resp'rest)
       (T'd1entry d1k d1v t'rest)
  => Run x (Array $ d1k /\ d1v)
x'd1entries = x'respondTo_ @p @"d1entries"

---------------------------------------------------------------------

type T'entries k v rest = (entries :: Responds'Const (Array $ k /\ v) | rest)

x'entries
  :: forall @p m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'entries k v resp'rest) (T'entry k v t'rest)
  => Run x (Array $ k /\ v)
x'entries = x'respondTo_ @p @"entries"

---------------------------------------------------------------------

type T'lookup k v rest = (lookup :: Responds k (Maybe v) | rest)

x'lookup
  :: forall @p m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'lookup k v resp'rest) (T'entry k v t'rest)
  => k
  -> Run x (Maybe v)
x'lookup = x'respondTo @p @"lookup"

---------------------------------------------------------------------

x'has
  :: forall @p m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'lookup k v resp'rest) (T'entry k v t'rest)
  => k
  -> Run x Boolean
x'has k = x'respondTo @p @"lookup" k <#> isJust

---------------------------------------------------------------------

type T'assign t rest = (assign :: Responds t Unit | rest)

type T'x'assign p =
  forall m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'assign t resp'rest) (T'self t t'rest)
  => t
  -> Run x Unit

x'assign :: forall @p. T'x'assign p
x'assign = x'respondTo @p @"assign"

---------------------------------------------------------------------

type T'x'update p =
  forall m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract t $ T'assign t resp'rest)
       (T'self t t'rest)
  => (t -> t)
  -> Run x Unit

x'update :: forall @p. T'x'update p
x'update f = x'respondTo_ @p @"extract" >>= x'respondTo @p @"assign" <<< f

---------------------------------------------------------------------

type T'x'reusePlus p =
  forall m x' x t resp'rest t'rest a b
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract t $ T'assign t resp'rest)
       (T'self t t'rest)
  => Optic Function t t a b
  -> b
  -> Run x Unit

---------------------------------------------------------------------

type T'x'set p =
  forall m x' x t resp'rest t'rest a b
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract t $ T'assign t resp'rest)
       (T'self t t'rest)
  => Optic Function t t a b
  -> b
  -> Run x Unit

x'set :: forall @p. T'x'set p
x'set l v = x'respondTo_ @p @"extract" >>= x'respondTo @p @"assign" <<< set l v

---------------------------------------------------------------------

type T'x'set'b sym p =
  forall lenses m x' x t resp'rest t'rest a b
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract t $ T'assign t resp'rest)
       (T'self t t'rest)
  => C'Barlow sym lenses Function t t a b
  => b
  -> Run x Unit

x'set'b :: forall @p @sym. T'x'set'b sym p
x'set'b = x'set @p $ barlow @sym

---------------------------------------------------------------------

type T'x'over p =
  forall m x' x t resp'rest t'rest a b
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract t $ T'assign t resp'rest)
       (T'self t t'rest)
  => Optic Function t t a b
  -> (a -> b)
  -> Run x Unit

x'over :: forall @p. T'x'over p
x'over f v =
  x'respondTo_ @p @"extract" >>= x'respondTo @p @"assign" <<< over f v

---------------------------------------------------------------------

type T'x'over'b sym p =
  forall lenses m x' x t resp'rest t'rest a b
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'extract t $ T'assign t resp'rest)
       (T'self t t'rest)
  => C'Barlow sym lenses Function t t a b
  => (a -> b)
  -> Run x Unit

x'over'b :: forall @p @sym. T'x'over'b sym p
x'over'b = x'over @p $ barlow @sym

---------------------------------------------------------------------

type T'assignAt k t rest = (assignAt :: Responds (k /\ t) Unit | rest)

x'assignAt
  :: forall @p m x' x k t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'assignAt k t resp'rest) (T'd1entry k t t'rest)
  => k
  -> t
  -> Run x Unit
x'assignAt k t = x'respondTo @p @"assignAt" $ k /\ t

---------------------------------------------------------------------

type T'reset rest = (reset :: Responds'Const Unit | rest)

x'reset
  :: forall @p m x' x resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'reset resp'rest) t'rest
  => Run x Unit
x'reset = x'respondTo_ @p @"reset"

---------------------------------------------------------------------

type T'resetAt k rest = (resetAt :: Responds k Unit | rest)

x'resetAt
  :: forall @p m x' x k resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'resetAt k resp'rest) (T'd1key k t'rest)
  => k
  -> Run x Unit
x'resetAt = x'respondTo @p @"resetAt"

---------------------------------------------------------------------

type T'clear rest = (clear :: Responds'Const Unit | rest)

x'clear
  :: forall @p m x' x resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'clear resp'rest) t'rest
  => Run x Unit
x'clear = x'respondTo_ @p @"clear"

---------------------------------------------------------------------

type T'clearAt k rest = (clearAt :: Responds k Unit | rest)

x'clearAt
  :: forall @p m x' x k resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'clearAt k resp'rest) (T'd1key k t'rest)
  => k
  -> Run x Unit
x'clearAt = x'respondTo @p @"clearAt"

---------------------------------------------------------------------

type T'add t rest = (add :: Responds t Unit | rest)

x'add
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'add t resp'rest) (T'self t t'rest)
  => t
  -> Run x Unit
x'add = x'respondTo @p @"add"

---------------------------------------------------------------------

type T'addAt k t rest = (addAt :: Responds (k /\ t) Unit | rest)

x'addAt
  :: forall @p m x' x k t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'addAt k t resp'rest) (T'd1entry k t t'rest)
  => k
  -> t
  -> Run x Unit
x'addAt k t = x'respondTo @p @"addAt" $ k /\ t

---------------------------------------------------------------------

type T'cons t rest = (cons :: Responds t Unit | rest)

x'cons
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'cons t resp'rest) (T'element t t'rest)
  => t
  -> Run x Unit
x'cons = x'respondTo @p @"cons"

---------------------------------------------------------------------

type T'consAt k t rest = (consAt :: Responds (k /\ t) Unit | rest)

x'consAt
  :: forall @p m x' x k t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'consAt k t resp'rest) (T'd2entry k t t'rest)
  => k
  -> t
  -> Run x Unit
x'consAt k t = x'respondTo @p @"consAt" $ k /\ t

---------------------------------------------------------------------

type T'uncons t rest = (uncons :: Responds'Const (Maybe t) | rest)

x'uncons
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'uncons t resp'rest) (T'element t t'rest)
  => Run x (Maybe t)
x'uncons = x'respondTo_ @p @"uncons"

---------------------------------------------------------------------

type T'push t rest = (push :: Responds t Unit | rest)

x'push
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'push t resp'rest) (T'element t t'rest)
  => t
  -> Run x Unit
x'push = x'respondTo @p @"push"

---------------------------------------------------------------------

type T'pop t rest = (pop :: Responds'Const (Maybe t) | rest)

x'pop
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'pop t resp'rest) (T'element t t'rest)
  => Run x (Maybe t)
x'pop = x'respondTo_ @p @"pop"

---------------------------------------------------------------------

type T'remove t rest = (remove :: Responds t Unit | rest)

x'remove
  :: forall @p m x' x t resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'remove t resp'rest) (T'key t t'rest)
  => t
  -> Run x Unit
x'remove = x'respondTo @p @"remove"

---------------------------------------------------------------------

type T'insert k v rest = (insert :: Responds (k /\ v) Unit | rest)

x'insert
  :: forall @p m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'insert k v resp'rest) (T'entry k v t'rest)
  => k
  -> v
  -> Run x Unit
x'insert k v = x'respondTo @p @"insert" (k /\ v)

---------------------------------------------------------------------

type T'replace k v rest = (replace :: Responds (k /\ v) Unit | rest)

x'replace
  :: forall @p m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m (VariantF $ T'replace k v resp'rest) (T'entry k v t'rest)
  => k
  -> v
  -> Run x Unit
x'replace k v = x'respondTo @p @"replace" (k /\ v)

---------------------------------------------------------------------

type T'x'alter p =
  forall m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m
       (VariantF $ T'lookup k v $ T'remove k $ T'insert k v resp'rest)
       (T'entry k v t'rest)
  => k
  -> (Maybe v -> Maybe v)
  -> Run x Unit

x'alter :: forall @p. T'x'alter p
x'alter k f = x'lookup @p k <#> f >>= case _ of
  Nothing -> x'remove @p k
  Just v -> x'insert @p k v

---------------------------------------------------------------------

type T'x'modify p =
  forall m x' x k v resp'rest t'rest
   . ConsSymbol p m x' x
  => X'RespondsTo m
       (VariantF $ T'lookup k v $ T'remove k $ T'insert k v resp'rest)
       (T'entry k v t'rest)
  => k
  -> (v -> v)
  -> Run x Unit

x'modify :: forall @p. T'x'modify p
x'modify k f = x'lookup @p k >>= case _ of
  Nothing -> pure unit
  Just v -> x'insert @p k $ f v