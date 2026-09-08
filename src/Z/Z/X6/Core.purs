module Z.Z.X6.Core
  ( R'Identity'RespondsTo
  , R'Tagged(..)
  , RW'Tagged(..)
  , T'Consable
  , T'Runnable
  , W'Tagged(..)
  , X'Runnable(..)
  , class X'Consable
  , class X'R'RespondsTo
  , class X'R'RespondsTo'rw
  , class X'Readable
  , class X'Readable'rw
  , class X'RespondsTo
  , x'consable'impl
  , x'mkResponds
  , x'mkResponds'types
  , x'r'mkResponds
  , x'r'mkResponds'r
  , x'r'mkResponds'rw'types
  , x'r'mkResponds'types
  , x'r'mkResponds'w
  , x'readable'mk
  , x'readable'run
  , x'readable'rw'mk
  , x'respondTo
  , x'respondTo_
  , x'run
  , x'runnable
  , x'runnable_
  ) where

import Z.Z.X6.UtilPrelude

import Z.Z.X6.Responds (Responds, Responds'Const, responds'const, responds'id)

type T'Consable p m param res'm =
  forall x' x a
   . ConsSymbol p m x' x
  => param
  -> Run x a
  -> Run x' (res'm a)

class X'Consable m param res'm where
  x'consable'impl :: forall p. Proxy p -> T'Consable p m param res'm

class
  X'RespondsTo m responds responds'types
  | m -> responds responds'types where
  x'mkResponds'types :: Proxy (Record responds'types)
  x'mkResponds
    :: forall p x' x a. ConsSymbol p m x' x => Proxy p -> responds a -> Run x a

------------------------------------------------------------------------------

type T'Runnable m =
  forall p x' x a
   . ConsSymbol p m x' x
  => Proxy p
  -> Run x a
  -> Run x' a

newtype X'Runnable mf = X'Runnable (T'Runnable mf)

x'runnable
  :: forall @m param
   . X'Consable m param Identity
  => param
  -> X'Runnable m
x'runnable param =
  X'Runnable \p m -> x'consable'impl @m p param m <#> \(Identity v) -> v

x'runnable_
  :: forall @m param
   . X'Consable m param Identity
  => Generable param GDefault param
  => X'Runnable m
x'runnable_ = x'runnable @m $ g @param

x'run
  :: forall @p m x' x param a
   . ConsSymbol p m x' x
  => X'Consable m param Identity
  => ConsSymbol p m x' x
  => X'Runnable m
  -> Run x a
  -> Run x' a
x'run (X'Runnable mf) = mf (Proxy @p)

------------------------------------------------------------------------------

instance
  X'Readable r param =>
  X'Consable (Reader r) param Identity where
  x'consable'impl = impl
    where
    impl :: forall p. Proxy p -> T'Consable p (Reader r) param Identity
    impl p param m =
      wrap <$> runReaderAt p (unsafePerformEffect $ x'readable'mk @r param) m

instance
  X'R'RespondsTo r responds responds'types =>
  X'RespondsTo (Reader r) responds responds'types where
  x'mkResponds'types = Proxy
  x'mkResponds p responds = asksAt p \r -> x'r'mkResponds @r r responds

class X'Readable r param | r -> param where
  x'readable'mk :: param -> Effect r

class
  X'R'RespondsTo r responds responds'types
  | r -> responds responds'types where
  x'r'mkResponds'types :: Proxy (Record responds'types)
  x'r'mkResponds :: forall a. r -> responds a -> a

x'readable'run
  :: forall @p @r param x' x a
   . X'Readable r param
  => ConsSymbol p (Reader r) x' x
  => param
  -> Run x a
  -> Run x' a
x'readable'run param =
  runReaderAt (Proxy @p) $ unsafePerformEffect $ x'readable'mk @r param

------------------------------------------------------------------------------

x'respondTo
  :: forall @p m x' x @method args res resp' resp resp'types
   . ConsSymbol method (Responds args res) resp' resp
  => ConsSymbol p m x' x
  => X'RespondsTo m (VariantF resp) resp'types
  => args
  -> Run x res
x'respondTo args = do
  x'mkResponds @m (Proxy @p) $ inj (Proxy @method) (responds'id args)

x'respondTo_
  :: forall @p m x' x @method args res resp' resp resp'types
   . ConsSymbol method (Responds args res) resp' resp
  => ConsSymbol p m x' x
  => Generable args GDefault args
  => X'RespondsTo m (VariantF resp) resp'types
  => Run x res
x'respondTo_ = do
  x'mkResponds @m (Proxy @p) $ inj (Proxy @method) (responds'id $ g @args)

------------------------------------------------------------------------------

instance X'Readable (Identity r) r where
  x'readable'mk = pure <<< Identity

type R'Identity'RespondsTo r = VariantF
  ( get :: Responds'Const r
  , result :: Responds'Const r
  )

instance X'R'RespondsTo (Identity r) (R'Identity'RespondsTo r) (self :: r) where
  x'r'mkResponds'types = Proxy
  x'r'mkResponds (Identity r) = match
    { get: responds'const r, result: responds'const r }

---------------------------------------------------------------------

newtype R'Tagged r = R'Tagged r
newtype W'Tagged r = W'Tagged r
newtype RW'Tagged r = RW'Tagged r

---------------------------------------------------------------------

class
  X'R'RespondsTo'rw r responds'r responds'w responds'types
  | r -> responds'r responds'w responds'types where
  x'r'mkResponds'rw'types :: Proxy (Record responds'types)
  x'r'mkResponds'r :: forall a. r -> responds'r a -> a
  x'r'mkResponds'w :: forall a. r -> responds'w a -> a

instance
  ( X'R'RespondsTo'rw r responds'r responds'w responds'types
  ) =>
  X'R'RespondsTo (W'Tagged r) responds'w responds'types where
  x'r'mkResponds'types = Proxy
  x'r'mkResponds (W'Tagged r) = x'r'mkResponds'w @r r

instance
  ( X'R'RespondsTo'rw r responds'r responds'w responds'types
  ) =>
  X'R'RespondsTo (R'Tagged r) responds'r responds'types where
  x'r'mkResponds'types = Proxy
  x'r'mkResponds (R'Tagged r) = x'r'mkResponds'r @r r

instance
  ( X'R'RespondsTo'rw r (VariantF responds'r) (VariantF responds'w)
      responds'types
  , Contractable responds responds'r
  , Contractable responds responds'w
  , Union responds'r responds'w responds
  ) =>
  X'R'RespondsTo (RW'Tagged r) (VariantF responds) responds'types where
  x'r'mkResponds'types = Proxy
  x'r'mkResponds (RW'Tagged r) vf = case contract'r vf of
    Just (vf'r) -> x'r'mkResponds'r @r r vf'r
    Nothing -> case contract'w vf of
      Just (vf'r) -> x'r'mkResponds'w @r r vf'r
      Nothing -> unsafePerformEffect $ throw "unhandled case"
    where
    contract'r :: forall a. VariantF responds a -> Maybe (VariantF responds'r a)
    contract'r = contract

    contract'w :: forall a. VariantF responds a -> Maybe (VariantF responds'w a)
    contract'w = contract

---------------------------------------------------------------------

class X'Readable'rw r param | r -> param where
  x'readable'rw'mk :: param -> Effect r

instance X'Readable'rw r param => X'Readable (R'Tagged r) param where
  x'readable'mk param = R'Tagged <$> x'readable'rw'mk @r param

instance X'Readable'rw r param => X'Readable (RW'Tagged r) param where
  x'readable'mk param = RW'Tagged <$> x'readable'rw'mk @r param

instance X'Readable'rw r param => X'Readable (W'Tagged r) param where
  x'readable'mk param = W'Tagged <$> x'readable'rw'mk @r param