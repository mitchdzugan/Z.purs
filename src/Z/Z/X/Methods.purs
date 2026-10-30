module Z.Z.X.Methods
  ( T'self
  , _'alter
  , _'cons
  , _'consAt
  , _'insert
  , _'lookup
  , _'modify
  , x'add
  , x'addAt
  , x'alter
  , x'assign
  , x'assignAt
  , x'assignPure
  , x'clear
  , x'clearAt
  , x'cons
  , x'consAt
  , x'consLast
  , x'd1cons
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
  , x'pop
  , x'push
  , x'remove
  , x'replace
  , x'resetAt
  , x'result
  , x'size
  , x'sizeAt
  , x'uncons
  , x'update
  , x'vals
  , x'valsAt
  ) where

import Z.Z.X.UtilPrelude

import Data.Maybe (isJust)
import Z.Z.Core (Eff'At)
import Z.Z.X.Core (class X'Results, T'use'_'AsSym, x'results'impl)
import Z.Z.X.Method
  ( T'xm'add
  , T'xm'addAt
  , T'xm'alter
  , T'xm'assign
  , T'xm'assignAt
  , T'xm'assignPure
  , T'xm'clear
  , T'xm'clearAt
  , T'xm'cons
  , T'xm'consAt
  , T'xm'consLast
  , T'xm'd1'cons
  , T'xm'd1'entries
  , T'xm'd1'keys
  , T'xm'entries
  , T'xm'entriesAt
  , T'xm'extract
  , T'xm'extractAt
  , T'xm'has
  , T'xm'insert
  , T'xm'keys
  , T'xm'keysAt
  , T'xm'lookup
  , T'xm'modify
  , T'xm'pop
  , T'xm'push
  , T'xm'remove
  , T'xm'replace
  , T'xm'resetAt
  , T'xm'size
  , T'xm'sizeAt
  , T'xm'uncons
  , T'xm'update
  , T'xm'vals
  , T'xm'valsAt
  , xm'add
  , xm'addAt
  , xm'alter
  , xm'assign
  , xm'assignAt
  , xm'assignPure
  , xm'clear
  , xm'clearAt
  , xm'cons
  , xm'consAt
  , xm'consLast
  , xm'd1'cons
  , xm'd1'entries
  , xm'd1'keys
  , xm'entries
  , xm'entriesAt
  , xm'extract
  , xm'extractAt
  , xm'has
  , xm'insert
  , xm'keys
  , xm'keysAt
  , xm'lookup
  , xm'modify
  , xm'pop
  , xm'push
  , xm'remove
  , xm'replace
  , xm'resetAt
  , xm'size
  , xm'sizeAt
  , xm'uncons
  , xm'update
  , xm'vals
  , xm'valsAt
  )

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

x'assign :: forall @p. T'xm'assign T'id p
x'assign = xm'assign (Proxy @p)

---------------------------------------------------------------------

x'assignPure :: forall @p. T'xm'assignPure T'id p
x'assignPure = xm'assignPure (Proxy @p)

---------------------------------------------------------------------

x'update :: forall @p. T'xm'update T'id p
x'update = xm'update (Proxy @p)

---------------------------------------------------------------------

x'extract :: forall @p. T'xm'extract T'id p
x'extract = xm'extract (Proxy @p)

---------------------------------------------------------------------

x'extractAt :: forall @p. T'xm'extractAt T'id p
x'extractAt = xm'extractAt (Proxy @p)

---------------------------------------------------------------------

x'size :: forall @p. T'xm'size T'id p
x'size = xm'size (Proxy @p)

---------------------------------------------------------------------

x'sizeAt :: forall @p. T'xm'sizeAt T'id p
x'sizeAt = xm'sizeAt (Proxy @p)

---------------------------------------------------------------------

x'vals :: forall @p. T'xm'vals T'id p
x'vals = xm'vals (Proxy @p)

---------------------------------------------------------------------

x'valsAt :: forall @p. T'xm'valsAt T'id p
x'valsAt = xm'valsAt (Proxy @p)

---------------------------------------------------------------------

x'keysAt :: forall @p. T'xm'keysAt T'id p
x'keysAt = xm'keysAt (Proxy @p)

---------------------------------------------------------------------

x'entriesAt :: forall @p. T'xm'entriesAt T'id p
x'entriesAt = xm'entriesAt (Proxy @p)

---------------------------------------------------------------------

x'keys :: forall @p. T'xm'keys T'id p
x'keys = xm'keys (Proxy @p)

---------------------------------------------------------------------

x'd1keys :: forall @p. T'xm'd1'keys T'id p
x'd1keys = xm'd1'keys (Proxy @p)

---------------------------------------------------------------------

x'd1entries :: forall @p. T'xm'd1'entries T'id p
x'd1entries = xm'd1'entries (Proxy @p)

---------------------------------------------------------------------

x'entries :: forall @p. T'xm'entries T'id p
x'entries = xm'entries (Proxy @p)

---------------------------------------------------------------------

x'lookup :: forall @p. T'xm'lookup T'id p
x'lookup = xm'lookup (Proxy @p)

---------------------------------------------------------------------

x'has :: forall @p. T'xm'has T'id p
x'has = xm'has (Proxy @p)

---------------------------------------------------------------------

x'assignAt :: forall @p. T'xm'assignAt T'id p
x'assignAt = xm'assignAt (Proxy @p)

---------------------------------------------------------------------

x'resetAt :: forall @p. T'xm'resetAt T'id p
x'resetAt = xm'resetAt (Proxy @p)

---------------------------------------------------------------------

x'clear :: forall @p. T'xm'clear T'id p
x'clear = xm'clear (Proxy @p)

---------------------------------------------------------------------

x'clearAt :: forall @p. T'xm'clearAt T'id p
x'clearAt = xm'clearAt (Proxy @p)

---------------------------------------------------------------------

x'add :: forall @p. T'xm'add T'id p
x'add = xm'add (Proxy @p)

---------------------------------------------------------------------

x'addAt :: forall @p. T'xm'addAt T'id p
x'addAt = xm'addAt (Proxy @p)

---------------------------------------------------------------------

x'cons :: forall @p. T'xm'cons T'id p
x'cons = xm'cons (Proxy @p)

---------------------------------------------------------------------

x'd1cons :: forall @p. T'xm'd1'cons T'id p
x'd1cons = xm'd1'cons (Proxy @p)

---------------------------------------------------------------------

x'consAt :: forall @p. T'xm'consAt T'id p
x'consAt = xm'consAt (Proxy @p)

---------------------------------------------------------------------

x'consLast :: forall @p. T'xm'consLast T'id p
x'consLast = xm'consLast (Proxy @p)

---------------------------------------------------------------------

x'uncons :: forall @p. T'xm'uncons T'id p
x'uncons = xm'uncons (Proxy @p)

---------------------------------------------------------------------

x'push :: forall @p. T'xm'push T'id p
x'push = xm'push (Proxy @p)

---------------------------------------------------------------------

x'pop :: forall @p. T'xm'pop T'id p
x'pop = xm'pop (Proxy @p)

---------------------------------------------------------------------

x'remove :: forall @p. T'xm'remove T'id p
x'remove = xm'remove (Proxy @p)

---------------------------------------------------------------------

x'insert :: forall @p. T'xm'insert T'id p
x'insert = xm'insert (Proxy @p)

---------------------------------------------------------------------

x'replace :: forall @p. T'xm'replace T'id p
x'replace = xm'replace (Proxy @p)

---------------------------------------------------------------------

x'alter :: forall @p. T'xm'alter T'id p
x'alter = xm'alter (Proxy @p)

x'modify :: forall @p. T'xm'modify T'id p
x'modify = xm'modify (Proxy @p)

-------------------------------------------------------------------------------

_'insert :: forall p. T'use'_'AsSym p (T'xm'insert T'id)
_'insert = x'insert @p

_'lookup :: forall p. T'use'_'AsSym p (T'xm'lookup T'id)
_'lookup = x'lookup @p

_'cons :: forall p. T'use'_'AsSym p (T'xm'cons T'id)
_'cons = x'cons @p

_'consAt :: forall p. T'use'_'AsSym p (T'xm'consAt T'id)
_'consAt = x'consAt @p

_'alter :: forall p. T'use'_'AsSym p (T'xm'alter T'id)
_'alter = x'alter @p

_'modify :: forall p. T'use'_'AsSym p (T'xm'modify T'id)
_'modify = x'modify @p
