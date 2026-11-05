module Z.Z.Row
  ( class Row'Mk
  , class Row'Mker
  , class Row'Mker'rows
  , row'mk
  , row'mker
  , row'mker'rows
  ) where

import Prelude

import Data.List (List(..))
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Foreign.Object (Object)
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Z.Z.Core (rec'insert, rec'set)
import Z.Z.It (obj'collect)
import Z.Z.Variant (var'inj)

class Row'Mk tag to from m | tag to -> from m where
  row'mk :: String -> from -> m to

class Row'Mker tag to from m | tag to -> from m where
  row'mker :: from -> m to

class Row'Mker'rows tag rowList row from m | tag rowList -> from row m where
  row'mker'rows :: Proxy rowList -> from -> m (Record row)

instance Monad m => Row'Mker'rows tag RL.Nil () from m where
  row'mker'rows _ _ = pure {}

instance
  ( IsSymbol key
  , Row'Mk tag focus from m
  , Lacks key rowTail
  , Cons key focus rowTail row
  , Row'Mker'rows tag rowListTail rowTail from m
  , Monad m
  ) =>
  Row'Mker'rows tag (RL.Cons key focus rowListTail) row from m where
  row'mker'rows _ from = do
    rest <- row'mker'rows @tag (Proxy @rowListTail) from
    currEl <- row'mk @tag @focus (reflectSymbol $ Proxy @key) from
    pure (rec'insert @key currEl rest)

instance
  ( RL.RowToList row rowList
  , Row'Mker'rows tag rowList row from m
  ) =>
  Row'Mker tag (Record row) from m where
  row'mker = row'mker'rows @tag (Proxy @rowList)

{-

else instance
  ( RL.RowToList row rowList
  , Variant'Mker'rows tag rowList row from m
  , Newtype nt (Variant row)
  , Functor m
  ) =>
  Variant'Mker tag nt from m where
  variant'mker = variant'mker'rows @tag (Proxy @rowList)
    <#> \(k /\ mkVar) -> k /\ \int -> wrap <$> mkVar int

variant'mkers
  :: forall @tag @to from m
   . Variant'Mker tag to from m
  => Object (from -> m to)
variant'mkers = obj'collect (variant'mker @tag @to)

-}