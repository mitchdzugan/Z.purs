module Z.Z.SorN
  ( SorN(..)
  , class IsStringOrNum
  , sOrN
  ) where

import Prelude

import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap, wrap)
import Z.Z.Id (class Identable, id'of)

newtype SorN = SorN (Either String Int)

derive instance Newtype SorN _

class IsStringOrNum a where
  sOrN :: a -> SorN

derive instance Eq SorN
derive instance Ord SorN

instance EncodeJson SorN where
  encodeJson = unwrap >>> either encodeJson encodeJson

instance IsStringOrNum String where
  sOrN = wrap <<< Left

instance IsStringOrNum Int where
  sOrN = wrap <<< Right

instance IsStringOrNum SorN where
  sOrN = identity

instance Show SorN where
  show = unwrap >>> either identity show

instance Identable SorN where
  ident'get = id'of <<< show