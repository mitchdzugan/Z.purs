module Z.Z.Module
  ( module Aff
  , module Arg
  , module Array
  , module CA
  , module Core
  , module DC
  , module Dec
  , module DecodeGeneric
  , module Effect
  , module EffectClass
  , module Either
  , module Enc
  , module EncodeGeneric
  , module Exists
  , module Foldable
  , module Generic
  , module Lens
  , module LensRecord
  , module Maybe
  , module Promise
  , module Proxy
  , module Record
  , module Row
  , module Run
  , module RunS
  , module Str
  , module StrCommon
  , module Symbol
  , module Tup
  , module TupNested
  , module XX
  , module ZUtil
  , ppx
  , px
  ) where

import Control.Promise (Promise) as Promise
import Data.Argonaut.Core
  ( Json
  , caseJsonString
  , caseJsonNumber
  , fromString
  , jsonNull
  ) as Arg
import Data.Argonaut.Decode (class DecodeJson, fromJsonString) as Dec
import Data.Argonaut.Decode.Generic (genericDecodeJson) as DecodeGeneric
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Enc
import Data.Argonaut.Encode.Generic (genericEncodeJson) as EncodeGeneric
import Data.Array (slice) as Array
import Data.Codec (Codec, Codec') as DC
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (Either(..), either) as Either
import Data.Exists (Exists, mkExists, runExists) as Exists
import Data.Foldable (for_) as Foldable
import Data.Generic.Rep (class Generic) as Generic
import Data.Lens (Lens, Lens', view, review, over, set) as Lens
import Data.Lens.Record (prop) as LensRecord
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe') as Maybe
import Data.String (Pattern(..)) as Str
import Data.String.Common (joinWith, split) as StrCommon
import Data.Symbol (class IsSymbol, reifySymbol, reflectSymbol) as Symbol
import Data.Tuple (Tuple(..), fst, snd) as Tup
import Data.Tuple.Nested ((/\), type (/\)) as TupNested
import Effect (Effect) as Effect
import Effect.Aff (Aff, launchAff, launchAff_, runAff, runAff_) as Aff
import Effect.Class (liftEffect) as EffectClass
import Prim.Row (class Cons) as Row
import Record (merge) as Record
import Run (Run, extract) as Run
import Run.State (execState) as RunS
import Type.Proxy (Proxy(..)) as Proxy
import Z.Z.Core
  ( class Defaultable
  , JsAny
  , JsError(..)
  , P
  , Set
  , arrFilter
  , arrSlice
  , auto
  , dec
  , default
  , fDiscard
  , inc
  , jsAny
  , jsError
  , jsError'
  , jsErrorMessage
  , jsErrorName
  , jsErrorStack
  , jsonStr
  , orPass
  , p
  , setEmpty
  , setFromFoldable
  , setHas
  , setSize
  , simpleHash
  , whenJust
  ) as Core
import Z.Z.X
  ( type (!)
  , type (!$)
  , type (-!)
  , type (-!$)
  , A
  , AFF
  , AffF
  , E
  , EA
  , EarlyReturn
  , Edit
  , R
  , RA
  , RE
  , REA
  , RS
  , RSA
  , RSE
  , RSEA
  , RW
  , RWA
  , RWE
  , RWEA
  , RWS
  , RWSA
  , RWSE
  , RWSEA
  , RWa
  , RWaA
  , RWaE
  , RWaEA
  , RWaS
  , RWaSA
  , RWaSE
  , RWaSEA
  , Result
  , S
  , SA
  , SE
  , SEA
  , TEarlyResult
  , TEarlyReturn
  , TError
  , TResult
  , W
  , WA
  , WE
  , WEA
  , WRITERa
  , WS
  , WSA
  , WSE
  , Wa
  , WaA
  , WaE
  , WaEA
  , WaS
  , WaSA
  , WaSE
  , X
  , XBASE
  , XBaseF
  , XRet
  , XShortCircuit
  , edit
  , xAEff
  , xAff
  , xAsk
  , xEffectPromise
  , xEval
  , xEvalAff
  , xEvalR
  , xEvalS
  , xExec
  , xExecAff
  , xExecS
  , xFail
  , xGet
  , xHush
  , xInfo
  , xLogError
  , xLogWarning
  , xMapE
  , xMapW
  , xMapWE
  , xOk
  , xOver
  , xResult
  , xRetFail
  , xRetLift
  , xReturn
  , xRunS
  , xSay
  , xSet
  , xTellMappedHush
  , xTellMappedMHush
  , xTimeout
  , xTry
  , xUnwrap
  , xUnwrap'
  , xView
  , xWithRet
  , xrView
  ) as XX
import Z.Z.Util
  ( type (#)
  , type ($)
  , JsonDecodeError(..)
  , JsonDecodeFn
  , JsonEncodeFn
  , StringOrNum
  , Type_Ap
  , Type_Ap_R
  , arg2'
  , arg3'
  , arg4'
  , arrReverse
  , arrSort
  , arrSortBy
  , arrSortWith
  , asOrNum
  , asStringOr
  , decode
  , decode'
  , decodeJson
  , decodeJson'
  , encode
  , id
  , jsonDecode
  , jsonKeys
  , jsonLookup
  , jsonPairs
  , jsonSortedPairs
  , jsonVals
  , mapL
  , nth
  , stringOrNumString
  ) as ZUtil
import Prelude

px
  :: ∀ (@l :: Symbol) r' r @a
  . Symbol.IsSymbol l
  ⇒ Row.Cons l a r' r
  ⇒ Lens.Lens' (Record r) a
px = LensRecord.prop (Proxy.Proxy @l)

ppx
  :: ∀ (@l1 :: Symbol) (@l2 :: Symbol) r1' r1 r2' r2 @a
  . Symbol.IsSymbol l1
  ⇒ Symbol.IsSymbol l2
  ⇒ Row.Cons l1 (Record r2) r1' r1
  ⇒ Row.Cons l2 a r2' r2
  ⇒ Lens.Lens' (Record r1) a
ppx = LensRecord.prop (Proxy.Proxy @l1) <<< LensRecord.prop (Proxy.Proxy @l2)
