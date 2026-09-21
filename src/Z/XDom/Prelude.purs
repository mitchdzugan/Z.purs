module Z.XDom.Prelude
  ( Reducer
  , XurlStProviderX
  , module ModuleReExports
  ) where

import Z.Prelude
  ( class Applicative
  , class Apply
  , class At
  , class Barlow
  , class Bind
  , class BooleanAlgebra
  , class Bounded
  , class BoundedEnum
  , class C'Barlow
  , class C'Relate
  , class Category
  , class CommutativeRing
  , class Cons
  , class ConsSymbol
  , class ConstructBarlow
  , class ConstructBarlow'Get
  , class ConstructBarlow'Get'
  , class DecodeJson
  , class DefaultValueRecord
  , class Discard
  , class DivisionRing
  , class EncodeJson
  , class Enum
  , class Eq
  , class EuclideanRing
  , class Field
  , class Foldable
  , class Functor
  , class G2OrDefault
  , class GOrDefault
  , class Generable
  , class Generic
  , class HeytingAlgebra
  , class Identable
  , class Identable'Functor
  , class Index
  , class IsId
  , class IsStringOrNum
  , class IsSymbol
  , class Lacks
  , class Mapping
  , class Monad
  , class Monoid
  , class Newtype
  , class Nub
  , class Ord
  , class ParseSymbol
  , class Resulting
  , class RevSym
  , class Ring
  , class RtError
  , class SText
  , class Semigroup
  , class Semigroupoid
  , class Semiring
  , class Show
  , class SplitSp1
  , class SplitSp1Impl
  , class Strong
  , class TypeEquals
  , class Union
  , class Unwraps
  , class UpCat
  , class UpCf
  , class UpCt
  , class Wraps
  , class Wraps'Old
  , type (#)
  , type ($)
  , type (+)
  , type (/\)
  , type (<@<)
  , type (<@@)
  , type (>@>)
  , type (@@>)
  , type (~>)
  , A'
  , Aff
  , AffineTraversal
  , AntiUnit
  , B'HashMap
  , B'HashSet
  , B'Ref
  , B'Ref'nt
  , Buffer
  , Byte
  , Codec
  , Codec'
  , D'Int'0
  , D'Int'1
  , Date
  , DateTime(..)
  , Day
  , Deferred
  , E'
  , EA'
  , Edit
  , Eff'At(..)
  , Effect
  , Either(..)
  , Except
  , Exists
  , First
  , Fold
  , Foreign
  , Forget
  , G1
  , G2
  , GDefault
  , HashMap(..)
  , HashSet(..)
  , Hour
  , Hours(..)
  , IdV
  , IdVF(..)
  , Idented
  , Identity(..)
  , Instant
  , Int'Default0(..)
  , Int'Default1(..)
  , JsAny
  , JsError(..)
  , Json
  , JsonCodec
  , JsonDecodeError(..)
  , JsonDecodeFn
  , JsonEncodeFn
  , Lens
  , Lens'
  , List(..)
  , Map
  , Maybe(..)
  , Millisecond
  , Milliseconds(..)
  , Minute
  , Month(..)
  , Object
  , Optic
  , Ordering(..)
  , P
  , Pair(..)
  , PairKey(..)
  , ParseError
  , Parser
  , ParserT
  , Pattern(..)
  , Prism
  , Prism'
  , Promise
  , Proxy(..)
  , R'
  , R'HashMap
  , R'HashSet
  , R'Ref
  , R'Ref'nt
  , REA'
  , RS'
  , RWaEA'
  , RWaSEA'
  , RawJsonDecodeError
  , Reader
  , ResourceStage(..)
  , Result
  , RouteDuplex
  , RouteDuplex'
  , RouteError
  , Run
  , RunMW
  , Runner
  , S'
  , SEA'
  , Second
  , Set
  , SorN(..)
  , State
  , StrW
  , T'Related
  , T'RelatedBy
  , T'_
  , T'apply
  , T'assign
  , T'comp
  , T'const
  , T'extract
  , T'flip
  , T'id
  , T'self
  , T'use'e'AsSym
  , T'use'r'AsSym
  , T'use's'AsSym
  , T'use'w'AsSym
  , T'useAsSym
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
  , T2'0
  , T2'1
  , TPlus
  , Time(..)
  , Tuple(..)
  , Type_Ap
  , Type_Ap_R
  , URL
  , Unit
  , Void
  , Wa'
  , WaE'
  , WaEA'
  , WithDefaultable
  , Writer
  , X
  , X'A
  , X'Base
  , X'E
  , X'HashMap
  , X'HashMap'w
  , X'HashMap2D
  , X'HashMap2D'w
  , X'HashSet
  , X'HashSet'w
  , X'HashSet2D
  , X'HashSet2D'w
  , X'Permit
  , X'R
  , X'Ref
  , X'Ref'nt
  , X'Ref'nt'w
  , X'Ref'w
  , X'S
  , X'W
  , X'Wa
  , X'flipped
  , Year
  , _'
  , _Just
  , __
  , _o
  , _o_
  , absurd
  , add
  , adjustDateTime
  , antiUnit
  , ap
  , append
  , apply
  , arg2'
  , arg3'
  , arg4'
  , arr'concat
  , arr'drop
  , arr'empty
  , arr'filter
  , arr'fold
  , arr'fromFoldable
  , arr'range
  , arr'range'inc
  , arr'reverse
  , arr'size
  , arr'slice
  , arr'sort
  , arr'sortBy
  , arr'sortWith
  , arr'withInd
  , async'x
  , at
  , barlow
  , baseDecodeJson
  , between
  , bind
  , bottom
  , byte
  , canonicalDate
  , caseJsonNumber
  , caseJsonString
  , ceil
  , clamp
  , compare
  , comparing
  , compose
  , conj
  , const
  , constL
  , dateTime'fromMS
  , dateTime'fromMS'Int
  , dateTime'month'i0
  , dateTime'toMS
  , dateTime'year
  , dec
  , decode
  , decode'
  , decodeErrTypeMismatch
  , decodeFailTypeMismatch
  , decodeJson
  , decodeJson'
  , default
  , defaultCardinality
  , defaultFromEnum
  , defaultToEnum
  , deferred'run
  , degree
  , discard
  , disj
  , div
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
  , eff'tag
  , either
  , encode
  , encodeForeign
  , encodeJson
  , encodeOpts
  , eq
  , execState
  , expand
  , extract
  , fDiscard
  , ffmap
  , ffmapFlipped
  , flap
  , flip
  , floor
  , fold
  , foldlDefault
  , forM
  , forM_
  , fromByte
  , fromJsonString
  , fromMaybe
  , fromMaybe'
  , fromRawDateTime
  , fromString
  , fst
  , g
  , g'
  , g1
  , g2
  , g_
  , gcd
  , genericDecodeJson
  , genericEncodeJson
  , genericShow
  , gmOr'_
  , gmOr_
  , gm_
  , hm'empty
  , hm'entries
  , hm'fromFoldable
  , hm'has
  , hm'keys
  , hm'lookup
  , hm'set
  , hm'size
  , hm'vals
  , hmap
  , hmapWithIndex
  , hs'add
  , hs'empty
  , hs'fromFoldable
  , hs'has
  , hs'size
  , hs'vals
  , hush
  , id
  , id'bytes
  , id'bytesImpl
  , id'char
  , id'key
  , id'keyImpl
  , id'of
  , id'via
  , idLens
  , ident'bytes
  , ident'get
  , ident'key
  , ident'uuid
  , identable'map
  , idented'id
  , idented'mk
  , idented'v
  , identity
  , ifM
  , inc
  , instant
  , intFromString
  , invert
  , isJust
  , isNothing
  , ix
  , jOr
  , jOr'
  , jOr0
  , jOr1
  , jOr1n
  , jOrE
  , jOrF
  , jOrT
  , join
  , jsAny
  , jsError
  , jsError'
  , jsErrorMessage
  , jsErrorName
  , jsErrorStack
  , jsonDecode
  , jsonEmptyObject
  , jsonKeys
  , jsonLookup
  , jsonNull
  , jsonPairs
  , jsonRmNils
  , jsonSortedPairs
  , jsonStr
  , jsonVals
  , lastOf
  , launchAff
  , launchAff_
  , lcm
  , lift
  , liftA1
  , liftEffect
  , liftM1
  , list'fromFoldable
  , map
  , map'empty
  , map'fromFoldable
  , map'set
  , map'size
  , map'vals
  , mapL
  , mapM
  , max
  , maximum
  , maximumBy
  , mempty
  , merge
  , mfirst
  , min
  , minimum
  , minimumBy
  , mkDefaultRecord
  , mkExists
  , mkGenerable
  , mlast
  , mod
  , mul
  , negate
  , not
  , notEq
  , nth
  , nu'
  , nu'mk
  , o_
  , obj'empty
  , obj'entries
  , obj'has
  , obj'insert
  , obj'keys
  , obj'lookup
  , obj'vals
  , objST'delete
  , objST'has
  , objST'new
  , objST'peek
  , objST'poke
  , objST'run
  , ofArrayBuffer
  , on
  , one
  , optional
  , orDefault
  , otherwise
  , over
  , over_
  , p
  , p2
  , parseAnyAroundString
  , parseAnyTill
  , parseAnyTill_
  , parseEof
  , parseFail
  , parseFailWithPosition
  , parseInt
  , parseNumber
  , parseRest
  , parseString
  , parseStringAs
  , parseStringEofAs
  , parseString_
  , parseTry
  , pass
  , pow
  , preview
  , previewOn
  , prop
  , pure
  , pureF
  , quot
  , r'ask
  , r'run
  , r'run''
  , r'view
  , r'view'b
  , rec'get
  , rec'insert
  , rec'merge
  , rec'modify
  , rec'set
  , rec'union
  , recip
  , reduce
  , reduceM
  , reflectSymbol
  , reifySymbol
  , resultVal
  , review
  , round
  , routeParse
  , routePrint
  , rtErrExtra
  , rtErrMessage
  , rtErrName
  , run
  , runAff
  , runAff_
  , runExists
  , runParser
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
  , sOrN
  , send
  , set
  , set'add
  , set'empty
  , set'fromFoldable
  , set'has
  , set'size
  , set_
  , sha256BytesOfBuffer
  , sha256OfBuffer
  , show
  , simpleHash
  , slice
  , snd
  , stext
  , stextConcat
  , stextConcatSp
  , str'endsWith
  , str'joinWith
  , str'length
  , str'split
  , str'startsWith
  , sub
  , sync'_
  , sync'x
  , toArrayOf
  , toDateTime
  , toEnum
  , toNumber
  , top
  , trunc
  , tryParseInt
  , tup'flip
  , un'
  , unit
  , unless
  , unlessM
  , unwrap
  , urlFromParts
  , urlFromString
  , urlOrigin
  , urlPathFromString
  , urlPathSegments
  , urlQuery
  , urlRelative
  , urlToString
  , var'inj
  , var'match
  , view
  , viewOn
  , void
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
  , when
  , whenJust
  , whenM
  , whenNot
  , wrap
  , wrapped'from
  , wrapped'get
  , wrapped'mk
  , wrapped'mkFor
  , wrapped'rest
  , x'act
  , x'add
  , x'addAt
  , x'alter
  , x'assign
  , x'assignAt
  , x'attemptAff
  , x'buildable'eval
  , x'clear
  , x'clearAt
  , x'cons
  , x'consAt
  , x'd1entries
  , x'd1keys
  , x'do
  , x'entries
  , x'entriesAt
  , x'eval
  , x'eval_
  , x'exec
  , x'exec_
  , x'extract
  , x'extractAt
  , x'has
  , x'hashmap
  , x'hashmap'w
  , x'hashmap2D
  , x'hashmap2D'w
  , x'hashmap2D_
  , x'hashmap2D_'w
  , x'hashmap_
  , x'hashmap_'w
  , x'hashset
  , x'hashset'w
  , x'hashset2D
  , x'hashset2D'w
  , x'hashset2D_
  , x'hashset2D_'w
  , x'hashset_
  , x'hashset_'w
  , x'info
  , x'insert
  , x'keys
  , x'keysAt
  , x'logError
  , x'logWarning
  , x'lookup
  , x'modify
  , x'now
  , x'nowMS
  , x'out
  , x'outErr
  , x'outWarn
  , x'over
  , x'over'b
  , x'permit
  , x'pop
  , x'preview
  , x'preview'b
  , x'push
  , x'ref
  , x'ref'nt
  , x'ref'nt'w
  , x'ref'w
  , x'ref_
  , x'ref_'w
  , x'remove
  , x'replace
  , x'reset
  , x'resetAt
  , x'result
  , x'run
  , x'run_
  , x'set
  , x'set'b
  , x'size
  , x'sizeAt
  , x'timeout
  , x'toArrayOf
  , x'toArrayOf'b
  , x'uncons
  , x'update
  , x'vals
  , x'valsAt
  , x'view
  , x'view'b
  , x'withReturn
  , x'withReturn''
  , zero
  , (#)
  , ($)
  , ($>)
  , (%)
  , (&&)
  , (*)
  , (*>)
  , (+)
  , (-)
  , (/)
  , (/=)
  , (/\)
  , (<)
  , (<##>)
  , (<#>)
  , (<$)
  , (<$$>)
  , (<$>)
  , (<*)
  , (<*>)
  , (<->)
  , (<:>)
  , (<<<)
  , (<=)
  , (<=<)
  , (<>)
  , (<@>)
  , (<|<)
  , (<|>)
  , (=<<)
  , (==)
  , (>)
  , (>=)
  , (>=>)
  , (>>=)
  , (>>>)
  , (>|>)
  , (||)
  , (~)
  , (~.)
  ) as ModuleReExports
import Z.Prelude as Z
import Z.XDom.Core
  ( ATTR
  , DomA
  , DomATag
  , DomE
  , DomETag
  , DomEffPermit
  , MDOMEFF
  , MDom
  , MDomEff
  , MDomEl
  , R'Self
  , Self
  , XDom
  , XDomA
  , XDomE
  , XDomEA
  , dom'a
  , dom'article
  , dom'br
  , dom'button
  , dom'div
  , dom'element
  , dom'iframe
  , dom'pre
  , dom'span
  , dom'text
  , dom'withAdapter
  , dom'withKey
  , dom'withNewState
  , domE'bind
  , domE'bind''
  , domE'fail
  , domE'fail''
  , domEff'do
  , domEff'getRunner
  , domEff'getSelf
  , domEff'on
  , domEff'on'1
  , domEff'on'1'post
  , domEff'on'1'pre
  , domEff'on'all
  , domEff'on'all'post
  , domEff'on'all'pre
  , domEff'on'post
  , domEff'on'pre
  , domR'run
  , domR'run''
  , domS'dispatch
  , domS'dispatch''
  , domS'get
  , domS'get''
  , domS'runReducer
  , domS'runReducer''
  , domS'runState
  , domS'runState''
  , domS'set
  , domS'set''
  , el'cn
  , el'cnW
  , el'href
  , el'key
  , el'onClick
  , exec'xdom
  , op'el'text
  , (%%)
  ) as ModuleReExports
import Z.XDom.Preact (ReactEl) as ModuleReExports
import Z.XDom.Router
  ( router'href
  , router'href''
  , router'routeOrE
  , router'routeOrE''
  , router'run
  , router'run''
  , router'urlState
  , router'urlState''
  ) as ModuleReExports
import Z.XDom.UrlState
  ( HrefSpec
  , RProvider
  , T
  , XProvider
  , actualHref
  , hrefFromUrl
  , mk
  , parsedHref
  , update
  ) as ModuleReExports
import Z.XDom.UrlState as UrlSt

type XurlStProviderX sx de = UrlSt.XProvider sx de

type Reducer a s =
  (Z.X'R { get :: s, update :: a -> Z.Eff'At "domEff" Z.Unit })
