module Z.SSBM.Slp.Read.Impl
  ( Game
  , SlpGame(..)
  , SlpGameData
  , SlpMatch
  , SlpMeta
  , SlpParseAndShaFail(..)
  , SlpSettings
  , SlpStats
  , Stats
  , xParse
  ) where

import Z.Prelude

import Z.SSBM.Slp.Read.Error as Err

foreign import data Game :: Type
foreign import data Stats :: Type

foreign import js_gameOfBuffer :: Buffer -> Game
foreign import js_stats :: Game -> Json
foreign import js_settings :: Game -> Json
foreign import js_meta :: Game -> Json
foreign import js_startAt
  :: Maybe Int -> (Int -> Maybe Int) -> Game -> Maybe Int

type SlpMatch =
  { sessionId :: String
  , gameNumber :: Int
  , tiebreakerNumber :: Int
  }

type SlpSettings =
  { isTeams :: Boolean
  , stageId :: Int
  , timerType :: Int
  , randomSeed :: Int
  , isPAL :: Maybe Boolean
  , isFrozenPS :: Maybe Boolean
  , matchInfo :: Maybe SlpMatch
  }

type SlpMeta =
  { startAt :: Maybe String
  }

type SlpStats =
  {}

type SlpGameData =
  { rawSettings :: SlpSettings
  , rawMeta :: SlpMeta
  , rawStats :: SlpStats
  , startAtOr_ :: Maybe DateTime
  , key :: Key
  }

slpSettings :: Game -> Either JsonDecodeError SlpSettings
slpSettings g = decodeJson $ js_settings g

slpMeta :: Game -> Either JsonDecodeError SlpMeta
slpMeta g = decodeJson $ js_meta g

slpStats :: Game -> Either JsonDecodeError SlpStats
slpStats g = decodeJson $ js_stats g

data SlpGame
  = SlpGame SlpGameData
  | SlpParseFail Err.T (Array Byte)

newtype SlpParseAndShaFail = SlpParseAndShaFail JsError

derive instance Newtype SlpParseAndShaFail _

xParseData :: forall x. Buffer -> E Err.T x #> SlpGameData
xParseData buffer = do
  let game = js_gameOfBuffer buffer
  rawSettings <- e'map Err.DecodeSettings $ e'ok $ slpSettings game
  rawMeta <- e'map Err.DecodeMeta $ e'ok $ slpMeta game
  rawStats <- e'map Err.DecodeStats $ e'ok $ slpStats game
  let startAtNOr_ = js_startAt Nothing Just game
  let startAtIOr_ = startAtNOr_ <#> toNumber <#> Milliseconds >>= instant
  let startAtOr_ = startAtIOr_ <#> toDateTime
  keys <- g @XWithReturn \xReturn -> do
    whenJust startAtOr_ \startAt -> xReturn $
      [ key "@", key $ dateTimeAsMS startAt, key rawSettings.randomSeed ]
    whenJust rawSettings.matchInfo \mi -> xReturn $
      [ key "M"
      , key rawSettings.randomSeed
      , key mi.sessionId
      , key mi.gameNumber
      , key mi.tiebreakerNumber
      ]
    g @XFail Err.UnmadeId
  pure { rawSettings, rawMeta, rawStats, startAtOr_, key: key keys }

xParse :: forall x. Buffer -> EA SlpParseAndShaFail x #> SlpGame
xParse b = e'try (xParseData b) >>= case _ of
  (Right v) -> pure $ SlpGame v
  (Left e) -> e'try (sha256BytesOfBuffer b) >>= case _ of
    (Left shaE) -> g @XFail $ SlpParseAndShaFail shaE
    (Right sha256) -> pure $ SlpParseFail e sha256

instance Keyed SlpGame where
  key (SlpGame game) = game.key
  key (SlpParseFail _ bytes) = key bytes