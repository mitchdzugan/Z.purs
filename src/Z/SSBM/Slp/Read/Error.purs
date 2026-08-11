module Z.SSBM.Slp.Read.Error where

import Z.Prelude

data T
  = DecodeMeta JsonDecodeError
  | DecodeStats JsonDecodeError
  | DecodeSettings JsonDecodeError
  | UnmadeId