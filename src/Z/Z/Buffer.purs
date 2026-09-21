module Z.Z.Buffer
  ( Buffer
  , ofArrayBuffer
  , sha256BytesOfBuffer
  , sha256OfBuffer
  ) where

import Prelude

import Z.Z.Core as Z
import Z.Z.Ext as E
import Z.Z.X.Index as X

foreign import data Buffer :: Type

foreign import js_ofArrayBuffer :: Array Int -> Buffer
foreign import js_sha256OfBuffer :: Buffer -> E.Effect (E.Promise String)
foreign import js_sha256ArrOfBuffer
  :: Buffer -> E.Effect (E.Promise (Array Int))

ofArrayBuffer :: Array Int -> Buffer
ofArrayBuffer = js_ofArrayBuffer

sha256OfBuffer :: forall x. Buffer -> X.EA' Z.JsError x X.@@> String
sha256OfBuffer = X.e'runEffPromise <<< js_sha256OfBuffer

sha256BytesOfBuffer
  :: forall x. Buffer -> X.EA' Z.JsError x X.@@> Array E.Byte
sha256BytesOfBuffer = (<$>) ((<$>) E.byte)
  <<< X.e'runEffPromise
  <<< js_sha256ArrOfBuffer