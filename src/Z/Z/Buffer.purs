module Z.Z.Buffer
  ( Buffer
  , ofArrayBuffer
  , sha256BytesOfBuffer
  , sha256OfBuffer
  ) where

import Prelude

import Z.Z.Core as Z
import Z.Z.Defaultable as D
import Z.Z.Ext as E
import Z.Z.X as X

foreign import data Buffer :: Type

foreign import js_ofArrayBuffer :: Array Int -> Buffer
foreign import js_sha256OfBuffer :: Buffer -> E.Effect (E.Promise String)
foreign import js_sha256ArrOfBuffer
  :: Buffer -> E.Effect (E.Promise (Array Int))

ofArrayBuffer :: Array Int -> Buffer
ofArrayBuffer = js_ofArrayBuffer

sha256OfBuffer :: forall x. Buffer -> X.XRun (X.EA Z.JsError x) String
sha256OfBuffer = D.g @X.XRunEffPromise <<< js_sha256OfBuffer

sha256BytesOfBuffer
  :: forall x. Buffer -> X.XRun (X.EA Z.JsError x) (Array E.Byte)
sha256BytesOfBuffer = (<$>) ((<$>) E.byte)
  <<< D.g @X.XRunEffPromise
  <<< js_sha256ArrOfBuffer