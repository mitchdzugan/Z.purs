module Z.Z.Buffer
  ( Buffer
  , ofArrayBuffer
  ) where

foreign import data Buffer :: Type

foreign import js_ofArrayBuffer :: Array Int -> Buffer

ofArrayBuffer :: Array Int -> Buffer
ofArrayBuffer = js_ofArrayBuffer