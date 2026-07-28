module Web.Z.Prelude
  ( WebXFlipped
  , module DOM
  , module ZPrelude
  , module InternalT
  , module HTML
  , module HTMLDoc
  , type (##>)
  , type (<##)
  ) where

import Z.Prelude as ZPrelude
import Web.Z.Web.DOM as DOM
import Web.DOM.Internal.Types (Element) as InternalT
import Web.HTML (Window) as HTML
import Web.HTML.HTMLDocument (HTMLDocument) as HTMLDoc

type WebXFlipped a x = DOM.XWeb x a

infixr 0 type DOM.XWeb as ##>

infixr 0 type WebXFlipped as <##