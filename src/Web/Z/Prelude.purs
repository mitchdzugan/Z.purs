module Web.Z.Prelude
  ( WebEvent
  , WebEventType
  , WebXFlipped
  , module DOM
  , module HTML
  , module HTMLDoc
  , module InternalT
  , module ZPrelude
  , type (##>)
  , type (<##)
  ) where

import Web.DOM.Internal.Types (Element) as InternalT
import Web.Event.Event as WebEvent
import Web.HTML (Window) as HTML
import Web.HTML.HTMLDocument (HTMLDocument) as HTMLDoc
import Web.Z.Web.DOM as DOM
import Z.Prelude as ZPrelude

type WebXFlipped a x = DOM.XWeb x a

infixr 0 type DOM.XWeb as ##>

infixr 0 type WebXFlipped as <##

type WebEvent = WebEvent.Event
type WebEventType = WebEvent.EventType