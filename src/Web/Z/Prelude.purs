module Web.Z.Prelude
  ( T'WebX
  , T'WebXflipped
  , WebEvent
  , WebEventType
  , module DOM
  , module HTML
  , module HTMLDoc
  , module InternalT
  , module ZPrelude
  , type (<^@)
  , type (@^>)
  ) where

import Web.DOM.Internal.Types (Element) as InternalT
import Web.Event.Event as WebEvent
import Web.HTML (Window) as HTML
import Web.HTML.HTMLDocument (HTMLDocument) as HTMLDoc
import Web.Z.Web.DOM
  ( class IsEventTarget
  , EventListenerOpts
  , X'Web
  , XEffWeb
  , XWebR
  , evTarget
  , eventType
  , runXAThenExit
  , runXWeb
  , toEventTarget
  , xAddEventListener
  , xClosest
  , xDocument
  , xGetAttribute
  , xGetElementById
  , xLocationUrl
  , xPreventDefault
  , xPushState
  , xSetDocumentTitle
  , xStopPropagation
  , xWebR
  , xWindow
  ) as DOM
import Z.Prelude as ZPrelude

type T'WebX x a = DOM.X'Web x ZPrelude.@@> a
type T'WebXflipped a x = T'WebX x a

infixr 0 type T'WebX as @^>

infixr 0 type T'WebXflipped as <^@

type WebEvent = WebEvent.Event
type WebEventType = WebEvent.EventType