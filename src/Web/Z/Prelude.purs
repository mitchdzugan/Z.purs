module Web.Z.Prelude
  ( T'WebX
  , T'WebXasync
  , WebEvent
  , WebEventType
  , module DOM
  , module HTML
  , module HTMLDoc
  , module InternalT
  , module ZPrelude
  , type (@@@->)
  , type (@@@^->)
  ) where

import Web.DOM.Internal.Types (Element) as InternalT
import Web.Event.Event as WebEvent
import Web.HTML (Window) as HTML
import Web.HTML.HTMLDocument (HTMLDocument) as HTMLDoc
import Web.Z.Web.DOM
  ( class IsEventTarget
  , EventListenerOpts
  , X'Web
  , XWebR
  , evTarget
  , eventType
  , runXAThenExit
  , runXWeb
  , toEventTarget
  , xAddEventListener
  , xBody
  , xClosest
  , xDocument
  , xGetAttribute
  , xGetElementById
  , xHead
  , xLocationUrl
  , xPreventDefault
  , xPushState
  , xSetDocumentTitle
  , xStopPropagation
  , xWindow
  ) as DOM
import Z.Prelude as ZPrelude

type T'WebX x a = DOM.X'Web x ZPrelude.@@-> a

infixr 0 type T'WebX as @@@->

type T'WebXasync x a = DOM.X'Web x ZPrelude.@@^-> a

infixr 0 type T'WebXasync as @@@^->

type WebEvent = WebEvent.Event
type WebEventType = WebEvent.EventType