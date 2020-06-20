module Stetson.WebSocket where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Cowboy.Req (Req)
import Foreign (Foreign)
import Stetson.Types (InitHandler, InitResult(..), WebSocketHandleHandler, StetsonHandlerCallbacks, InnerStetsonHandler(..), WebSocketInfoHandler, WebSocketInitHandler, emptyHandler)

handler :: forall msg state. InitHandler state -> StetsonHandlerCallbacks msg state
handler i = emptyHandler i

init  :: forall msg state. WebSocketInitHandler msg state -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
init fn h = h { wsInit = Just fn }

handle :: forall msg state.  WebSocketHandleHandler msg state -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
handle fn h =
  h { handle = Just fn  }

info :: forall msg state.  WebSocketInfoHandler msg state -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
info fn h =
  h { info = Just fn  }

externalMapping :: forall msg state. (Foreign -> Maybe msg) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
externalMapping fn h =
  h { externalMapping = Just fn }

yeeha :: forall msg state. StetsonHandlerCallbacks msg state -> InnerStetsonHandler msg state
yeeha = Complete

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ WebSocket rq st
