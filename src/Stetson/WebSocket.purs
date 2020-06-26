module Stetson.WebSocket ( handler
                         , init
                         , handle
                         , info
                         , self
                         , initResult
                         , module Exports
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Cowboy.Req (Req)
import Foreign (Foreign)
import Erl.Process (Process)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift) as Exports
import Stetson.Types (InitHandler, InitResult(..), WebSocketHandleHandler, StetsonHandlerBuilder(..), WebSocketInfoHandler, WebSocketInitHandler, emptyHandler)

handler :: forall msg state. InitHandler state -> StetsonHandlerBuilder msg state
handler i = emptyHandler i

init  :: forall msg state. WebSocketInitHandler msg state -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
init fn (StetsonHandlerBuilder h) = StetsonHandlerBuilder $ h { wsInit = Just fn }

self :: forall msg. State.StateT (Process msg) Effect (Process msg)
self = State.get

handle :: forall msg state.  WebSocketHandleHandler msg state -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
handle fn (StetsonHandlerBuilder h) =
  StetsonHandlerBuilder $ h { wsHandle = Just fn  }

info :: forall msg state.  WebSocketInfoHandler msg state -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
info fn (StetsonHandlerBuilder h) =
  StetsonHandlerBuilder $ h { wsInfo = Just fn  }

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ WebSocket rq st
