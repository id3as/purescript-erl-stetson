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
import Stetson.Types (InitHandler, InitResult(..), WebSocketHandleHandler, StetsonHandler(..), WebSocketInfoHandler, WebSocketInitHandler, emptyHandler)

handler :: forall msg state. InitHandler state -> StetsonHandler msg state
handler i = emptyHandler i

init  :: forall msg state. WebSocketInitHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
init fn (StetsonHandler h) = StetsonHandler $ h { wsInit = Just fn }

self :: forall msg. State.StateT (Process msg) Effect (Process msg)
self = State.get

handle :: forall msg state.  WebSocketHandleHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
handle fn (StetsonHandler h) =
  StetsonHandler $ h { wsHandle = Just fn  }

info :: forall msg state.  WebSocketInfoHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
info fn (StetsonHandler h) =
  StetsonHandler $ h { wsInfo = Just fn  }

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ WebSocket rq st
