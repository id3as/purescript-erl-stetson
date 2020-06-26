module Stetson.Loop ( handler
                    , init
                    , info
                    , self
                    , initResult
                    , module Exports
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Process  (Process)
import Control.Monad.State as State
import Erl.Cowboy.Req (Req)
import Foreign (Foreign)
import Stetson.Types (InitHandler, InitResult(..), WebSocketHandleHandler, StetsonHandler(..), LoopInfoHandler, LoopInitHandler, emptyHandler, LoopInternalState(..))

import Control.Monad.Trans.Class (lift) as Exports

handler :: forall msg state. InitHandler state -> StetsonHandler msg state
handler = emptyHandler 

init :: forall msg state.  LoopInitHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
init fn (StetsonHandler h) =
  StetsonHandler $ h { loopInit = Just fn  }

info :: forall msg state.  LoopInfoHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
info fn (StetsonHandler h) =
  StetsonHandler $ h { loopInfo = Just fn  }

self :: forall msg. State.StateT (Process msg) Effect (Process msg)
self = State.get

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ Loop rq st
