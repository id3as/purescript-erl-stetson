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
import Stetson.Types (InitHandler, InitResult(..), WebSocketHandleHandler, StetsonHandlerBuilder(..), LoopInfoHandler, LoopInitHandler, emptyHandler, LoopInternalState(..))

import Control.Monad.Trans.Class (lift) as Exports

handler :: forall msg state. InitHandler state -> StetsonHandlerBuilder msg state
handler = emptyHandler 

init :: forall msg state.  LoopInitHandler msg state -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
init fn (StetsonHandlerBuilder h) =
  StetsonHandlerBuilder $ h { loopInit = Just fn  }

info :: forall msg state.  LoopInfoHandler msg state -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
info fn (StetsonHandlerBuilder h) =
  StetsonHandlerBuilder $ h { loopInfo = Just fn  }

self :: forall msg. State.StateT (Process msg) Effect (Process msg)
self = State.get

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ Loop rq st
