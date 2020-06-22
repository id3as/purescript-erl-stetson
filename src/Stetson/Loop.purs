module Stetson.Loop where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Cowboy.Req (Req)
import Foreign (Foreign)
import Stetson.Types (InitHandler, InitResult(..), WebSocketHandleHandler, StetsonHandlerCallbacks, InnerStetsonHandler(..), LoopInfoHandler, LoopInitHandler, emptyHandler)

handler :: forall msg state. InitHandler state -> StetsonHandlerCallbacks msg state
handler = emptyHandler 

init :: forall msg state.  LoopInitHandler msg state -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
init fn h =
  h { loopInit = Just fn  }

info :: forall msg state.  LoopInfoHandler msg state -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
info fn h =
  h { loopInfo = Just fn  }

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ Loop rq st

yeeha :: forall msg state. StetsonHandlerCallbacks msg state -> InnerStetsonHandler msg state
yeeha = Complete
