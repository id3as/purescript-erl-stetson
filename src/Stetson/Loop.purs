module Stetson.Loop ( handler
                    , init
                    , info
                    , self
                    , initResult
                    , terminate
                    , module Exports
  ) where

import Prelude

import Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Process  (Process)
import Control.Monad.State as State
import Erl.Cowboy.Req (Req)
import Stetson.Types (InitHandler, InitResult(..), LoopInfoHandler, LoopInitHandler, StetsonHandler(..), emptyHandler)

import Control.Monad.Trans.Class (lift) as Exports

handler :: forall msg state. InitHandler state -> StetsonHandler msg state
handler = emptyHandler 

init :: forall msg state.  LoopInitHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
init fn (StetsonHandler h) =
  StetsonHandler $ h { loopInit = Just fn  }

info :: forall msg state.  LoopInfoHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
info fn (StetsonHandler h) =
  StetsonHandler $ h { loopInfo = Just fn  }

-- | Add a terminate callback to the provided StetsonHandler
terminate :: forall msg state. (Foreign -> Req -> state -> Effect Unit) -> StetsonHandler msg state -> StetsonHandler msg state
terminate fn (StetsonHandler h) = (StetsonHandler $ h { terminate = Just fn })

self :: forall msg. State.StateT (Process msg) Effect (Process msg)
self = State.get

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ Loop rq st
