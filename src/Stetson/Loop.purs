module Stetson.Loop
  ( initResult
  , init
  , handler
  , info
  , terminate
  , module EffectExports
  , module ProcessExports
  ) where

import Prelude
import Effect.Class (liftEffect) as EffectExports
import Erl.Process (self) as ProcessExports

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Cowboy.Req (Req)
import Foreign (Foreign)
import Stetson.Types (InitHandler, InitResult(..), LoopInfoHandler, LoopInitHandler, StetsonHandler(..), emptyHandler)

handler :: forall msg state. InitHandler state -> StetsonHandler msg state
handler = emptyHandler

init :: forall msg state. LoopInitHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
init fn (StetsonHandler h) = StetsonHandler $ h { loopInit = Just fn }

info :: forall msg state. LoopInfoHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
info fn (StetsonHandler h) = StetsonHandler $ h { loopInfo = Just fn }

-- | Add a terminate callback to the provided StetsonHandler
terminate :: forall msg state. (Foreign -> Req -> state -> Effect Unit) -> StetsonHandler msg state -> StetsonHandler msg state
terminate fn (StetsonHandler h) = (StetsonHandler $ h { terminate = Just fn })

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ Loop rq st
