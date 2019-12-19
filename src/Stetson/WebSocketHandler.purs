module Stetson.WebSocketHandler where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2)

import Erl.Cowboy.Handlers.WebSocket (CallResult, FrameHandler, InfoHandler, InitHandler, WSInitHandler, decodeInFrame, hibernateResult, initResult, okResult, outFrame, replyAndHibernateResult, replyResult, stopResult)

import Stetson (InitResult(..), WebSocketCallResult(..), WebSocketHandler)
import Erl.Process.Raw (Pid, send)

foreign import self :: Effect Pid

type State msg state = 
  { handler :: WebSocketHandler msg state
  , innerState :: state 
  }

init :: forall msg state. InitHandler (WebSocketHandler msg state) (State msg state)
init = mkEffectFn2 \req handler -> do
  (InitOk req2 innerState) <- handler.init req
  pure $ initResult { handler, innerState } req

websocket_init :: forall msg state. WSInitHandler (State msg state)
websocket_init = mkEffectFn1 \state -> do
  case state of
    { innerState, handler: { wsInit: Just wsInit }} -> do
      pid <- self
      transformResult state =<< wsInit (router pid) innerState 
    _ -> 
      pure $ okResult state

router :: forall msg. Pid -> msg -> Effect Unit
router pid msg = do
  _ <- send pid msg
  pure unit

websocket_handle :: forall msg state. FrameHandler (State msg state)
websocket_handle = mkEffectFn2 \frame state ->
  case state of
    { innerState, handler: { handle: Just handle }} ->
      transformResult state =<< handle (decodeInFrame frame) innerState
    _ -> 
      pure $ okResult state

websocket_info :: forall msg state. InfoHandler msg (State msg state)
websocket_info = mkEffectFn2 \msg state ->
  case state of
    { innerState, handler: { info: Just info }} ->
      transformResult state =<< info msg innerState
    _ -> 
      pure $ okResult state

transformResult :: forall msg state. State msg state -> WebSocketCallResult state -> Effect (CallResult (State msg state))
transformResult state result =
  case result of
       NoReply innerState -> pure $ okResult $ state { innerState = innerState  }
       Hibernate innerState -> pure $ hibernateResult $ state { innerState = innerState }
       Reply frames innerState ->  pure $ replyResult  state { innerState = innerState }$ map outFrame frames
       ReplyAndHibernate frames innerState -> pure $ replyAndHibernateResult state { innerState = innerState } $ map outFrame frames
       Stop innerState -> pure $ stopResult state { innerState = innerState } 

